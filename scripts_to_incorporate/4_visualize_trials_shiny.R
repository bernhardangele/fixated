library(shiny)
library(tidyverse)
library(plotly)
library(saccades)
library(crosstalk)
library(DT)

# Source functions directly to avoid any caching issues
source("functions/R/read_eyelogic_file.R")
source("functions/R/process_EK_trial.R")
source("functions/R/merge_fixations.R")
source("functions/R/assign_word_info.R")
source("functions/R/calculate_subject_EK_measures.R")
source("functions/R/calculate_fixation_time_measures.R")
source("functions/R/determine_downsampling_factor.R")

message("Loading eyelogic CSV...")
eyelogic_data <- read_eyelogic_file("subject-999.eyelogic.csv")

message("Loading external trials data for text encoding recovery...")
b1 <- read_csv("trials_b1.csv", show_col_types = FALSE)
b2 <- read_csv("trials_b2.csv", show_col_types = FALSE)
trials_db_external <- bind_rows(b1, b2)

# Join the external strings to the internal trial db
eyelogic_data$trial_db <- eyelogic_data$trial_db %>%
  left_join(trials_db_external, by = "sentence_number")

ui <- fluidPage(
  titlePanel("Eyelogic Trial Visualization"),
  sidebarLayout(
    sidebarPanel(
      numericInput("trial_idx", "Trial Number:", 1, min = 1, max = nrow(eyelogic_data$trial_db)),
      actionButton("prevBtn", "Previous"),
      actionButton("nextBtn", "Next"),
      hr(),
      checkboxInput("filter_display_on", "Remove samples before DISPLAY ON", value = TRUE),
      checkboxInput("filter_display_off", "Remove samples after DISPLAY OFF", value = TRUE),
      checkboxInput("show_fixations", "Overlay detected fixations", value = TRUE),
      checkboxInput("show_saccades", "Highlight saccades (inter-fixation paths)", value = TRUE),
      checkboxInput("show_measures", "Show FFD/GD bars above words", value = FALSE),
      checkboxInput("show_gazetarget", "Show gaze target", value = FALSE),
      checkboxInput("show_trialend", "Show trial end area", value = FALSE),
      checkboxInput("show_fixation_table", "Show fixation table", value = FALSE),
      checkboxInput("show_word_table", "Show word measures table", value = FALSE),
      checkboxInput("show_sample_table", "Show raw samples table", value = FALSE),
      hr(),
      fileInput("bg_image", "Upload Background Image (optional, PNG/JPEG)", accept = c("image/png", "image/jpeg", "image/jpg")),
      hr(),
      helpText("This tool visualizations raw samples and detected fixations overlaid on word boundaries for each trial.")
    ),
    mainPanel(
      uiOutput("warning_msg"),
      uiOutput("time_slider_ui"),
      plotlyOutput("gazePlot", height = "700px"),
      conditionalPanel(
        condition = "input.show_fixation_table",
        tags$hr(),
        tags$h4("Detected Fixations"),
        DT::DTOutput("fixation_dt")
      ),
      conditionalPanel(
        condition = "input.show_word_table",
        tags$hr(),
        tags$h4("Word Measures"),
        DT::DTOutput("word_dt")
      ),
      conditionalPanel(
        condition = "input.show_sample_table",
        tags$hr(),
        tags$h4("Raw Samples"),
        tags$div(
          style = "margin-bottom: 10px; font-weight: bold; color: #d35400;",
          textOutput("sample_totals")
        ),
        DT::DTOutput("sample_dt")
      )
    )
  )
)

server <- function(input, output, session) {
  
  observeEvent(input$prevBtn, {
    if (input$trial_idx > 1) {
      updateNumericInput(session, "trial_idx", value = input$trial_idx - 1)
    }
  })
  
  observeEvent(input$nextBtn, {
    if (input$trial_idx < nrow(eyelogic_data$trial_db)) {
      updateNumericInput(session, "trial_idx", value = input$trial_idx + 1)
    }
  })
  
  output$warning_msg <- renderUI({
    req(input$trial_idx)
    i <- input$trial_idx
    db <- eyelogic_data$trial_db[i, ]
    msgs <- list()
    
    if (!is.null(db$missing_display_on) && isTRUE(db$missing_display_on)) {
      msgs[[length(msgs) + 1]] <- div(
        style = "color: red; font-weight: bold; margin-bottom: 15px; padding: 10px; border: 1px solid red; background-color: #fee;",
        "Warning: DISPLAY ON message missing! Timeline timestamps are calculated relative to early GAZE TARGET ON fallback."
      )
    }
    
    # Check if a background image was uploaded and compare resolutions
    if (!is.null(input$bg_image)) {
        img_path <- input$bg_image$datapath
        img_name <- input$bg_image$name
        bg_dims <- NULL
        
        if (grepl("\\.png$", img_name, ignore.case = TRUE) && requireNamespace("png", quietly = TRUE)) {
          bg_img <- png::readPNG(img_path)
          bg_dims <- dim(bg_img)[1:2] # height, width
        } else if (grepl("\\.jpe?g$", img_name, ignore.case = TRUE) && requireNamespace("jpeg", quietly = TRUE)) {
          bg_img <- jpeg::readJPEG(img_path)
          bg_dims <- dim(bg_img)[1:2] # height, width
        }
        
        if (!is.null(bg_dims)) {
            # Find the display resolution in the raw CSV Header
            raw_lines <- readLines("subject-999.eyelogic.csv", n = 500)
            res_line <- grep("display resolution = ", raw_lines, value = TRUE)
            
            if (length(res_line) > 0) {
                # Format: MSG;0;display resolution = 1920x1080;;;;;;
                res_str <- sub(".*display resolution = ([0-9]+x[0-9]+).*", "\\1", res_line[1])
                res_parts <- as.numeric(unlist(strsplit(res_str, "x")))
                
                if (length(res_parts) == 2) {
                    expected_w <- res_parts[1]
                    expected_h <- res_parts[2]
                    
                    if (bg_dims[2] != expected_w || bg_dims[1] != expected_h) {
                        msgs[[length(msgs) + 1]] <- div(
                            style = "color: #856404; font-weight: bold; margin-bottom: 15px; padding: 10px; border: 1px solid #ffeeba; background-color: #fff3cd;",
                            sprintf("Warning: Uploaded image resolution (%dx%d) does not match the trial display resolution (%dx%d). Coordinates may misalign.", bg_dims[2], bg_dims[1], expected_w, expected_h)
                        )
                    }
                }
            }
        }
    }
    
    if (length(msgs) > 0) {
        do.call(tagList, msgs)
    } else {
        NULL
    }
  })
  
  output$time_slider_ui <- renderUI({
    req(input$trial_idx)
    i <- input$trial_idx
    samples <- eyelogic_data$trial_samples[[i]]
    db <- eyelogic_data$trial_db[i, ]
    
    if (input$filter_display_on && !is.na(db$timestamp_display_on)) {
      samples <- samples %>% filter(timestamp >= db$timestamp_display_on)
    }
    
    if (input$filter_display_off && !is.na(db$timestamp_display_off)) {
      samples <- samples %>% filter(timestamp <= db$timestamp_display_off)
    }
    
    # Pre-calculate relative timestamp
    t0 <- ifelse(!is.na(db$timestamp_display_on), db$timestamp_display_on, min(samples$timestamp, na.rm=TRUE))
    samples$time_rel <- samples$timestamp - t0
    
    if(nrow(samples) == 0) return(NULL)
    
    min_t <- min(samples$time_rel, na.rm = TRUE)
    max_t <- max(samples$time_rel, na.rm = TRUE)
    
    sliderInput("time_limit", "Animate Trial (Time in ms relative to DISPLAY ON):", 
                min = min_t, max = max_t, value = max_t, step = 50, 
                width = "100%",
                animate = animationOptions(interval = 300, loop = FALSE))
  })
  
  output$gazePlot <- renderPlotly({
    req(input$trial_idx)
    i <- input$trial_idx
    
    samples <- eyelogic_data$trial_samples[[i]]
    words <- eyelogic_data$trial_words[[i]]
    db <- eyelogic_data$trial_db[i, ]
    
    if (input$filter_display_on && !is.na(db$timestamp_display_on)) {
      samples <- samples %>% filter(timestamp >= db$timestamp_display_on)
    }
    
    if (input$filter_display_off && !is.na(db$timestamp_display_off)) {
      samples <- samples %>% filter(timestamp <= db$timestamp_display_off)
    }
    
    if(nrow(samples) == 0 || nrow(words) == 0) return(plotly_empty() %>% layout(title="No data for this trial"))
    
    t0 <- ifelse(!is.na(db$timestamp_display_on), db$timestamp_display_on, min(samples$timestamp, na.rm=TRUE))
    samples$time_rel <- samples$timestamp - t0
    
    fixations <- process_EK_trial(
        trial_samples = samples,
        trial_words = words,
        trial_db_for_trial = db,
        lambda = 6,
        downsampling_factor = 1,
        downsampling_method = "drop",
        use_eye = "R"
    )
    
    y_median <- median(samples$y_r, na.rm=TRUE)
    if(is.na(y_median)) y_median <- 540
    
    # Calculate text boundaries
    left_bounds <- c(125, words$word_right_x_boundary[-nrow(words)])
    words$left_boundary <- left_bounds
    words$center_x <- (words$left_boundary + words$word_right_x_boundary) / 2
    
    # Recover original words and target formatting from external db to fix Opensesame '?' encoding limits
    clean_sentence <- if(!is.na(db$cond) && db$cond == "high" && !is.na(db$sentence_high)) db$sentence_high else if (!is.na(db$cond) && db$cond == "low" && !is.na(db$sentence_low)) db$sentence_low else NA
    target_text <- if(!is.na(db$cond) && db$cond == "high" && !is.na(db$A2)) db$A2 else if (!is.na(db$cond) && db$cond == "low" && !is.na(db$A1)) db$A1 else NA
    
    if(!is.na(clean_sentence)) {
        actual_words <- str_split(clean_sentence, "\\s+")[[1]]
        for(w in 1:nrow(words)) {
            idx <- words$word_nr[w]
            if(idx > 0 && idx <= length(actual_words)) {
                words$word[w] <- actual_words[idx]
            }
        }
    }
    
    if(!is.na(target_text)) {
        words$is_target <- str_detect(words$word, fixed(target_text))
    } else {
        words$is_target <- rep(FALSE, nrow(words))
    }
    
    # Compute FFD, GD, and TVT per word from fixations
    words$ffd <- NA_real_
    words$gd <- NA_real_
    words$tvt <- NA_real_
    
    if(!is.null(fixations) && nrow(fixations) > 0) {
        # Assign word_nr to each fixation based on x position
        fix_word_nr <- sapply(fixations$x, function(fx) {
            if(fx < 125) return(-1)
            if(fx >= max(words$word_right_x_boundary)) return(-99)
            idx <- which(fx < words$word_right_x_boundary)[1]
            if(!is.na(idx)) words$word_nr[idx] else NA
        })
        fixations$word_nr_calc <- fix_word_nr
        
        # For each word, compute FFD (first fixation duration) and GD (gaze duration = first-pass sum)
        for(w in 1:nrow(words)) {
            wn <- words$word_nr[w]
            fix_on_word <- fixations %>% filter(word_nr_calc == wn)
            if(nrow(fix_on_word) > 0) {
                words$ffd[w] <- fix_on_word$duration[1]
                
                # Gaze duration: sum of consecutive fixations on this word during first pass
                # (before the eyes leave this word for the first time)
                gd_sum <- 0
                for(fi in 1:nrow(fix_on_word)) {
                    gd_sum <- gd_sum + fix_on_word$duration[fi]
                    # Check if the next fixation is on a different word (first pass ends)
                    fix_idx_in_all <- which(fixations$x == fix_on_word$x[fi] & fixations$duration == fix_on_word$duration[fi])[1]
                    if(!is.na(fix_idx_in_all) && fix_idx_in_all < nrow(fixations)) {
                        next_fix_word <- fixations$word_nr_calc[fix_idx_in_all + 1]
                        if(is.na(next_fix_word) || next_fix_word != wn) break
                    } else {
                        break
                    }
                }
                words$gd[w] <- gd_sum
                
                # TVT: sum of ALL fixation durations on this word (entire trial)
                words$tvt[w] <- sum(fix_on_word$duration)
            }
        }
    }
    
    if(!is.null(fixations) && nrow(fixations) > 0) {
      fixations$time_rel_start <- fixations$t_start - t0
    }
    
    if(!is.null(input$time_limit)) {
      t_limit <- input$time_limit
      samples <- samples %>% filter(time_rel <= t_limit)
      if(!is.null(fixations) && nrow(fixations) > 0) {
        fixations <- fixations %>% filter(time_rel_start <= t_limit)
      }
    }
    
      # Prepare highlight groups (linking samples to fixations AND saccades)
      # Fixations get "F1", "F2", etc. Saccades get "S1", "S2" for periods between them.
      if(!is.null(fixations) && nrow(fixations) > 0) {
          samples$fixation_nr <- NA_integer_
          samples$saccade_id <- NA
          
          # First mapping fixations
          for(f in 1:nrow(fixations)) {
              f_idx <- which(samples$time_rel >= fixations$time_rel_start[f] & 
                             samples$time_rel <= (fixations$time_rel_start[f] + fixations$duration[f]))
              samples$fixation_nr[f_idx] <- f
          }
          
          # Then mapping saccades (inter-fixation intervals)
          # A saccade is any continuous block of samples without a fixation_nr
          saccade_counter <- 1
          in_saccade <- FALSE
          
          for(r in 1:nrow(samples)) {
              if (is.na(samples$fixation_nr[r])) {
                  if (!in_saccade) {
                      in_saccade <- TRUE
                      curr_saccade_id <- paste0("S_id_", as.character(saccade_counter))
                      saccade_counter <- saccade_counter + 1
                  }
                  samples$saccade_id[r] <- curr_saccade_id
              } else {
                  in_saccade <- FALSE
              }
          }
      } else {
          samples$fixation_nr <- NA_integer_
          samples$saccade_id <- "S_id_1"
      }
      
      # Now map fixation_nr to the exact same fixation_id format used in the DT table
      samples$fixation_id <- ifelse(!is.na(samples$fixation_nr), paste0("F_id_", samples$fixation_nr), NA_character_)
      
      # If a fixation is selected in the table, filter the plot to ONLY show those samples
      sel_fix <- input$fixation_dt_rows_selected
      if(!is.null(sel_fix) && length(sel_fix) > 0) {
          samples <- samples %>% filter(fixation_nr %in% sel_fix)
      }
      
      # Use SharedData for plotly highlighting
      library(crosstalk)
      sd_group_name <- paste0("eyetracking_trial_", i)
      sd_sac_group <- paste0("saccade_trial_", i)
      sd_word_group <- paste0("word_trial_", i)
      
      # Sample-level crosstalk
      # Link fixation samples to the main fixation table group using fixation_id
      sd_samples_fix <- SharedData$new(samples %>% drop_na(x_r, y_r) %>% filter(!is.na(fixation_id)), key = ~fixation_id, group = sd_group_name)
      sd_samples_sac <- SharedData$new(samples %>% drop_na(x_r, y_r) %>% filter(!is.na(saccade_id)), key = ~saccade_id, group = sd_sac_group)
      
      # Word-level crosstalk group
      words$word_nr_key <- as.character(words$word_nr)
      sd_words <- SharedData$new(words, key = ~word_nr_key, group = sd_word_group)
      
      # Handle background image if uploaded
      bg_img_base64 <- NULL
      if (!is.null(input$bg_image)) {
        img_path <- input$bg_image$datapath
        img_name <- input$bg_image$name
        if (grepl("\\.(png|jpe?g)$", img_name, ignore.case = TRUE) && requireNamespace("base64enc", quietly = TRUE)) {
          raw_img <- readBin(img_path, "raw", file.info(img_path)$size)
          mime_type <- ifelse(grepl("\\.png$", img_name, ignore.case = TRUE), "image/png", "image/jpeg")
          bg_img_base64 <- paste0("data:", mime_type, ";base64,", base64enc::base64encode(raw_img))
        }
      }
      
      p <- suppressWarnings({
        ggplot() + 
        # Gaze target (50x50 filled square; OpenSesame center-based coords → screen coords)
        # x_center = 125, y_center = screen_height/2; assuming 1080p: y_center = 540
        (if(input$show_gazetarget) {
            gt_x_center <- 125
            gt_y_center <- 540  # var.height / 2 for 1080p
            gt_half <- 25       # gazetarget_width / 2 = gazetarget_height / 2
            list(
                geom_rect(aes(xmin = gt_x_center - gt_half, xmax = gt_x_center + gt_half,
                              ymin = gt_y_center - gt_half, ymax = gt_y_center + gt_half),
                          fill = "black", alpha = 0.8, color = "black", linewidth = 0.5),
                annotate("text", x = gt_x_center, y = gt_y_center + gt_half + 12, 
                         label = "Gaze Target", size = 2.5, color = "black")
            )
        } else NULL) +
        # Trial end area (lower-right corner: x >= 1728, y >= 972 on 1920x1080)
        (if(input$show_trialend) {
            te_xmin <- 1920 * 0.9  # 1728
            te_ymin <- 1080 * 0.9  # 972
            list(
                geom_rect(aes(xmin = te_xmin, xmax = 1920, ymin = te_ymin, ymax = 1080),
                          fill = "darkgray", alpha = 0.3, color = "gray40", linewidth = 0.5, linetype = "dashed"),
                annotate("text", x = (te_xmin + 1920) / 2, y = (te_ymin + 1080) / 2,
                         label = "Trial End\nArea", size = 2.5, color = "gray30")
            )
        } else NULL) +
        # Word regions - backed by SharedData for crosstalk highlighting
        # We use geom_segment instead of geom_rect because plotly crosstalk highlights segments much more reliably than rects/polygons
        geom_segment(data = sd_words, aes(x = left_boundary, xend = word_right_x_boundary, 
                                          y = y_median, yend = y_median,
                                          group = word_nr_key,
                                          text = paste("Word:", word, "\nNr:", word_nr, 
                                                       ifelse(is_target, "\n(Target Word)", ""),
                                                       ifelse(!is.na(ffd), paste0("\nFFD: ", round(ffd), " ms"), "\nFFD: —"),
                                                       ifelse(!is.na(gd), paste0("\nGD: ", round(gd), " ms"), "\nGD: —"),
                                                       ifelse(!is.na(tvt), paste0("\nTVT: ", round(tvt), " ms"), "\nTVT: —"))),
                     color = ifelse(words$is_target, "lightpink", "lightblue"), 
                     alpha = 0.3, linewidth = 25) + # linewidth ~80px box height to match y_median +/- 40
        # Invisible hover targets for word tooltips (segment tooltips are hard to trigger in plotly)
        geom_point(data = sd_words, aes(x = center_x, y = y_median, 
                                        text = paste("Word:", word, "\nNr:", word_nr, 
                                                     ifelse(is_target, "\n(Target Word)", ""),
                                                     ifelse(!is.na(ffd), paste0("\nFFD: ", round(ffd), " ms"), "\nFFD: —"),
                                                     ifelse(!is.na(gd), paste0("\nGD: ", round(gd), " ms"), "\nGD: —"),
                                                     ifelse(!is.na(tvt), paste0("\nTVT: ", round(tvt), " ms"), "\nTVT: —"))),
                   alpha = 0, size = 15) +
        # Add a thin outline around the "segment box"
        geom_segment(data = sd_words, aes(x = left_boundary, xend = word_right_x_boundary, 
                                          y = y_median - 40, yend = y_median - 40, group = word_nr_key),
                     color = ifelse(words$is_target, "red", "blue"), alpha = 0.5, linewidth = 0.5) +
        geom_segment(data = sd_words, aes(x = left_boundary, xend = word_right_x_boundary, 
                                          y = y_median + 40, yend = y_median + 40, group = word_nr_key),
                     color = ifelse(words$is_target, "red", "blue"), alpha = 0.5, linewidth = 0.5) +
        geom_segment(data = sd_words, aes(x = left_boundary, xend = left_boundary, 
                                          y = y_median - 40, yend = y_median + 40, group = word_nr_key),
                     color = ifelse(words$is_target, "red", "blue"), alpha = 0.5, linewidth = 0.5) +
        geom_segment(data = sd_words, aes(x = word_right_x_boundary, xend = word_right_x_boundary, 
                                          y = y_median - 40, yend = y_median + 40, group = word_nr_key),
                     color = ifelse(words$is_target, "red", "blue"), alpha = 0.5, linewidth = 0.5) +
        # Removed clickable word markers as requested
        geom_text(data = words, aes(x = center_x, y = y_median + 55, label = word), vjust = 0, size = 4) +
        
        # FFD/GD bar graph overlays (drawn above word boxes)
        # In the reversed Y axis, "above" = smaller Y values
        # Bars scale: 1 ms = 0.5 px height
        (if(input$show_measures) {
            bar_scale <- 0.5  # pixels per ms
            bar_words <- words %>% filter(!is.na(ffd) | !is.na(gd) | !is.na(tvt))
            bar_width <- (bar_words$word_right_x_boundary - bar_words$left_boundary) * 0.25
            bar_gap <- bar_width * 0.08
            
            bar_words_ffd <- bar_words %>% filter(!is.na(ffd))
            bar_words_gd <- bar_words %>% filter(!is.na(gd))
            bar_words_tvt <- bar_words %>% filter(!is.na(tvt))
            
            list(
                # FFD bars (green, left)
                if(nrow(bar_words_ffd) > 0) geom_rect(
                    data = bar_words_ffd,
                    aes(xmin = left_boundary + bar_gap, 
                        xmax = left_boundary + bar_width,
                        ymin = y_median - 40 - ffd * bar_scale, 
                        ymax = y_median - 40,
                        text = paste("FFD:", round(ffd), "ms")),
                    fill = "#2ecc71", alpha = 0.6, color = "#27ae60", linewidth = 0.3
                ) else NULL,
                if(nrow(bar_words_ffd) > 0) geom_text(
                    data = bar_words_ffd,
                    aes(x = left_boundary + bar_width / 2, 
                        y = y_median - 40 - ffd * bar_scale - 8,
                        label = round(ffd)),
                    size = 2.5, color = "#27ae60"
                ) else NULL,
                
                # GD bars (purple, middle)
                if(nrow(bar_words_gd) > 0) geom_rect(
                    data = bar_words_gd,
                    aes(xmin = left_boundary + bar_width + bar_gap * 2,
                        xmax = left_boundary + bar_width * 2 + bar_gap,
                        ymin = y_median - 40 - gd * bar_scale, 
                        ymax = y_median - 40,
                        text = paste("GD:", round(gd), "ms")),
                    fill = "#9b59b6", alpha = 0.6, color = "#8e44ad", linewidth = 0.3
                ) else NULL,
                if(nrow(bar_words_gd) > 0) geom_text(
                    data = bar_words_gd,
                    aes(x = left_boundary + bar_width * 1.5 + bar_gap * 1.5,
                        y = y_median - 40 - gd * bar_scale - 8,
                        label = round(gd)),
                    size = 2.5, color = "#8e44ad"
                ) else NULL,
                
                # TVT bars (blue, right)
                if(nrow(bar_words_tvt) > 0) geom_rect(
                    data = bar_words_tvt,
                    aes(xmin = left_boundary + bar_width * 2 + bar_gap * 3,
                        xmax = left_boundary + bar_width * 3 + bar_gap * 2,
                        ymin = y_median - 40 - tvt * bar_scale, 
                        ymax = y_median - 40,
                        text = paste("TVT:", round(tvt), "ms")),
                    fill = "#3498db", alpha = 0.6, color = "#2980b9", linewidth = 0.3
                ) else NULL,
                if(nrow(bar_words_tvt) > 0) geom_text(
                    data = bar_words_tvt,
                    aes(x = left_boundary + bar_width * 2.5 + bar_gap * 2.5,
                        y = y_median - 40 - tvt * bar_scale - 8,
                        label = round(tvt)),
                    size = 2.5, color = "#2980b9"
                ) else NULL
            )
        } else NULL) +
        
        # Saccade line overlays (highlightable paths)
        # We only draw this if show_saccades is checked
        (if(input$show_saccades) geom_path(data = sd_samples_sac, aes(x = x_r, y = y_r, group=saccade_id), color = "orange", alpha = 0.5, linewidth=0.8) else NULL) +
        
        # Raw Samples Background
        geom_path(data = samples %>% drop_na(x_r, y_r), aes(x = x_r, y = y_r), color = "gray", alpha = 0.4) +
        
        # Point highlights (Split by Fix vs Sac)
        geom_point(data = sd_samples_fix, aes(x = x_r, y = y_r, color = time_rel, 
                                          text=paste("t:", round(time_rel), "ms\nX:", round(x_r), "\nY:", round(y_r))), 
                   size = 1.0, alpha = 0.7) +
        geom_point(data = sd_samples_sac, aes(x = x_r, y = y_r, color = time_rel, 
                                          text=paste("Saccade:", saccade_id, "\nt:", round(time_rel), "ms\nX:", round(x_r), "\nY:", round(y_r))), 
                   size = 1.0, alpha = 0.7) +
        scale_color_viridis_c(name = "Time (ms)", option = "plasma")
      })
      
      if (input$show_fixations && !is.null(fixations) && nrow(fixations) > 0) {
        fixations$fixation_id <- paste0("F_id_", as.character(1:nrow(fixations)))
        fixations$word_nr_key <- as.character(fixations$word_nr)
        sd_fixations <- SharedData$new(fixations, key = ~fixation_id, group = sd_group_name)
        # Invisible layer tied to word selection group
        sd_fixations_by_word <- SharedData$new(fixations, key = ~word_nr_key, group = sd_word_group)
        
        p <- suppressWarnings({ p + 
          # Fixations
          geom_path(data = sd_fixations, aes(x = x, y = y), color = "red", linetype = "dashed", alpha = 0.6) +
          geom_point(data = sd_fixations, aes(x = x, y = y, size = duration, 
                                           text=paste("Dur:", round(duration), "ms\nWord nr:", word_nr, "\nWord:", word, 
                                                      "\nStart:", round(time_rel_start), "ms\nCenter X:", round(x), "\nCenter Y:", round(y))), 
                     color = "red", alpha = 0.7) +
          geom_text(data = sd_fixations, aes(x = x, y = y, label = seq_along(x)), color="white", size=2.5) +
          # Invisible overlay that pops up when a word is selected
          geom_point(data = sd_fixations_by_word, aes(x = x, y = y), size = 4, color = "orange", alpha = 0)
        })
      }
      
      x_max <- max(words$word_right_x_boundary, na.rm=T) + 200
      y_max <- max(y_median + 400, 1080)
      
      # Calculate y_min to accommodate very tall measure bars in reversed y-axis
      y_min <- 0
      if(input$show_measures && nrow(words) > 0) {
          max_tvt <- max(words$tvt, na.rm = TRUE)
          if(!is.infinite(max_tvt) && !is.na(max_tvt)) {
              # bar height = tvt * 0.5. Top of bar = y_median - 40 - height
              highest_bar_y <- y_median - 40 - (max_tvt * 0.5)
              # If the bar shoots up past y=0 into negative coordinates, expand the plot upwards
              if(highest_bar_y < 0) {
                  y_min <- highest_bar_y - 100
              }
          }
      }
      
      if(input$show_gazetarget || input$show_trialend) {
          x_max <- max(x_max, 1920)
          y_max <- max(y_max, 1080)
      }
      p <- p + scale_y_continuous(limits = c(y_min, y_max)) + 
        scale_x_continuous(limits = c(0, x_max)) +
        theme_minimal() +
        labs(title = paste("Trial", i, "- Sentence", db$sentence_number), x = "Screen X (px)", y = "Screen Y (px)")
      
      suppressWarnings({
        plt <- ggplotly(p, tooltip = "text", source = "gazePlot") %>% 
          layout(yaxis = list(autorange = "reversed"))
          
        if(!is.null(bg_img_base64)) {
            plt <- plt %>% layout(
                images = list(
                    list(
                        source = bg_img_base64,
                        xref = "x",
                        yref = "y",
                        xanchor = "left",
                        yanchor = "top",
                        x = 0,
                        y = 0,
                        sizex = 1920,
                        sizey = 1080,
                        sizing = "stretch",
                        layer = "below"
                    )
                )
            )
        }
        
        plt %>%
          config(displayModeBar = FALSE) %>%
          highlight(on = "plotly_click", off = "plotly_doubleclick", color = "orange", opacityDim = 0.2, selected = attrs_selected(opacity = 1))
      })
  })
  
  output$fixation_dt <- DT::renderDT(server = FALSE, {
    req(input$show_fixation_table)
    req(input$trial_idx)
    i <- input$trial_idx
    samples <- eyelogic_data$trial_samples[[i]]
    words <- eyelogic_data$trial_words[[i]]
    db <- eyelogic_data$trial_db[i, ]
    
    if (input$filter_display_on && !is.na(db$timestamp_display_on)) {
      samples <- samples %>% filter(timestamp >= db$timestamp_display_on)
    }
    if (input$filter_display_off && !is.na(db$timestamp_display_off)) {
      samples <- samples %>% filter(timestamp <= db$timestamp_display_off)
    }
    if(nrow(samples) == 0 || nrow(words) == 0) return(NULL)
    
    t0 <- ifelse(!is.na(db$timestamp_display_on), db$timestamp_display_on, min(samples$timestamp, na.rm=TRUE))
    samples$time_rel <- samples$timestamp - t0
    
    fixations <- process_EK_trial(
      trial_samples = samples, trial_words = words, trial_db_for_trial = db,
      lambda = 6, downsampling_factor = 1, downsampling_method = "drop", use_eye = "R"
    )
    
    if(is.null(fixations) || nrow(fixations) == 0) return(NULL)
    
    fixations$time_rel_start <- fixations$t_start - t0
    fixations$time_rel_end <- fixations$time_rel_start + fixations$duration
    
    # Recover correct word text from external trial data (fix OpenSesame '?' encoding)
    clean_sentence <- if(!is.na(db$cond) && db$cond == "high" && !is.na(db$sentence_high)) db$sentence_high else if (!is.na(db$cond) && db$cond == "low" && !is.na(db$sentence_low)) db$sentence_low else NA
    if(!is.na(clean_sentence)) {
        actual_words <- str_split(clean_sentence, "\\s+")[[1]]
        for(fi in 1:nrow(fixations)) {
            wn <- fixations$word_nr[fi]
            if(!is.na(wn) && wn > 0 && wn <= length(actual_words)) {
                fixations$word[fi] <- actual_words[wn]
            }
        }
    }
    
    # Compute pass number per word
    fixations$pass <- NA_integer_
    word_visit_count <- list()
    prev_word <- NA
    for(fi in 1:nrow(fixations)) {
        wn <- as.character(fixations$word_nr[fi])
        if(is.na(wn) || wn == "-1" || wn == "-99") {
            fixations$pass[fi] <- NA
            prev_word <- NA
        } else {
            if(is.na(prev_word) || prev_word != wn) {
                word_visit_count[[wn]] <- (word_visit_count[[wn]] %||% 0L) + 1L
            }
            fixations$pass[fi] <- word_visit_count[[wn]]
            prev_word <- wn
        }
    }
    
    # Use fixation_id format
    fixations$fixation_id <- paste0("F_id_", as.character(1:nrow(fixations)))
    
    fix_table <- fixations %>%
      mutate(fixation_nr = row_number()) %>%
      select(fixation_id, fixation_nr, x = x, y = y, duration, word_nr, word, 
             start_ms = time_rel_start, end_ms = time_rel_end, pass) %>%
      mutate(across(c(x, y, duration, start_ms, end_ms), ~ round(., 1)))
    
    sd_group_name <- paste0("eyetracking_trial_", i)
    sd_fix_table <- SharedData$new(fix_table, key = ~fixation_id, group = sd_group_name)
    
    DT::datatable(
      sd_fix_table,
      selection = "multiple", # Allow multiple fixations to be selected when a word is clicked
      rownames = FALSE,
      options = list(
        pageLength = 20,
        scrollX = TRUE,
        columnDefs = list(list(visible = FALSE, targets = 0))  # hide fixation_id column
      ),
      class = "compact stripe hover"
    )
  })
  
  output$word_dt <- DT::renderDT(server = FALSE, {
    req(input$show_word_table)
    req(input$trial_idx)
    i <- input$trial_idx
    samples <- eyelogic_data$trial_samples[[i]]
    words <- eyelogic_data$trial_words[[i]]
    db <- eyelogic_data$trial_db[i, ]
    
    if (input$filter_display_on && !is.na(db$timestamp_display_on)) {
      samples <- samples %>% filter(timestamp >= db$timestamp_display_on)
    }
    if (input$filter_display_off && !is.na(db$timestamp_display_off)) {
      samples <- samples %>% filter(timestamp <= db$timestamp_display_off)
    }
    if(nrow(samples) == 0 || nrow(words) == 0) return(NULL)
    
    t0 <- ifelse(!is.na(db$timestamp_display_on), db$timestamp_display_on, min(samples$timestamp, na.rm=TRUE))
    samples$time_rel <- samples$timestamp - t0
    
    fixations <- process_EK_trial(
      trial_samples = samples, trial_words = words, trial_db_for_trial = db,
      lambda = 6, downsampling_factor = 1, downsampling_method = "drop", use_eye = "R"
    )
    
    # Calculate word boundaries
    left_bounds <- c(125, words$word_right_x_boundary[-nrow(words)])
    words$left_boundary <- left_bounds
    
    # Recover correct word text
    clean_sentence <- if(!is.na(db$cond) && db$cond == "high" && !is.na(db$sentence_high)) db$sentence_high else if (!is.na(db$cond) && db$cond == "low" && !is.na(db$sentence_low)) db$sentence_low else NA
    target_text <- if(!is.na(db$cond) && db$cond == "high" && !is.na(db$A2)) db$A2 else if (!is.na(db$cond) && db$cond == "low" && !is.na(db$A1)) db$A1 else NA
    
    if(!is.na(clean_sentence)) {
        actual_words <- str_split(clean_sentence, "\\s+")[[1]]
        for(w in 1:nrow(words)) {
            idx <- words$word_nr[w]
            if(idx > 0 && idx <= length(actual_words)) words$word[w] <- actual_words[idx]
        }
    }
    
    words$is_target <- if(!is.na(target_text)) str_detect(words$word, fixed(target_text)) else rep(FALSE, nrow(words))
    
    # Compute FFD, GD, TVT
    words$ffd <- NA_real_
    words$gd <- NA_real_
    words$tvt <- NA_real_
    
    if(!is.null(fixations) && nrow(fixations) > 0) {
        fix_word_nr <- sapply(fixations$x, function(fx) {
            if(fx < 125) return(-1)
            if(fx >= max(words$word_right_x_boundary)) return(-99)
            idx <- which(fx < words$word_right_x_boundary)[1]
            if(!is.na(idx)) words$word_nr[idx] else NA
        })
        fixations$word_nr_calc <- fix_word_nr
        
        for(w in 1:nrow(words)) {
            wn <- words$word_nr[w]
            fix_on_word <- fixations %>% filter(word_nr_calc == wn)
            if(nrow(fix_on_word) > 0) {
                words$ffd[w] <- fix_on_word$duration[1]
                gd_sum <- 0
                for(fi in 1:nrow(fix_on_word)) {
                    gd_sum <- gd_sum + fix_on_word$duration[fi]
                    fix_idx_in_all <- which(fixations$x == fix_on_word$x[fi] & fixations$duration == fix_on_word$duration[fi])[1]
                    if(!is.na(fix_idx_in_all) && fix_idx_in_all < nrow(fixations)) {
                        next_fix_word <- fixations$word_nr_calc[fix_idx_in_all + 1]
                        if(is.na(next_fix_word) || next_fix_word != wn) break
                    } else break
                }
                words$gd[w] <- gd_sum
                words$tvt[w] <- sum(fix_on_word$duration)
            }
        }
    }
    
    word_table <- words %>%
      mutate(word_nr_key = as.character(word_nr)) %>%
      select(word_nr_key, word_nr, word, is_target, ffd, gd, tvt) %>%
      mutate(across(c(ffd, gd, tvt), ~ round(., 1)))
    
    sd_word_group <- paste0("word_trial_", i)
    sd_word_table <- SharedData$new(word_table, key = ~word_nr_key, group = sd_word_group)
    
    DT::datatable(
      sd_word_table,
      selection = "single",
      rownames = FALSE,
      options = list(
        pageLength = 25, 
        scrollX = TRUE,
        columnDefs = list(list(visible = FALSE, targets = 0))  # hide word_nr_key
      ),
      class = "compact stripe hover"
    )
  })
  
  output$sample_dt <- DT::renderDT(server = FALSE, {
    req(input$show_sample_table)
    req(input$trial_idx)
    i <- input$trial_idx
    samples <- eyelogic_data$trial_samples[[i]]
    db <- eyelogic_data$trial_db[i, ]
    words <- eyelogic_data$trial_words[[i]]
    
    if (input$filter_display_on && !is.na(db$timestamp_display_on)) {
      samples <- samples %>% filter(timestamp >= db$timestamp_display_on)
    }
    if (input$filter_display_off && !is.na(db$timestamp_display_off)) {
      samples <- samples %>% filter(timestamp <= db$timestamp_display_off)
    }
    if(nrow(samples) == 0) return(NULL)
    
    t0 <- ifelse(!is.na(db$timestamp_display_on), db$timestamp_display_on, min(samples$timestamp, na.rm=TRUE))
    samples$time_rel <- samples$timestamp - t0
    samples$duration <- c(diff(samples$time_rel), 0)
    
    fixations <- process_EK_trial(
      trial_samples = samples, trial_words = words, trial_db_for_trial = db,
      lambda = 6, downsampling_factor = 1, downsampling_method = "drop", use_eye = "R"
    )
    
    samples$fixation_nr <- NA_integer_
    if(!is.null(fixations) && nrow(fixations) > 0) {
        fixations$time_rel_start <- fixations$t_start - t0
        for(f in 1:nrow(fixations)) {
            f_idx <- which(samples$time_rel >= fixations$time_rel_start[f] & 
                           samples$time_rel <= (fixations$time_rel_start[f] + fixations$duration[f]))
            samples$fixation_nr[f_idx] <- f
        }
    }
    
    sample_table <- samples %>%
        mutate(sample_nr = row_number()) %>%
        select(sample_nr, time_rel, x_r, y_r, duration, fixation_nr) %>%
        mutate(across(c(time_rel, x_r, y_r, duration), ~ round(., 1)))
        
    sel_fix <- input$fixation_dt_rows_selected
    if(!is.null(sel_fix) && length(sel_fix) > 0) {
        sample_table <- sample_table %>% filter(fixation_nr %in% sel_fix)
    }
        
    DT::datatable(
        sample_table,
        selection = "multiple",
        rownames = FALSE,
        options = list(pageLength = 25, scrollX = TRUE),
        class = "compact stripe hover"
    )
  })
  
  output$sample_totals <- renderText({
      req(input$trial_idx)
      i <- input$trial_idx
      samples <- eyelogic_data$trial_samples[[i]]
      db <- eyelogic_data$trial_db[i, ]
      words <- eyelogic_data$trial_words[[i]]
      
      if (input$filter_display_on && !is.na(db$timestamp_display_on)) {
        samples <- samples %>% filter(timestamp >= db$timestamp_display_on)
      }
      if (input$filter_display_off && !is.na(db$timestamp_display_off)) {
        samples <- samples %>% filter(timestamp <= db$timestamp_display_off)
      }
      if(nrow(samples) == 0) return("Duration: 0 ms")
      
      t0 <- ifelse(!is.na(db$timestamp_display_on), db$timestamp_display_on, min(samples$timestamp, na.rm=TRUE))
      samples$time_rel <- samples$timestamp - t0
      samples$duration <- c(diff(samples$time_rel), 0)
      
      fixations <- process_EK_trial(
          trial_samples = samples, trial_words = words, trial_db_for_trial = db,
          lambda = 6, downsampling_factor = 1, downsampling_method = "drop", use_eye = "R"
      )
      
      samples$fixation_nr <- NA_integer_
      if(!is.null(fixations) && nrow(fixations) > 0) {
          fixations$time_rel_start <- fixations$t_start - t0
          for(f in 1:nrow(fixations)) {
              f_idx <- which(samples$time_rel >= fixations$time_rel_start[f] & 
                             samples$time_rel <= (fixations$time_rel_start[f] + fixations$duration[f]))
              samples$fixation_nr[f_idx] <- f
          }
      }
      
      sel_fix <- input$fixation_dt_rows_selected
      filtered_samples <- samples
      if(!is.null(sel_fix) && length(sel_fix) > 0) {
          filtered_samples <- filtered_samples %>% filter(fixation_nr %in% sel_fix)
      }
      
      sel_samp <- input$sample_dt_rows_selected
      if(length(sel_samp) > 0) {
          tot <- sum(filtered_samples$duration[sel_samp], na.rm = TRUE)
          paste("Selected Samples Duration:", round(tot, 1), "ms")
      } else {
          if(!is.null(sel_fix) && length(sel_fix) > 0) {
              tot <- sum(filtered_samples$duration, na.rm = TRUE)
              paste("All Visible Samples Total Duration:", round(tot, 1), "ms  (Select specific rows to sum only those)")
          } else {
              "Total Duration: 0 ms (Select a fixation or sample rows)"
          }
      }
  })
  
  # Server-side linking: Word table -> Fixation table
  observeEvent(input$word_dt_rows_selected, {
      req(input$trial_idx)
      i <- input$trial_idx
      words <- eyelogic_data$trial_words[[i]]
      db <- eyelogic_data$trial_db[i, ]
      
      # Determine which word_nr was clicked
      if(length(input$word_dt_rows_selected) > 0) {
          # The row index in DT corresponds to the row in word_table
          # Usually DT row indices are 1-based, we can just use words$word_nr[row]
          selected_word_nrs <- words$word_nr[input$word_dt_rows_selected]
          
          # Now find the fixations that have this word_nr
          samples <- eyelogic_data$trial_samples[[i]]
          fixations <- process_EK_trial(
              trial_samples = samples, trial_words = words, trial_db_for_trial = db,
              lambda = 6, downsampling_factor = 1, downsampling_method = "drop", use_eye = "R"
          )
          
          if(!is.null(fixations) && nrow(fixations) > 0) {
              # Replicate calculation of word_nr_calc for fixations
              fix_word_nr <- sapply(fixations$x, function(fx) {
                  if(fx < 125) return(-1)
                  if(fx >= max(words$word_right_x_boundary)) return(-99)
                  idx <- which(fx < words$word_right_x_boundary)[1]
                  if(!is.na(idx)) words$word_nr[idx] else NA
              })
              
              matching_fix_rows <- which(fix_word_nr %in% selected_word_nrs)
              if(length(matching_fix_rows) > 0) {
                  DT::dataTableProxy("fixation_dt") %>% DT::selectRows(matching_fix_rows)
              } else {
                  DT::dataTableProxy("fixation_dt") %>% DT::selectRows(NULL)
              }
          }
      } else {
          DT::dataTableProxy("fixation_dt") %>% DT::selectRows(NULL)
      }
  }, ignoreNULL = FALSE, ignoreInit = TRUE)
  
  # Server-side linking: Fixation table -> Word table AND Sample table
  observeEvent(input$fixation_dt_rows_selected, {
      req(input$trial_idx)
      i <- input$trial_idx
      samples <- eyelogic_data$trial_samples[[i]]
      words <- eyelogic_data$trial_words[[i]]
      db <- eyelogic_data$trial_db[i, ]
      
      if(length(input$fixation_dt_rows_selected) > 0) {
          # Apply standard filtering to match sample_dt
          if (input$filter_display_on && !is.na(db$timestamp_display_on)) {
            samples <- samples %>% filter(timestamp >= db$timestamp_display_on)
          }
          if (input$filter_display_off && !is.na(db$timestamp_display_off)) {
            samples <- samples %>% filter(timestamp <= db$timestamp_display_off)
          }
          t0 <- ifelse(!is.na(db$timestamp_display_on), db$timestamp_display_on, min(samples$timestamp, na.rm=TRUE))
          samples$time_rel <- samples$timestamp - t0
          
          fixations <- process_EK_trial(
              trial_samples = samples, trial_words = words, trial_db_for_trial = db,
              lambda = 6, downsampling_factor = 1, downsampling_method = "drop", use_eye = "R"
          )
          
          if(!is.null(fixations) && nrow(fixations) > 0) {
              # Get the fixations selected
              selected_fixes <- fixations[input$fixation_dt_rows_selected, ]
              fixations$time_rel_start <- fixations$t_start - t0
              selected_fixes$time_rel_start <- selected_fixes$t_start - t0
              
              # 1. Sync Word Table
              fix_word_nr <- sapply(selected_fixes$x, function(fx) {
                  if(fx < 125) return(-1)
                  if(fx >= max(words$word_right_x_boundary)) return(-99)
                  idx <- which(fx < words$word_right_x_boundary)[1]
                  if(!is.na(idx)) words$word_nr[idx] else NA
              })
              
              matching_word_rows <- which(words$word_nr %in% fix_word_nr)
              if(length(matching_word_rows) > 0) {
                  current_word_sel <- isolate(input$word_dt_rows_selected)
                  if(length(current_word_sel) != length(matching_word_rows) || !all(current_word_sel %in% matching_word_rows)) {
                      DT::dataTableProxy("word_dt") %>% DT::selectRows(matching_word_rows)
                  }
              }
              
                  # Obsolete proxy sync removed since sample_dt now filters itself
          }
      } else {
          current_word_sel <- isolate(input$word_dt_rows_selected)
          if(length(current_word_sel) > 0) {
              DT::dataTableProxy("word_dt") %>% DT::selectRows(NULL)
          }
      }
  }, ignoreNULL = FALSE, ignoreInit = TRUE)
  
  # Server-side linking: Plot Click -> Fixation table
  observeEvent(event_data("plotly_click", source = "gazePlot"), {
      d <- event_data("plotly_click", source = "gazePlot")
      if (is.null(d)) return()
      
      # The plotly_click event returns the 'key' argument passed to SharedData
      if ("key" %in% names(d)) {
          click_key <- d$key[1]
          
          # If the user clicked a fixation or a sample belonging to one, the key is F_id_X
          if (!is.na(click_key) && startsWith(as.character(click_key), "F_id_")) {
              # Extract the row number
              fix_num <- as.numeric(gsub("F_id_", "", click_key))
              if (!is.na(fix_num)) {
                  DT::dataTableProxy("fixation_dt") %>% DT::selectRows(fix_num)
              }
          }
      }
  })
  
}

shinyApp(ui = ui, server = server)
