#' @title Launch an interactive Shiny app to visualise eye-tracking trials
#'
#' @description
#' Opens a \pkg{shiny} application that lets you browse every trial in an
#' experiment, toggling overlays for raw gaze samples, EyeLink-detected
#' fixations, and word regions of interest.  The gaze plot is rendered with
#' \pkg{plotly} so you can hover over individual samples and fixations to see
#' detailed tooltips, and zoom / pan the view.
#'
#' @param asc_result A named list as returned by \code{\link{read_asc}},
#'   containing at minimum `samples` and `events`.  If you prefer not to use `read_asc`,
#'   you may set this to `NULL` and provide the other data frames individually.
#' @param samples A data frame of raw gaze samples.
#' @param fixations A data frame of detected fixations.
#' @param rois A data frame of word regions of interest. Takes precedence over
#'   `asc_result$word_boundaries`.
#' @param measures A data frame of word measures. If provided, the app will not
#'   recompute measures internally.
#' @param roi A data frame of word regions of interest as returned by
#'   \code{\link{read_roi}}. Alias for `rois`.
#' @param trial_db A data frame containing trial metadata (e.g. `t_display_on`).
#' @param launch.browser Logical.  Passed to
#'   \code{\link[shiny]{shinyApp}}'s `options` list as the `launch.browser`
#'   element.  Defaults to `TRUE`.
#'
#' @return Invisibly, the \pkg{shiny} app object (returned by
#'   \code{\link[shiny]{shinyApp}}).  The app is also launched immediately
#'   (unless `launch.browser = FALSE`).
#'
#' @details
#' ## Controls
#' \describe{
#'   \item{Subject}{When a `subject` (or `subject_nr`) column is present in the
#'     provided data, choose which participant to inspect.}
#'   \item{Trial Number}{Select any trial by its `trial_nr` (or sequential
#'     index when no `trial_nr` column is present). Use the
#'     **Previous** / **Next** buttons to step through trials one by one.}
#'   \item{Sentence Number}{Select any trial by its `sentence_nr` if present in the data.}
#'   \item{Eye}{Choose which eye's data to display (`L` for left, `R` for
#'     right).}
#'   \item{Display window filter}{Optionally restrict samples and fixations
#'     to the period between `t_display_on` and `t_display_off` as recorded
#'     in `asc_result$trial_db`.}
#'   \item{Overlay toggles}{Independent checkboxes to show/hide raw gaze
#'     samples, detected fixations, and word-boundary lines with labels.}
#'   \item{Animate slider}{Replay the trial up to any relative time (ms
#'     from display onset).}
#'   \item{Background image}{Upload a PNG or JPEG screenshot of the stimulus
#'     display for layout verification.}
#'   \item{Data tables}{Optionally show interactive tables of detected
#'     fixations, word measures, and raw samples below the plot.}
#' }
#'
#' ## Required packages
#' `shiny`, `plotly`, and `DT` must be installed.  The function checks for
#' their availability at call time and stops with an informative message if
#' any are missing.
#'
#' @importFrom dplyr filter mutate tibble left_join across where
#' @importFrom rlang .data
#'
#' @export
#'
#' @examples
#' \dontrun{
#' asc_file <- system.file("extdata", "sub_1_example.asc", package = "fixated")
#' if (file.exists(asc_file)) {
#'   result <- read_asc(asc_file)
#'   plot_trials_shiny(result)
#' }
#' }
plot_trials_shiny <- function(asc_result = NULL, samples = NULL,
                               fixations = NULL, rois = NULL, measures = NULL,
                               roi = NULL, trial_db = NULL,
                               launch.browser = TRUE) {

  # ---- dependency checks ----------------------------------------------------
  for (pkg in c("shiny", "plotly", "DT", "ggplot2", "shinyjs")) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop(
        "Package '", pkg, "' is required for plot_trials_shiny(). ",
        "Install it with: install.packages('", pkg, "')"
      )
    }
  }

  # ---- parse arguments ------------------------------------------------------
  if (!is.null(roi) && is.null(rois)) {
    rois <- roi
  }

  if (is.null(asc_result)) {
    if (is.null(samples) && is.null(fixations)) {
      stop("You must provide either 'asc_result' or 'samples' and 'fixations'.")
    }
  } else {
    stopifnot(is.list(asc_result))
    if (!all(c("samples", "events") %in% names(asc_result))) {
      stop("asc_result must contain 'samples' and 'events' elements.")
    }
    if (is.null(samples)) samples <- asc_result$samples
    if (is.null(fixations)) {
      fixations <- tryCatch(
        get_eyelink_fixations(asc_result$events),
        error = function(e) NULL
      )
    }
    if (is.null(rois)) rois <- asc_result$word_boundaries
    if (is.null(trial_db)) trial_db <- asc_result$trial_db
  }

  # ---- mismatch trials check ------------------------------------------------
  .check_trial_mismatch(samples, fixations, rois, measures, trial_db)

  # ---- normalize subject columns --------------------------------------------
  samples  <- .add_subject_column(samples)
  fixations <- .add_subject_column(fixations)
  rois <- .add_subject_column(rois)
  measures <- .add_subject_column(measures)
  trial_db <- .add_subject_column(trial_db)

  # ---- determine available trials -------------------------------------------
  trial_info    <- .shiny_trial_choices(samples, fixations, rois, measures, trial_db)
  subject_choices <- trial_info$subject_choices
  mapping       <- trial_info$mapping
  has_subject_selector <- !is.null(subject_choices)
  current_subject_value <- if (has_subject_selector) subject_choices[[1L]] else NULL
  current_mapping <- if (has_subject_selector) {
    mapping[mapping$subject == current_subject_value, , drop = FALSE]
  } else {
    mapping
  }
  trial_choices <- .shiny_make_trial_choices(current_mapping)
  sent_choices <- .shiny_make_sentence_choices(current_mapping)
  
  n_trials      <- length(trial_choices)
  if (n_trials == 0L) {
    stop("No trials found in the provided data.")
  }

  # ---- build UI ---------------------------------------------------------------
  ui <- shiny::fluidPage(
    shinyjs::useShinyjs(),
    shiny::titlePanel("Eye-Tracking Trial Visualisation"),
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        if (has_subject_selector) {
          shiny::selectizeInput("subject_sel", "Subject:", choices = subject_choices)
        } else {
          NULL
        },
        if (has_subject_selector) shiny::hr() else NULL,
        shiny::selectizeInput("trial_sel", "Trial:", choices = trial_choices),
        shiny::actionButton("prevBtn", "Previous"),
        shiny::actionButton("nextBtn", "Next"),
        shiny::hr(),
        if (!is.null(sent_choices)) {
          shiny::selectizeInput("sent_sel", "Sentence Number:", choices = sent_choices)
        } else {
          NULL
        },
        if (!is.null(sent_choices)) shiny::hr() else NULL,
        shiny::selectInput("eye", "Eye to plot:", choices = c("R", "L"),
                           selected = "R"),
        shiny::hr(),
        shiny::div(
          id = "display_filter_container",
          title = if (is.null(trial_db)) "Disabled because trial_db or asc_result was not provided" else "",
          shiny::checkboxInput("filter_display_on",
                               "Remove samples before display onset",
                               value = TRUE),
          shiny::checkboxInput("filter_display_off",
                               "Remove samples after display offset",
                               value = TRUE)
        ),
        shiny::hr(),
        shiny::checkboxInput("show_samples",
                             "Show raw gaze samples", value = TRUE),
        shiny::checkboxInput("show_fixations",
                             "Overlay EyeLink fixations", value = TRUE),
        shiny::checkboxInput("show_word_regions",
                             "Show word regions / boundaries", value = TRUE),
        shiny::checkboxInput("show_word_labels",
                             "Show word text labels", value = TRUE),
        shiny::checkboxInput("show_measures",
                             "Show FFD / GD / TVT bars", value = FALSE),
        shiny::hr(),
        shiny::checkboxInput("show_fixation_table",
                             "Show fixation table", value = FALSE),
        shiny::checkboxInput("show_word_table",
                             "Show word measures table", value = FALSE),
        shiny::checkboxInput("show_sample_table",
                             "Show raw samples table", value = FALSE),
        shiny::hr(),
        shiny::fileInput("bg_image", "Background image (PNG/JPEG, optional)",
                         accept = c("image/png", "image/jpeg", "image/jpg")),
        shiny::hr(),
        shiny::helpText(
          "Hover over the plot for detailed tooltips.",
          "Use the animation slider to replay a trial over time."
        )
      ),
      shiny::mainPanel(
        shiny::uiOutput("warning_msg"),
        shiny::uiOutput("time_slider_ui"),
        plotly::plotlyOutput("gazePlot", height = "600px"),
        shiny::conditionalPanel(
          condition = "input.show_fixation_table",
          shiny::tags$hr(),
          shiny::tags$h4("EyeLink Fixations"),
          DT::DTOutput("fixation_dt")
        ),
        shiny::conditionalPanel(
          condition = "input.show_word_table",
          shiny::tags$hr(),
          shiny::tags$h4("Word Measures"),
          DT::DTOutput("word_dt")
        ),
        shiny::conditionalPanel(
          condition = "input.show_sample_table",
          shiny::tags$hr(),
          shiny::tags$h4("Raw Samples"),
          DT::DTOutput("sample_dt")
        )
      )
    )
  )

  # ---- server -----------------------------------------------------------------
  server <- function(input, output, session) {
    current_subject <- shiny::reactive({
      if (has_subject_selector) as.character(input$subject_sel) else NULL
    })

    mapping_current <- shiny::reactive({
      if (!has_subject_selector) return(mapping)
      mapping[mapping$subject == current_subject(), , drop = FALSE]
    })

    trial_choices_current <- shiny::reactive({
      .shiny_make_trial_choices(mapping_current())
    })

    sent_choices_current <- shiny::reactive({
      .shiny_make_sentence_choices(mapping_current())
    })

    # -- handle trial_db presence ---------------------------------------------
    if (is.null(trial_db)) {
      shinyjs::disable("filter_display_on")
      shinyjs::disable("filter_display_off")
    }

    # -- navigation buttons ---------------------------------------------------
    shiny::observeEvent(input$prevBtn, {
      choices <- trial_choices_current()
      idx <- match(input$trial_sel, choices)
      if (idx > 1L) {
        shiny::updateSelectInput(session, "trial_sel", selected = choices[[idx - 1L]])
      }
    })
    shiny::observeEvent(input$nextBtn, {
      choices <- trial_choices_current()
      idx <- match(input$trial_sel, choices)
      if (idx < length(choices)) {
        shiny::updateSelectInput(session, "trial_sel", selected = choices[[idx + 1L]])
      }
    })

    if (has_subject_selector) {
      shiny::observeEvent(input$subject_sel, {
        choices <- trial_choices_current()
        sents <- sent_choices_current()
        shiny::updateSelectizeInput(session, "trial_sel", choices = choices, selected = choices[[1L]], server = TRUE)
        if (!is.null(sents)) {
          shiny::updateSelectizeInput(session, "sent_sel", choices = sents, selected = sents[[1L]], server = TRUE)
        }
      }, ignoreInit = TRUE)
    }

    # -- trial/sentence sync --------------------------------------------------
    # When trial changes, update sentence
    if (!is.null(sent_choices)) {
      shiny::observe({
        shiny::req(input$trial_sel)
        tnr <- as.integer(input$trial_sel)
        map <- mapping_current()
        snr <- map$sentence_nr[map$trial_nr == tnr]
        if (length(snr) > 0 && !is.na(snr[1])) {
          shiny::updateSelectInput(session, "sent_sel", selected = as.character(snr[1]))
        }
      })
      
      # When sentence changes, update trial (if trial does not match the sentence)
      shiny::observeEvent(input$sent_sel, {
        snr <- as.integer(input$sent_sel)
        tnr <- as.integer(input$trial_sel)
        map <- mapping_current()
        
        # Check if current trial belongs to this sentence
        current_snr <- map$sentence_nr[map$trial_nr == tnr]
        if (length(current_snr) == 0 || is.na(current_snr[1]) || current_snr[1] != snr) {
          # Pick the first trial for this sentence
          matching_trials <- map$trial_nr[map$sentence_nr == snr]
          if (length(matching_trials) > 0) {
            shiny::updateSelectInput(session, "trial_sel", selected = as.character(matching_trials[1]))
          }
        }
      }, ignoreInit = TRUE)
    }

    # -- reactive helpers -------------------------------------------------------

    # Current trial_nr (actual value from the data)
    current_tnr <- shiny::reactive({
      if (is.numeric(trial_choices_current())) {
        as.integer(input$trial_sel)
      } else {
        # Fallback if character
        utils::type.convert(input$trial_sel, as.is = TRUE)
      }
    })

    # Resolve display window timestamps for the current trial
    display_times <- shiny::reactive({
      tnr  <- current_tnr()
      tdb  <- trial_db
      t_on <- NA_real_
      t_off <- NA_real_
      if (!is.null(tdb)) {
        if (has_subject_selector && "subject" %in% names(tdb)) {
          tdb <- dplyr::filter(tdb, .data$subject == current_subject())
        }
        row <- if ("trial_nr" %in% names(tdb)) {
          tdb[tdb$trial_nr == tnr, , drop = FALSE]
        } else if ("trial" %in% names(tdb)) {
          tdb[tdb$trial == tnr, , drop = FALSE]
        } else {
          # Assume first col is trial ID
          tdb[tdb[[1L]] == tnr, , drop = FALSE]
        }
        if (nrow(row) > 0L) {
          if ("t_display_on"  %in% names(row)) t_on  <- as.numeric(row$t_display_on[[1L]])
          if ("t_display_off" %in% names(row)) t_off <- as.numeric(row$t_display_off[[1L]])
        }
      }
      list(t_on = t_on, t_off = t_off)
    })

    # Filtered samples for the current trial + eye
    trial_samples <- shiny::reactive({
      tnr   <- current_tnr()
      eye   <- input$eye
      samp  <- samples
      if (has_subject_selector && "subject" %in% names(samp)) {
        samp <- dplyr::filter(samp, .data$subject == current_subject())
      }
      if ("trial_nr" %in% names(samp)) {
        samp <- dplyr::filter(samp, .data$trial_nr == tnr)
      } else if ("trial" %in% names(samp)) {
        samp <- dplyr::filter(samp, .data$trial == tnr)
      }
      if ("eye" %in% names(samp)) {
        samp <- dplyr::filter(samp, .data$eye == !!eye)
      }
      dt  <- display_times()
      if (input$filter_display_on  && !is.na(dt$t_on))  {
        samp <- dplyr::filter(samp, .data$time >= dt$t_on)
      }
      if (input$filter_display_off && !is.na(dt$t_off)) {
        samp <- dplyr::filter(samp, .data$time <= dt$t_off)
      }
      # Add relative time column
      t0 <- if (!is.na(dt$t_on)) {
        dt$t_on
      } else if (nrow(samp) > 0L) {
        min(samp$time, na.rm = TRUE)
      } else {
        0
      }
      if (nrow(samp) > 0L) samp$time_rel <- samp$time - t0
      samp
    })

    # t0 for the current trial
    t_zero <- shiny::reactive({
      dt  <- display_times()
      if (!is.na(dt$t_on)) return(dt$t_on)
      samp <- trial_samples()
      if (nrow(samp) > 0L) min(samp$time, na.rm = TRUE) else 0
    })

    # Filtered fixations for the current trial + eye
    trial_fixations <- shiny::reactive({
      tnr  <- current_tnr()
      eye  <- input$eye
      fix  <- fixations
      if (is.null(fix) || nrow(fix) == 0L) return(NULL)
      if (has_subject_selector && "subject" %in% names(fix)) {
        fix <- dplyr::filter(fix, .data$subject == current_subject())
      }
      
      if ("trial_nr" %in% names(fix)) {
        fix <- dplyr::filter(fix, .data$trial_nr == tnr)
      } else if ("trial" %in% names(fix)) {
        fix <- dplyr::filter(fix, .data$trial == tnr)
      }
      if (nrow(fix) == 0L) return(NULL)
      fix <- dplyr::filter(fix, .data$eye == !!eye)
      dt  <- display_times()
      if (input$filter_display_on  && !is.na(dt$t_on))  {
        fix <- dplyr::filter(fix, .data$start_time >= dt$t_on)
      }
      if (input$filter_display_off && !is.na(dt$t_off)) {
        fix <- dplyr::filter(fix, .data$end_time   <= dt$t_off)
      }
      if (nrow(fix) == 0L) return(NULL)
      t0 <- t_zero()
      fix$time_rel_start <- fix$start_time - t0
      fix$fixation_nr    <- seq_len(nrow(fix))
      fix
    })

    # Resolve word boundary / ROI data for the current trial
    trial_wb <- shiny::reactive({
      tnr <- current_tnr()
      wb  <- NULL
      if (!is.null(rois)) {
        wb_in <- rois
        if (has_subject_selector && "subject" %in% names(wb_in)) {
          wb_in <- dplyr::filter(wb_in, .data$subject == current_subject())
        }
        wb <- if ("trial_nr" %in% names(wb_in)) {
          dplyr::filter(wb_in, .data$trial_nr == tnr)
        } else if ("trial" %in% names(wb_in)) {
          dplyr::filter(wb_in, .data$trial == tnr)
        } else {
          wb_in
        }
      }
      wb
    })

    # Word measures (FFD, GD, TVT) – computed only when needed
    word_measures <- shiny::reactive({
      shiny::req(input$show_measures || input$show_word_table)
      tnr <- current_tnr()
      if (!is.null(measures)) {
        wm_in <- measures
        if (has_subject_selector && "subject" %in% names(wm_in)) {
          wm_in <- dplyr::filter(wm_in, .data$subject == current_subject())
        }
        wm <- if ("trial_nr" %in% names(wm_in)) {
          dplyr::filter(wm_in, .data$trial_nr == tnr)
        } else if ("trial" %in% names(wm_in)) {
          dplyr::filter(wm_in, .data$trial == tnr)
        } else {
          wm_in
        }
        return(wm)
      }
      wb  <- trial_wb()
      fix <- trial_fixations()
      if (is.null(wb) || nrow(wb) == 0L) return(NULL)
      if (!all(c("x_start", "x_end", "y_start", "y_end") %in% names(wb))) return(NULL)
      if (is.null(fix) || nrow(fix) == 0L) return(NULL)

      fix_m <- fix
      if (!"trial_nr" %in% names(fix_m)) fix_m$trial_nr <- tnr
      roi_m <- wb
      if (!"trial_nr" %in% names(roi_m)) roi_m$trial_nr <- tnr

      tryCatch(
        compute_eye_measures(fix_m, roi_m, trial_col = "trial_nr", eye_col = NULL),
        error = function(e) NULL
      )
    })

    # -- warning panel --------------------------------------------------------
    output$warning_msg <- shiny::renderUI({
      # Background image resolution check
      msgs <- list()
      if (!is.null(input$bg_image)) {
        img_path <- input$bg_image$datapath
        img_name <- input$bg_image$name
        if (grepl("\\.(png|jpe?g)$", img_name, ignore.case = TRUE)) {
          bg_dims  <- NULL
          if (grepl("\\.png$", img_name, ignore.case = TRUE) &&
              requireNamespace("png", quietly = TRUE)) {
            bg_dims <- dim(png::readPNG(img_path))[1:2]
          } else if (grepl("\\.jpe?g$", img_name, ignore.case = TRUE) &&
                     requireNamespace("jpeg", quietly = TRUE)) {
            bg_dims <- dim(jpeg::readJPEG(img_path))[1:2]
          }
          if (!is.null(bg_dims)) {
            msgs[[length(msgs) + 1L]] <- shiny::div(
              style = paste0("color:#856404;font-weight:bold;margin-bottom:10px;",
                             "padding:8px;border:1px solid #ffeeba;background:#fff3cd;"),
              sprintf("Background image: %d \u00d7 %d px. ",
                      bg_dims[2L], bg_dims[1L]),
              "Verify this matches your display resolution."
            )
          }
        }
      }
      if (length(msgs) > 0L) do.call(shiny::tagList, msgs) else NULL
    })

    # -- animation time slider ------------------------------------------------
    output$time_slider_ui <- shiny::renderUI({
      samp <- trial_samples()
      if (nrow(samp) == 0L || !"time_rel" %in% names(samp)) return(NULL)
      min_t <- floor(min(samp$time_rel, na.rm = TRUE))
      max_t <- ceiling(max(samp$time_rel, na.rm = TRUE))
      if (min_t >= max_t) return(NULL)
      shiny::sliderInput(
        "time_limit",
        "Animate (time relative to display onset, ms):",
        min = min_t, max = max_t, value = max_t, step = 50L,
        width = "100%",
        animate = shiny::animationOptions(interval = 300L, loop = FALSE)
      )
    })

    # -- main gaze plot -------------------------------------------------------
    output$gazePlot <- plotly::renderPlotly({
      tnr  <- current_tnr()
      samp <- trial_samples()
      fix  <- trial_fixations()
      wb   <- trial_wb()

      # Optionally apply animation time limit
      if (!is.null(input$time_limit) && "time_rel" %in% names(samp)) {
        samp <- dplyr::filter(samp, .data$time_rel <= input$time_limit)
        if (!is.null(fix)) {
          fix <- dplyr::filter(fix, .data$time_rel_start <= input$time_limit)
          if (nrow(fix) == 0L) fix <- NULL
        }
      }

      has_full_roi <- !is.null(wb) && nrow(wb) > 0L &&
        all(c("x_start", "x_end", "y_start", "y_end") %in% names(wb))
      has_x_end    <- !is.null(wb) && nrow(wb) > 0L && "x_end" %in% names(wb)
      has_words    <- !is.null(wb) && nrow(wb) > 0L && "word" %in% names(wb)

      # Median y for label positioning
      y_median <- if (nrow(samp) > 0L) {
        stats::median(samp$y, na.rm = TRUE)
      } else if (has_full_roi) {
        mean(c(wb$y_start[[1L]], wb$y_end[[1L]]))
      } else {
        540
      }

      # --- build base ggplot -------------------------------------------------
      p <- ggplot2::ggplot() +
        ggplot2::theme_minimal() +
        ggplot2::labs(
          title = if (has_subject_selector) {
            paste0("Subject ", current_subject(), " - Trial ", tnr, "  (eye: ", input$eye, ")")
          } else {
            paste0("Trial ", tnr, "  (eye: ", input$eye, ")")
          },
          x = "Screen X (px)",
          y = "Screen Y (px)"
        )

      # Background image (encoded as base64 for plotly)
      bg_layout_images <- NULL
      if (!is.null(input$bg_image) &&
          requireNamespace("base64enc", quietly = TRUE)) {
        img_path <- input$bg_image$datapath
        img_name <- input$bg_image$name
        if (grepl("\\.(png|jpe?g)$", img_name, ignore.case = TRUE)) {
          raw_img   <- readBin(img_path, "raw", file.info(img_path)$size)
          mime_type <- if (grepl("\\.png$", img_name, ignore.case = TRUE)) {
            "image/png"
          } else {
            "image/jpeg"
          }
          bg_w <- 1920L
          bg_h <- 1080L
          if (grepl("\\.png$", img_name, ignore.case = TRUE) &&
              requireNamespace("png", quietly = TRUE)) {
            dims <- dim(png::readPNG(img_path))
            bg_h <- dims[1L]
            bg_w <- dims[2L]
          } else if (requireNamespace("jpeg", quietly = TRUE)) {
            dims <- dim(jpeg::readJPEG(img_path))
            bg_h <- dims[1L]
            bg_w <- dims[2L]
          }
          bg_src <- paste0("data:", mime_type, ";base64,",
                           base64enc::base64encode(raw_img))
          bg_layout_images <- list(list(
            source  = bg_src,
            xref    = "x", yref = "y",
            xanchor = "left", yanchor = "top",
            x = 0, y = 0,
            sizex = bg_w, sizey = bg_h,
            sizing = "stretch",
            layer  = "below"
          ))
        }
      }

      # Word region rectangles / boundary lines
      if (input$show_word_regions && !is.null(wb) && nrow(wb) > 0L) {
        if (has_full_roi) {
          wb_text <- if (has_words) paste0("Word: ", wb$word) else paste0("Region: ", wb$word_id)
          wb_plot <- dplyr::mutate(wb, .wb_tip = wb_text)
          p <- p + ggplot2::geom_rect(
            data = wb_plot,
            ggplot2::aes(
              xmin = .data$x_start, xmax = .data$x_end,
              ymin = .data$y_start, ymax = .data$y_end,
              text = .data$.wb_tip
            ),
            fill = "steelblue", alpha = 0.07, color = "steelblue", linewidth = 0.3
          )
        } else if (has_x_end) {
          p <- p + ggplot2::geom_vline(
            data = wb, ggplot2::aes(xintercept = .data$x_end),
            linetype = "dashed", color = "grey50", alpha = 0.7
          )
        }
      }

      # Word text labels
      if (input$show_word_labels && has_words) {
        label_x <- if (has_full_roi && "x_start" %in% names(wb)) (wb$x_start + wb$x_end) / 2 else if (has_x_end) wb$x_end - 20 else rep(0, nrow(wb))
        label_y <- if (has_full_roi) (wb$y_start + wb$y_end) / 2 else rep(y_median, nrow(wb))
        label_df <- dplyr::tibble(x = label_x, y = label_y, label = wb$word)
        p <- p + ggplot2::geom_text(
          data = label_df, ggplot2::aes(x = .data$x, y = .data$y, label = .data$label),
          vjust = 0.5, size = 3.5, color = "grey20"
        )
      }

      # FFD / GD / TVT bar overlays
      if (input$show_measures) {
        wm <- word_measures()
        if (!is.null(wm) && nrow(wm) > 0L && !is.null(wb) && "word_id" %in% names(wb) && has_full_roi) {
          m_df <- dplyr::left_join(wb, wm[, intersect(names(wm), c("word_id", "ffd", "gd", "tvt"))], by = "word_id")
          m_df <- m_df[(!is.na(m_df$ffd) | !is.na(m_df$gd) | !is.na(m_df$tvt)), , drop = FALSE]
          if (nrow(m_df) > 0L) {
            bar_scale <- 0.5
            if (any(!is.na(m_df$ffd))) {
              ffd_df <- m_df[!is.na(m_df$ffd), , drop = FALSE]
              bw_f   <- (ffd_df$x_end - ffd_df$x_start) * 0.25
              p <- p + ggplot2::geom_rect(
                data = ffd_df, ggplot2::aes(
                  xmin = .data$x_start + bw_f * 0.08, xmax = .data$x_start + bw_f,
                  ymin = .data$y_start - .data$ffd * bar_scale, ymax = .data$y_start,
                  text = paste0("FFD: ", round(.data$ffd), " ms")
                ), fill = "#2ecc71", alpha = 0.6, color = "#27ae60", linewidth = 0.3
              )
            }
            if (any(!is.na(m_df$gd))) {
              gd_df <- m_df[!is.na(m_df$gd), , drop = FALSE]
              bw_g  <- (gd_df$x_end - gd_df$x_start) * 0.25
              p <- p + ggplot2::geom_rect(
                data = gd_df, ggplot2::aes(
                  xmin = .data$x_start + bw_g + (bw_g * 0.08) * 2, xmax = .data$x_start + bw_g * 2 + (bw_g * 0.08),
                  ymin = .data$y_start - .data$gd * bar_scale, ymax = .data$y_start,
                  text = paste0("GD: ", round(.data$gd), " ms")
                ), fill = "#9b59b6", alpha = 0.6, color = "#8e44ad", linewidth = 0.3
              )
            }
            if (any(!is.na(m_df$tvt))) {
              tvt_df <- m_df[!is.na(m_df$tvt), , drop = FALSE]
              bw_t   <- (tvt_df$x_end - tvt_df$x_start) * 0.25
              p <- p + ggplot2::geom_rect(
                data = tvt_df, ggplot2::aes(
                  xmin = .data$x_start + bw_t * 2 + (bw_t * 0.08) * 3, xmax = .data$x_start + bw_t * 3 + (bw_t * 0.08) * 2,
                  ymin = .data$y_start - .data$tvt * bar_scale, ymax = .data$y_start,
                  text = paste0("TVT: ", round(.data$tvt), " ms")
                ), fill = "#3498db", alpha = 0.6, color = "#2980b9", linewidth = 0.3
              )
            }
          }
        }
      }

      # Raw gaze samples
      if (input$show_samples && nrow(samp) > 0L) {
        p <- p + ggplot2::geom_point(
          data = samp, ggplot2::aes(x = .data$x, y = .data$y, text = paste0("t: ", round(.data$time_rel), " ms\nX: ", round(.data$x), "\nY: ", round(.data$y))),
          color = "grey40", size = 0.5, alpha = 0.5
        )
      }

      # Fixation path + circles
      if (input$show_fixations && !is.null(fix) && nrow(fix) > 0L) {
        p <- p + ggplot2::geom_path(data = fix, ggplot2::aes(x = .data$avg_x, y = .data$avg_y), color = "red", linetype = "dashed", alpha = 0.5) +
          ggplot2::geom_point(data = fix, ggplot2::aes(x = .data$avg_x, y = .data$avg_y, size = .data$duration, text = paste0("Fix #", .data$fixation_nr, "\nDur: ", round(.data$duration), " ms\nStart: ", round(.data$time_rel_start), " ms\nX: ", round(.data$avg_x), "\nY: ", round(.data$avg_y))), color = "red", alpha = 0.65) +
          ggplot2::scale_size_continuous(name = "Duration (ms)", range = c(2, 12))
      }

      # --- convert to plotly -------------------------------------------------
      plt <- suppressWarnings(
        plotly::ggplotly(p, tooltip = "text") |>
          plotly::layout(yaxis = list(autorange = "reversed"), images = bg_layout_images) |>
          plotly::config(displayModeBar = TRUE, displaylogo = FALSE)
      )
      plt
    })

    # -- data tables ----------------------------------------------------------
    output$fixation_dt <- DT::renderDT(server = FALSE, {
      shiny::req(input$show_fixation_table)
      fix <- trial_fixations()
      if (is.null(fix) || nrow(fix) == 0L) return(DT::datatable(dplyr::tibble(message = "No fixations found."), rownames = FALSE))
      tbl <- dplyr::mutate(fix[, intersect(c("fixation_nr", "avg_x", "avg_y", "duration", "start_time", "end_time", "time_rel_start"), names(fix))], dplyr::across(dplyr::where(is.numeric), ~ round(.x, 1L)))
      DT::datatable(tbl, rownames = FALSE, options = list(pageLength = 20L, scrollX = TRUE), class = "compact stripe hover")
    })

    output$word_dt <- DT::renderDT(server = FALSE, {
      shiny::req(input$show_word_table)
      wm <- word_measures()
      if (is.null(wm) || nrow(wm) == 0L) return(DT::datatable(dplyr::tibble(message = "No word measures."), rownames = FALSE))
      tbl <- dplyr::mutate(wm[, intersect(c("word_id", "word", "ffd", "gd", "gpt", "tvt", "n_fixations"), names(wm))], dplyr::across(dplyr::where(is.numeric), ~ round(.x, 1L)))
      DT::datatable(tbl, rownames = FALSE, options = list(pageLength = 25L, scrollX = TRUE), class = "compact stripe hover")
    })

    output$sample_dt <- DT::renderDT(server = FALSE, {
      shiny::req(input$show_sample_table)
      samp <- trial_samples()
      if (nrow(samp) == 0L) return(DT::datatable(dplyr::tibble(message = "No samples."), rownames = FALSE))
      tbl <- dplyr::mutate(samp[, intersect(c("time", "time_rel", "x", "y"), names(samp))], dplyr::across(dplyr::where(is.numeric), ~ round(.x, 1L)))
      DT::datatable(tbl, rownames = FALSE, options = list(pageLength = 25L, scrollX = TRUE), class = "compact stripe hover")
    })
  }

  # ---- launch app -----------------------------------------------------------
  app <- shiny::shinyApp(ui, server, options = list(launch.browser = launch.browser))
  shiny::runApp(app)
  invisible(app)
}


# ---------------------------------------------------------------------------
# Internal helpers
# ---------------------------------------------------------------------------

#' Extract unique trials and sentences to build choices for shiny dropdown
#' @return A list with 'subject_choices', 'trial_choices', 'sentence_choices',
#'   and 'mapping'
#' @noRd
.shiny_trial_choices <- function(samples, fixations, rois, measures, trial_db) {
  dfs <- list(trial_db, measures, rois, fixations, samples)
  dfs <- dfs[!vapply(dfs, is.null, logical(1))]
  
  for (df in dfs) {
    df <- .add_subject_column(df)
    t_col <- if ("trial_nr" %in% names(df)) "trial_nr" else if ("trial" %in% names(df)) "trial" else NULL
    if (!is.null(t_col)) {
      mapping_cols <- intersect(c("subject", t_col, "sentence_nr"), names(df))
      mapping <- unique(df[, mapping_cols, drop = FALSE])
      if (t_col != "trial_nr") names(mapping)[names(mapping) == t_col] <- "trial_nr"
      if ("subject" %in% names(mapping)) {
        mapping <- mapping[order(mapping$subject, mapping$trial_nr), , drop = FALSE]
      } else {
        mapping <- mapping[order(mapping$trial_nr), , drop = FALSE]
      }

      subject_choices <- NULL
      if ("subject" %in% names(mapping)) {
        subs <- unique(mapping$subject)
        subject_choices <- as.character(subs)
        names(subject_choices) <- paste0("Subject ", subs)
      }

      mapping_first <- if ("subject" %in% names(mapping)) {
        mapping[mapping$subject == subject_choices[[1L]], , drop = FALSE]
      } else {
        mapping
      }

      return(list(
        subject_choices = subject_choices,
        trial_choices = .shiny_make_trial_choices(mapping_first),
        sentence_choices = .shiny_make_sentence_choices(mapping_first),
        mapping = mapping
      ))
    }
  }
  list(
    subject_choices = NULL,
    trial_choices = stats::setNames(0L, "Trial 0"),
    sentence_choices = NULL,
    mapping = data.frame(trial_nr = 0L)
  )
}

#' @noRd
.shiny_make_trial_choices <- function(mapping) {
  if (is.null(mapping) || nrow(mapping) == 0L || !"trial_nr" %in% names(mapping)) {
    return(stats::setNames(0L, "Trial 0"))
  }
  choices <- mapping$trial_nr
  if ("sentence_nr" %in% names(mapping)) {
    names(choices) <- ifelse(
      is.na(mapping$sentence_nr),
      paste0("Trial ", mapping$trial_nr),
      paste0("Trial ", mapping$trial_nr, " (Sentence ", mapping$sentence_nr, ")")
    )
  } else {
    names(choices) <- paste0("Trial ", mapping$trial_nr)
  }
  choices
}

#' @noRd
.shiny_make_sentence_choices <- function(mapping) {
  if (is.null(mapping) || nrow(mapping) == 0L || !"sentence_nr" %in% names(mapping)) {
    return(NULL)
  }
  sents <- sort(unique(mapping$sentence_nr[!is.na(mapping$sentence_nr)]))
  if (length(sents) == 0L) return(NULL)
  sent_choices <- as.character(sents)
  names(sent_choices) <- paste0("Sentence ", sents)
  sent_choices
}

#' Check that all passed discrete tables contain the same set of trials
#' @noRd
.check_trial_mismatch <- function(...) {
  dfs <- list(...)
  dfs <- dfs[!vapply(dfs, is.null, logical(1))]
  if (length(dfs) <= 1L) return()
  trial_lists <- lapply(dfs, function(df) {
    if ("trial_nr" %in% names(df)) sort(unique(df$trial_nr)) else if ("trial" %in% names(df)) sort(unique(df$trial)) else NULL
  })
  trial_lists <- trial_lists[!vapply(trial_lists, is.null, logical(1))]
  if (length(trial_lists) <= 1L) return()
  first_trials <- trial_lists[[1L]]
  for (i in seq_along(trial_lists)[-1L]) {
    if (!identical(trial_lists[[i]], first_trials)) {
      warning("The provided data frames do not contain the exact same set of trials.")
      break
    }
  }
}
