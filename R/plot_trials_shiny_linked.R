#' @title Interactive Shiny app with linked fixation highlighting (WebGL)
#'
#' @description
#' Opens a \pkg{shiny} application that lets you browse every trial in an
#' experiment with **linked highlighting**: clicking a fixation marker in the
#' plot highlights its associated gaze samples and word region of interest, and
#' synchronises the data tables below the plot.  Bidirectional linking means
#' selecting a row in the fixation table also highlights it in the plot.
#'
#' @param asc_result A named list as returned by \code{\link{read_asc}},
#'   containing at minimum `samples` and `events`.  If you prefer not to use
#'   `read_asc`, you may set this to `NULL` and provide the other data frames
#'   individually.
#' @param samples A data frame of raw gaze samples.
#' @param fixations A data frame of detected fixations.
#' @param rois A data frame of word regions of interest. Takes precedence over
#'   `asc_result$word_boundaries`.
#' @param chars A data frame of character regions of interest. Takes precedence
#'   over `asc_result$character_boundaries`.
#' @param measures A data frame of word measures. If provided, the app will not
#'   recompute measures internally.
#' @param roi A data frame of word regions of interest as returned by
#'   \code{\link{read_roi}}. Alias for `rois`.
#' @param trial_db A data frame containing trial metadata (e.g. `t_display_on`).
#' @param launch.browser Logical.  Passed to
#'   \code{\link[shiny]{shinyApp}}'s `launch.browser` option.  Defaults to
#'   `TRUE`.
#'
#' @return Invisibly, the \pkg{shiny} app object (returned by
#'   \code{\link[shiny]{shinyApp}}).  The app is also launched immediately
#'   (unless `launch.browser = FALSE`).
#'
#' @details
#' ## Linked Highlighting
#' Click any fixation marker in the plot to:
#' \enumerate{
#'   \item Highlight the raw gaze samples belonging to that fixation (blue,
#'     full opacity) while dimming all other samples.
#'   \item Highlight the word ROI that the fixation falls inside (orange
#'     border and fill).
#'   \item Select the corresponding row in the fixation table, word measures
#'     table, and samples table.
#' }
#' The linking is **bidirectional**: selecting a row in the fixation table
#' highlights it in the plot as well.
#'
#' ## Controls
#' Same as \code{\link{plot_trials_shiny_fast}}: trial/sentence selectors,
#' eye toggle, display-window filter, overlay checkboxes, animation slider,
#' background image upload, and optional data tables.
#'
#' ## Required packages
#' `shiny`, `plotly`, `DT`, and `shinyjs` must be installed.
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
#'   plot_trials_shiny_linked(result)
#' }
#' }
plot_trials_shiny_linked <- function(asc_result = NULL, samples = NULL,
                                     fixations = NULL, rois = NULL,
                                     measures = NULL, roi = NULL,
                                     trial_db = NULL, chars = NULL,
                                     launch.browser = TRUE) {

  # ---- dependency checks ----------------------------------------------------
  for (pkg in c("shiny", "plotly", "DT", "shinyjs")) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop(
        "Package '", pkg, "' is required for plot_trials_shiny_linked(). ",
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
    if (is.null(chars)) chars <- asc_result$character_boundaries
    if (is.null(trial_db)) trial_db <- asc_result$trial_db
  }

  # ---- mismatch trials check ------------------------------------------------
  .check_trial_mismatch(samples, fixations, rois, measures, trial_db)

  # ---- determine available trials -------------------------------------------
  trial_info    <- .shiny_trial_choices(samples, fixations, rois, measures, trial_db)
  trial_choices <- trial_info$trial_choices
  sent_choices  <- trial_info$sentence_choices
  mapping       <- trial_info$mapping

  n_trials <- length(trial_choices)
  if (n_trials == 0L) {
    stop("No trials found in the provided data.")
  }

  # ---- build UI ---------------------------------------------------------------
  ui <- shiny::fluidPage(
    shinyjs::useShinyjs(),
    shiny::titlePanel("Eye-Tracking Trial Visualisation (Linked)"),
    shiny::sidebarLayout(
      shiny::sidebarPanel(
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
        shiny::checkboxInput("show_char_regions",
                             "Show character regions / boundaries", value = FALSE),
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
        shiny::conditionalPanel(
          condition = "input.show_sample_table",
          shiny::checkboxInput("filter_samples_to_fixation",
                               "Show only selected fixation's samples",
                               value = FALSE)
        ),
        shiny::hr(),
        shiny::fileInput("bg_image", "Background image (PNG/JPEG, optional)",
                         accept = c("image/png", "image/jpeg", "image/jpg")),
        shiny::hr(),
        shiny::helpText(
          "Click a fixation marker to highlight its samples and word ROI.",
          "Selecting a table row highlights the fixation in the plot."
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

    # -- handle trial_db presence ---------------------------------------------
    if (is.null(trial_db)) {
      shinyjs::disable("filter_display_on")
      shinyjs::disable("filter_display_off")
    }

    # -- navigation buttons ---------------------------------------------------
    shiny::observeEvent(input$prevBtn, {
      idx <- match(input$trial_sel, trial_choices)
      if (idx > 1L) {
        shiny::updateSelectInput(session, "trial_sel",
                                 selected = trial_choices[[idx - 1L]])
      }
    })
    shiny::observeEvent(input$nextBtn, {
      idx <- match(input$trial_sel, trial_choices)
      if (idx < n_trials) {
        shiny::updateSelectInput(session, "trial_sel",
                                 selected = trial_choices[[idx + 1L]])
      }
    })

    # -- trial/sentence sync --------------------------------------------------
    if (!is.null(sent_choices)) {
      shiny::observe({
        shiny::req(input$trial_sel)
        tnr <- as.integer(input$trial_sel)
        snr <- mapping$sentence_nr[mapping$trial_nr == tnr]
        if (length(snr) > 0 && !is.na(snr[1])) {
          shiny::updateSelectInput(session, "sent_sel",
                                   selected = as.character(snr[1]))
        }
      })

      shiny::observeEvent(input$sent_sel, {
        snr <- as.integer(input$sent_sel)
        tnr <- as.integer(input$trial_sel)
        current_snr <- mapping$sentence_nr[mapping$trial_nr == tnr]
        if (length(current_snr) == 0 || is.na(current_snr[1]) ||
            current_snr[1] != snr) {
          matching_trials <- mapping$trial_nr[mapping$sentence_nr == snr]
          if (length(matching_trials) > 0) {
            shiny::updateSelectInput(session, "trial_sel",
                                     selected = as.character(matching_trials[1]))
          }
        }
      }, ignoreInit = TRUE)
    }

    # -- reactive helpers -------------------------------------------------------

    # Current trial_nr (actual value from the data)
    current_tnr <- shiny::reactive({
      if (is.numeric(trial_choices)) {
        as.integer(input$trial_sel)
      } else {
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
        row <- if ("trial_nr" %in% names(tdb)) {
          tdb[tdb$trial_nr == tnr, , drop = FALSE]
        } else if ("trial" %in% names(tdb)) {
          tdb[tdb$trial == tnr, , drop = FALSE]
        } else {
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
        wb <- if ("trial_nr" %in% names(rois)) {
          dplyr::filter(rois, .data$trial_nr == tnr)
        } else if ("trial" %in% names(rois)) {
          dplyr::filter(rois, .data$trial == tnr)
        } else {
          rois
        }
      }
      wb
    })

    # Resolve character boundary ROI data for the current trial
    trial_cb <- shiny::reactive({
      tnr <- current_tnr()
      cb  <- NULL
      if (!is.null(chars)) {
        cb <- if ("trial_nr" %in% names(chars)) {
          dplyr::filter(chars, .data$trial_nr == tnr)
        } else if ("trial" %in% names(chars)) {
          dplyr::filter(chars, .data$trial == tnr)
        } else {
          chars
        }
      }
      cb
    })

    # ===== LINKING REACTIVES ==================================================

    # Assign each sample to a fixation (temporal join)
    linked_samples <- shiny::reactive({
      samp <- trial_samples()
      fix  <- trial_fixations()
      if (is.null(fix) || nrow(fix) == 0L || nrow(samp) == 0L) {
        samp$fixation_id <- NA_integer_
        return(samp)
      }
      samp$fixation_id <- NA_integer_
      for (i in seq_len(nrow(fix))) {
        in_fix <- samp$time >= fix$start_time[i] & samp$time <= fix$end_time[i]
        samp$fixation_id[in_fix] <- fix$fixation_nr[i]
      }
      samp
    })

    # Assign each fixation to a word ROI (spatial join)
    linked_fixations <- shiny::reactive({
      fix <- trial_fixations()
      wb  <- trial_wb()
      if (is.null(fix) || nrow(fix) == 0L) return(NULL)
      if (is.null(wb) || nrow(wb) == 0L ||
          !all(c("x_start", "x_end", "y_start", "y_end") %in% names(wb))) {
        fix$word_id <- NA_integer_
        return(fix)
      }
      fix$word_id <- NA_integer_
      for (i in seq_len(nrow(fix))) {
        hit <- which(
          wb$x_start <= fix$avg_x[i] & fix$avg_x[i] <= wb$x_end &
            wb$y_start <= fix$avg_y[i] & fix$avg_y[i] <= wb$y_end
        )
        if (length(hit) >= 1L) fix$word_id[i] <- wb$word_id[hit[1]]
      }
      fix
    })

    # ===== CLICK / SELECTION HANDLING =========================================

    # Capture plotly click on fixation markers
    clicked_fix_id <- shiny::reactive({
      d <- plotly::event_data("plotly_click", source = "gaze_linked")
      if (is.null(d)) return(NULL)
      id <- d$customdata
      if (is.null(id) || is.na(id)) return(NULL)
      as.integer(id)
    })

    # Bidirectional selection: merge plot click and DT row selection
    selected_fix_id <- shiny::reactiveVal(NULL)

    # When trial changes, clear selection
    shiny::observeEvent(current_tnr(), {
      selected_fix_id(NULL)
    })

    # Update from plot click (toggle: clicking same fixation deselects it)
    shiny::observe({
      fid <- clicked_fix_id()
      shiny::req(fid)
      shiny::isolate({
        if (identical(selected_fix_id(), fid)) {
          selected_fix_id(NULL)
        } else {
          selected_fix_id(fid)
        }
      })
    })

    # Update from fixation table row click (just set; deselection via plot click)
    shiny::observeEvent(input$fixation_dt_rows_selected, {
      sel_row <- input$fixation_dt_rows_selected
      fix <- trial_fixations()
      shiny::req(!is.null(sel_row), !is.null(fix), sel_row <= nrow(fix))
      selected_fix_id(fix$fixation_nr[sel_row])
    })

    # Derive selected word_id
    selected_word_id <- shiny::reactive({
      fid <- selected_fix_id()
      if (is.null(fid)) return(NULL)
      lfix <- linked_fixations()
      if (is.null(lfix) || !"word_id" %in% names(lfix)) return(NULL)
      idx <- which(lfix$fixation_nr == fid)
      if (length(idx) == 0L) return(NULL)
      wid <- lfix$word_id[idx[1]]
      if (is.na(wid)) NULL else wid
    })

    # ===== WORD MEASURES ======================================================

    word_measures <- shiny::reactive({
      shiny::req(input$show_measures || input$show_word_table)
      tnr <- current_tnr()
      if (!is.null(measures)) {
        wm <- if ("trial_nr" %in% names(measures)) {
          dplyr::filter(measures, .data$trial_nr == tnr)
        } else if ("trial" %in% names(measures)) {
          dplyr::filter(measures, .data$trial == tnr)
        } else {
          measures
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
      msgs <- list()
      if (!is.null(input$bg_image)) {
        img_path <- input$bg_image$datapath
        img_name <- input$bg_image$name
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
      samp <- linked_samples()
      fix  <- linked_fixations()
      wb   <- trial_wb()
      cb   <- trial_cb()
      sel_id <- selected_fix_id()

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

      sel_wid <- selected_word_id()

      # --- Start Plotly Object ---
      p <- plotly::plot_ly(source = "gaze_linked")

      # Base layout
      p <- plotly::layout(
        p,
        title = paste0("Trial ", tnr, "  (eye: ", input$eye, ")"),
        xaxis = list(title = "Screen X (px)", showgrid = TRUE, zeroline = FALSE),
        yaxis = list(title = "Screen Y (px)", showgrid = TRUE,
                     autorange = "reversed", zeroline = FALSE)
      )

      # Background image
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
          p <- plotly::layout(p, images = list(list(
            source  = bg_src,
            xref = "x", yref = "y",
            xanchor = "left", yanchor = "top",
            x = 0, y = 0,
            sizex = bg_w, sizey = bg_h,
            sizing = "stretch",
            layer  = "below"
          )))
        }
      }

      shapes <- list()
      hover_x   <- numeric()
      hover_y   <- numeric()
      hover_txt <- character()

      # Word region rectangles (with highlight)
      if (input$show_word_regions && !is.null(wb) && nrow(wb) > 0L) {
        if (has_full_roi) {
          for (i in seq_len(nrow(wb))) {
            is_sel_w <- !is.null(sel_wid) && !is.na(wb$word_id[[i]]) &&
              wb$word_id[[i]] == sel_wid
            wb_text <- if (has_words) paste0("Word: ", wb$word[[i]]) else paste0("Region: ", wb$word_id[[i]])
            shapes <- append(shapes, list(list(
              type = "rect",
              x0 = wb$x_start[[i]], x1 = wb$x_end[[i]],
              y0 = wb$y_start[[i]], y1 = wb$y_end[[i]],
              fillcolor = if (is_sel_w) "rgba(255,165,0,0.25)" else "rgba(70,130,180,0.07)",
              line = list(
                color = if (is_sel_w) "orange" else "steelblue",
                width = if (is_sel_w) 2 else 0.3
              ),
              layer = "below"
            )))
            hover_x <- c(hover_x, (wb$x_start[[i]] + wb$x_end[[i]]) / 2)
            hover_y <- c(hover_y, (wb$y_start[[i]] + wb$y_end[[i]]) / 2)
            hover_txt <- c(hover_txt, wb_text)
          }
        } else if (has_x_end) {
          for (i in seq_len(nrow(wb))) {
            shapes <- append(shapes, list(list(
              type = "line",
              x0 = wb$x_end[[i]], x1 = wb$x_end[[i]],
              y0 = 0, y1 = 1, yref = "paper",
              line = list(color = "gray", width = 1, dash = "dash"),
              layer = "below"
            )))
          }
        }
      }

      # Character region rectangles
      if (input$show_char_regions && !is.null(cb) && nrow(cb) > 0L) {
        has_full_cb <- all(c("x_start", "x_end", "y_start", "y_end") %in% names(cb))
        has_cb_x_end <- "x_end" %in% names(cb)

        if (has_full_cb) {
          for (i in seq_len(nrow(cb))) {
            cb_text <- if ("char" %in% names(cb)) paste0("Char: ", cb$char[[i]]) else paste0("Char: ", cb$char_id[[i]])
            shapes <- append(shapes, list(list(
              type = "rect",
              x0 = cb$x_start[[i]], x1 = cb$x_end[[i]],
              y0 = cb$y_start[[i]], y1 = cb$y_end[[i]],
              fillcolor = "rgba(255,165,0,0.05)",
              line = list(color = "orange", width = 0.2),
              layer = "below"
            )))
            hover_x <- c(hover_x, (cb$x_start[[i]] + cb$x_end[[i]]) / 2)
            hover_y <- c(hover_y, (cb$y_start[[i]] + cb$y_end[[i]]) / 2)
            hover_txt <- c(hover_txt, cb_text)
          }
        } else if (has_cb_x_end) {
          for (i in seq_len(nrow(cb))) {
            shapes <- append(shapes, list(list(
              type = "line",
              x0 = cb$x_end[[i]], x1 = cb$x_end[[i]],
              y0 = 0, y1 = 1, yref = "paper",
              line = list(color = "orange", width = 0.5, dash = "dot"),
              layer = "below"
            )))
          }
        }
      }

      # Word text labels
      if (input$show_word_labels && has_words) {
        label_x <- if (has_full_roi && "x_start" %in% names(wb)) {
          (wb$x_start + wb$x_end) / 2
        } else if (has_x_end) {
          wb$x_end - 20
        } else {
          rep(0, nrow(wb))
        }
        label_y <- if (has_full_roi) {
          (wb$y_start + wb$y_end) / 2
        } else {
          rep(y_median, nrow(wb))
        }
        p <- plotly::add_text(
          p, x = label_x, y = label_y, text = wb$word,
          textfont = list(color = "gray20", size = 11),
          hoverinfo = "none", showlegend = FALSE
        )
      }

      # FFD / GD / TVT bar overlays
      if (input$show_measures) {
        wm <- word_measures()
        if (!is.null(wm) && nrow(wm) > 0L && !is.null(wb) &&
            "word_id" %in% names(wb) && has_full_roi) {
          m_df <- dplyr::left_join(
            wb,
            wm[, intersect(names(wm), c("word_id", "ffd", "gd", "tvt"))],
            by = "word_id"
          )
          m_df <- m_df[(!is.na(m_df$ffd) | !is.na(m_df$gd) |
                          !is.na(m_df$tvt)), , drop = FALSE]
          if (nrow(m_df) > 0L) {
            bar_scale <- 0.5
            for (i in seq_len(nrow(m_df))) {
              x0 <- m_df$x_start[[i]]
              x1 <- m_df$x_end[[i]]
              y1 <- m_df$y_start[[i]]
              bw <- (x1 - x0) * 0.25
              gap <- bw * 0.08

              if (!is.na(m_df$ffd[[i]])) {
                bx0 <- x0 + gap
                bx1 <- x0 + bw
                by0 <- y1 - m_df$ffd[[i]] * bar_scale
                by1 <- y1
                shapes <- append(shapes, list(list(
                  type = "rect", x0 = bx0, x1 = bx1, y0 = by0, y1 = by1,
                  fillcolor = "rgba(46,204,113,0.6)",
                  line = list(color = "#27ae60", width = 0.3)
                )))
                hover_x <- c(hover_x, (bx0 + bx1) / 2)
                hover_y <- c(hover_y, by0)
                hover_txt <- c(hover_txt, paste0("FFD: ", round(m_df$ffd[[i]]), " ms"))
              }
              if (!is.na(m_df$gd[[i]])) {
                bx0 <- x0 + bw + gap * 2
                bx1 <- x0 + bw * 2 + gap
                by0 <- y1 - m_df$gd[[i]] * bar_scale
                by1 <- y1
                shapes <- append(shapes, list(list(
                  type = "rect", x0 = bx0, x1 = bx1, y0 = by0, y1 = by1,
                  fillcolor = "rgba(155,89,182,0.6)",
                  line = list(color = "#8e44ad", width = 0.3)
                )))
                hover_x <- c(hover_x, (bx0 + bx1) / 2)
                hover_y <- c(hover_y, by0)
                hover_txt <- c(hover_txt, paste0("GD: ", round(m_df$gd[[i]]), " ms"))
              }
              if (!is.na(m_df$tvt[[i]])) {
                bx0 <- x0 + bw * 2 + gap * 3
                bx1 <- x0 + bw * 3 + gap * 2
                by0 <- y1 - m_df$tvt[[i]] * bar_scale
                by1 <- y1
                shapes <- append(shapes, list(list(
                  type = "rect", x0 = bx0, x1 = bx1, y0 = by0, y1 = by1,
                  fillcolor = "rgba(52,152,219,0.6)",
                  line = list(color = "#2980b9", width = 0.3)
                )))
                hover_x <- c(hover_x, (bx0 + bx1) / 2)
                hover_y <- c(hover_y, by0)
                hover_txt <- c(hover_txt, paste0("TVT: ", round(m_df$tvt[[i]]), " ms"))
              }
            }
          }
        }
      }

      p <- plotly::layout(p, shapes = shapes)

      # Add dummy scatter trace for shapes tooltips
      if (length(hover_x) > 0L) {
        p <- plotly::add_markers(
          p, x = hover_x, y = hover_y, text = hover_txt,
          hoverinfo = "text", showlegend = FALSE,
          marker = list(opacity = 0, size = 1)
        )
      }

      # ---- RAW GAZE SAMPLES (two-layer highlighting) -----------------------
      if (input$show_samples && nrow(samp) > 0L) {
        if (!is.null(sel_id)) {
          # Background: unselected samples
          bg <- samp[is.na(samp$fixation_id) | samp$fixation_id != sel_id, , drop = FALSE]
          if (nrow(bg) > 0L) {
            p <- plotly::add_trace(
              p, data = bg,
              x = ~x, y = ~y,
              type = "scattergl", mode = "lines+markers",
              line = list(color = "rgba(200,200,200,0.15)", width = 0.5),
              marker = list(color = "rgba(200,200,200,0.15)", size = 2),
              hoverinfo = "text",
              text = ~paste0("t: ", round(time_rel), " ms<br>",
                             "X: ", round(x), "<br>Y: ", round(y)),
              name = "Samples (other)"
            )
          }
          # Foreground: selected fixation's samples
          hl <- samp[!is.na(samp$fixation_id) & samp$fixation_id == sel_id, , drop = FALSE]
          if (nrow(hl) > 0L) {
            p <- plotly::add_trace(
              p, data = hl,
              x = ~x, y = ~y,
              type = "scattergl", mode = "lines+markers",
              line = list(color = "rgba(0,100,255,0.8)", width = 1.5),
              marker = list(color = "rgba(0,100,255,0.8)", size = 4),
              hoverinfo = "text",
              text = ~paste0("Fix #", fixation_id, "<br>",
                             "t: ", round(time_rel), " ms<br>",
                             "X: ", round(x), "<br>Y: ", round(y)),
              name = "Samples (selected)"
            )
          }
        } else {
          # No selection — show all at default opacity
          p <- plotly::add_trace(
            p, data = samp,
            x = ~x, y = ~y,
            type = "scattergl", mode = "lines+markers",
            line = list(color = "rgba(100,100,100,0.5)", width = 0.5),
            marker = list(color = "rgba(100,100,100,0.5)", size = 2),
            hoverinfo = "text",
            text = ~paste0("t: ", round(time_rel), " ms<br>",
                           "X: ", round(x), "<br>Y: ", round(y)),
            name = "Samples"
          )
        }
      }

      # ---- FIXATION PATH + MARKERS (with highlight) -----------------------
      if (input$show_fixations && !is.null(fix) && nrow(fix) > 0L) {

        # Line path connecting fixations
        p <- plotly::add_trace(
          p, data = fix,
          x = ~avg_x, y = ~avg_y,
          type = "scatter", mode = "lines",
          line = list(color = "rgba(255,0,0,0.5)", width = 1, dash = "dash"),
          hoverinfo = "none",
          name = "Fixation Path"
        )

        # Tooltip content
        hover_txt_fix <- paste0(
          "Fix #", fix$fixation_nr, "<br>",
          "Dur: ", round(fix$duration), " ms<br>",
          "Start: ", round(fix$time_rel_start), " ms<br>",
          "X: ", round(fix$avg_x), "<br>",
          "Y: ", round(fix$avg_y)
        )

        # Append ROI info to tooltip if available
        if (!is.null(wb) && has_full_roi && nrow(wb) > 0L) {
          associated_roi <- vapply(seq_len(nrow(fix)), function(i) {
            fx <- fix$avg_x[i]
            fy <- fix$avg_y[i]
            match_idx <- which(
              fx >= wb$x_start & fx <= wb$x_end &
                fy >= wb$y_start & fy <= wb$y_end
            )
            if (length(match_idx) > 0) {
              if (has_words) as.character(wb$word[match_idx[1]]) else as.character(wb$word_id[match_idx[1]])
            } else {
              NA_character_
            }
          }, character(1))
          hover_txt_fix <- ifelse(
            is.na(associated_roi), hover_txt_fix,
            paste0(hover_txt_fix, "<br><b>ROI:</b> ", associated_roi)
          )
        }

        # Scale duration to marker size
        dur_min <- min(fix$duration, na.rm = TRUE)
        dur_max <- max(fix$duration, na.rm = TRUE)
        marker_sizes <- if (dur_max > dur_min) {
          5 + 25 * ((fix$duration - dur_min) / (dur_max - dur_min))
        } else {
          rep(10, nrow(fix))
        }

        # Marker styling: highlight selected
        is_sel <- !is.null(sel_id) & fix$fixation_nr == sel_id
        fix_colors    <- ifelse(is_sel, "rgba(255,165,0,0.9)", "rgba(255,0,0,0.65)")
        fix_border    <- ifelse(is_sel, "rgba(255,165,0,1)", "rgba(255,0,0,0.8)")
        fix_border_w  <- ifelse(is_sel, 3, 1)

        p <- plotly::add_trace(
          p, data = fix,
          x = ~avg_x, y = ~avg_y,
          type = "scatter", mode = "markers",
          customdata = ~fixation_nr,
          marker = list(
            color = fix_colors,
            size  = marker_sizes,
            line  = list(color = fix_border, width = fix_border_w)
          ),
          text = hover_txt_fix,
          hoverinfo = "text",
          name = "Fixations"
        )
      }

      p |> plotly::config(displayModeBar = TRUE, displaylogo = FALSE)
    })

    # -- fixation data table --------------------------------------------------
    output$fixation_dt <- DT::renderDT(server = FALSE, {
      shiny::req(input$show_fixation_table)
      fix <- trial_fixations()
      if (is.null(fix) || nrow(fix) == 0L) {
        return(DT::datatable(
          dplyr::tibble(message = "No fixations found for this trial/eye."),
          rownames = FALSE
        ))
      }
      cols <- c("fixation_nr", "avg_x", "avg_y", "duration",
                "start_time", "end_time", "time_rel_start")
      cols <- intersect(cols, names(fix))
      tbl  <- fix[, cols, drop = FALSE]
      tbl  <- dplyr::mutate(tbl, dplyr::across(
        dplyr::where(is.numeric), ~ round(.x, 1L)
      ))
      # Determine selected row
      sel_row <- NULL
      fid <- selected_fix_id()
      if (!is.null(fid)) {
        idx <- which(fix$fixation_nr == fid)
        if (length(idx) > 0L) sel_row <- idx
      }
      DT::datatable(
        tbl,
        rownames = FALSE,
        selection = list(mode = "single", selected = sel_row),
        options  = list(pageLength = 20L, scrollX = TRUE),
        class    = "compact stripe hover"
      )
    })

    # -- word measures table --------------------------------------------------
    output$word_dt <- DT::renderDT(server = FALSE, {
      shiny::req(input$show_word_table)
      wm <- word_measures()
      if (is.null(wm) || nrow(wm) == 0L) {
        wb <- trial_wb()
        msg <- if (is.null(wb) || nrow(wb) == 0L) {
          "No word boundary data available for this trial."
        } else if (!all(c("x_start", "x_end", "y_start", "y_end") %in% names(wb))) {
          "Word boundaries lack full ROI columns (x_start/x_end/y_start/y_end)."
        } else {
          "No fixations found; cannot compute word measures."
        }
        return(DT::datatable(
          dplyr::tibble(message = msg),
          rownames = FALSE
        ))
      }
      cols <- intersect(
        c("word_id", "word", "ffd", "gd", "gpt", "tvt", "n_fixations"),
        names(wm)
      )
      tbl <- wm[, cols, drop = FALSE]
      tbl <- dplyr::mutate(tbl, dplyr::across(
        dplyr::where(is.numeric), ~ round(.x, 1L)
      ))
      # Determine selected row
      sel_row <- NULL
      wid <- selected_word_id()
      if (!is.null(wid) && "word_id" %in% names(wm)) {
        idx <- which(wm$word_id == wid)
        if (length(idx) > 0L) sel_row <- idx
      }
      DT::datatable(
        tbl,
        rownames = FALSE,
        selection = list(mode = "single", selected = sel_row),
        options  = list(pageLength = 25L, scrollX = TRUE),
        class    = "compact stripe hover"
      )
    })

    # -- raw samples table ----------------------------------------------------
    output$sample_dt <- DT::renderDT(server = FALSE, {
      shiny::req(input$show_sample_table)
      samp <- linked_samples()
      # Filter to selected fixation if requested
      if (isTRUE(input$filter_samples_to_fixation)) {
        fid <- selected_fix_id()
        if (!is.null(fid)) {
          samp <- samp[!is.na(samp$fixation_id) & samp$fixation_id == fid, , drop = FALSE]
        }
      }
      if (nrow(samp) == 0L) {
        return(DT::datatable(
          dplyr::tibble(message = "No samples found for this trial/eye."),
          rownames = FALSE
        ))
      }
      cols <- intersect(c("time", "time_rel", "x", "y", "fixation_id"), names(samp))
      tbl  <- samp[, cols, drop = FALSE]
      tbl  <- dplyr::mutate(tbl, dplyr::across(
        dplyr::where(is.numeric), ~ round(.x, 1L)
      ))
      DT::datatable(
        tbl,
        rownames = FALSE,
        selection = "none",
        options  = list(pageLength = 25L, scrollX = TRUE),
        class    = "compact stripe hover"
      ) |>
        DT::formatStyle(
          "fixation_id",
          backgroundColor = DT::styleEqual(
            selected_fix_id(),
            "rgba(0,100,255,0.15)"
          )
        )
    })
  }

  # ---- launch app -----------------------------------------------------------
  app <- shiny::shinyApp(ui, server,
                         options = list(launch.browser = launch.browser))
  shiny::runApp(app)
  invisible(app)
}
