#' @title Launch an interactive Shiny app to visualise eye-tracking trials (Fast WebGL)
#'
#' @description
#' Opens a \pkg{shiny} application that lets you browse every trial in an
#' experiment, toggling overlays for raw gaze samples, EyeLink-detected
#' fixations, and word regions of interest.  The gaze plot is rendered with
#' \pkg{plotly} natively using WebGL (`scattergl`) to handle large numbers of
#' data points smoothly.
#'
#' @param asc_result A named list as returned by \code{\link{read_asc}},
#'   containing at minimum `samples` and `events`.  If you prefer not to use `read_asc`,
#'   you may set this to `NULL` and provide the other data frames individually.
#' @param samples A data frame of raw gaze samples.
#' @param fixations A data frame of detected fixations.
#' @param rois A data frame of word regions of interest. Takes precedence over
#'   `asc_result$word_boundaries`.
#' @param chars A data frame of character regions of interest. Takes precedence over
#'   `asc_result$character_boundaries`.
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
#'   plot_trials_shiny_fast(result)
#' }
#' }
plot_trials_shiny_fast <- function(asc_result = NULL, samples = NULL,
                              fixations = NULL, rois = NULL, measures = NULL,
                              roi = NULL, trial_db = NULL, chars = NULL,
                              launch.browser = TRUE) {

  # ---- dependency checks ----------------------------------------------------
  for (pkg in c("shiny", "plotly", "DT", "shinyjs")) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop(
        "Package '", pkg, "' is required for plot_trials_shiny_fast(). ",
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

  # ---- normalize subject columns --------------------------------------------
  samples  <- .add_subject_column(samples)
  fixations <- .add_subject_column(fixations)
  rois <- .add_subject_column(rois)
  chars <- .add_subject_column(chars)
  measures <- .add_subject_column(measures)
  trial_db <- .add_subject_column(trial_db)

  # ---- flag: character boundary data is available ----------------------------
  has_char_bounds <- !is.null(chars) && is.data.frame(chars) && nrow(chars) > 0L
  # ---- flag: fixations have a 'removed' annotation column (clean_fixations annotate=TRUE) ----
  has_removed_col <- !is.null(fixations) && "removed" %in% names(fixations)

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
    shiny::titlePanel("Eye-Tracking Trial Visualisation (Fast)"),
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
        shiny::div(
          id = "removed_fixations_container",
          title = if (!has_removed_col) paste0(
            "Disabled: fixations tibble does not have a 'removed' column. ",
            "Use clean_fixations(..., annotate = TRUE) to create one."
          ) else "",
          shiny::checkboxInput("show_removed_fixations",
                               "Show removed fixations", value = FALSE)
        ),
        shiny::checkboxInput("show_word_regions",
                             "Show word regions / boundaries", value = TRUE),
        shiny::div(
          id = "char_regions_container",
          title = if (!has_char_bounds) "Disabled: no character boundary data available" else "",
          shiny::checkboxInput("show_char_regions",
                               "Show character regions / boundaries", value = FALSE)
        ),
        shiny::div(
          id = "fp_landing_container",
          title = if (!has_char_bounds) paste0(
            "Disabled: requires character boundary data and first pass landing position. ",
            "Provide character boundaries and fixation data."
          ) else "",
          shiny::checkboxInput("show_fp_landing",
                               "Show first pass fixation position", value = FALSE)
        ),
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

    # -- handle missing character boundary data --------------------------------
    if (!has_char_bounds) {
      shinyjs::disable("show_char_regions")
      shinyjs::disable("show_fp_landing")
    }

    # -- handle missing 'removed' annotation in fixations ---------------------
    if (!has_removed_col) {
      shinyjs::disable("show_removed_fixations")
    }

    # -- dynamically enable/disable fp_landing based on per-trial data --------
    shiny::observe({
      if (has_char_bounds) {
        fp <- tryCatch(trial_fp_landing(), error = function(e) NULL)
        if (is.null(fp) || nrow(fp) == 0L) {
          shinyjs::disable("show_fp_landing")
        } else {
          shinyjs::enable("show_fp_landing")
        }
      }
    })

    # -- navigation buttons ---------------------------------------------------
    shiny::observeEvent(input$prevBtn, {
      choices <- trial_choices_current()
      idx <- match(input$trial_sel, choices)
      if (!is.na(idx) && idx > 1L) {
        shiny::updateSelectInput(session, "trial_sel", selected = choices[[idx - 1L]])
      }
    })
    shiny::observeEvent(input$nextBtn, {
      choices <- trial_choices_current()
      idx <- match(input$trial_sel, choices)
      if (!is.na(idx) && idx < length(choices)) {
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
      # Exclude removed fixations (from clean_fixations annotate=TRUE) for analysis
      if ("removed" %in% names(fix)) {
        fix <- dplyr::filter(fix, !.data$removed)
      }
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

    # Removed fixations for the current trial + eye (from clean_fixations annotate=TRUE)
    trial_removed_fixations <- shiny::reactive({
      if (!has_removed_col) return(NULL)
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
      fix <- dplyr::filter(fix, .data$removed == TRUE)
      if (nrow(fix) == 0L) return(NULL)
      if ("eye" %in% names(fix)) {
        fix <- dplyr::filter(fix, .data$eye == !!eye)
      }
      if (nrow(fix) == 0L) return(NULL)
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

    # Resolve character boundary ROI data for the current trial
    trial_cb <- shiny::reactive({
      tnr <- current_tnr()
      cb  <- NULL
      if (!is.null(chars)) {
        cb_in <- chars
        if (has_subject_selector && "subject" %in% names(cb_in)) {
          cb_in <- dplyr::filter(cb_in, .data$subject == current_subject())
        }
        cb <- if ("trial_nr" %in% names(cb_in)) {
          dplyr::filter(cb_in, .data$trial_nr == tnr)
        } else if ("trial" %in% names(cb_in)) {
          dplyr::filter(cb_in, .data$trial == tnr)
        } else {
          cb_in
        }
      }
      cb
    })

    # First pass landing position per word (for Feature 4/5)
    # Computed automatically when no measures tibble is supplied; uses the
    # measures tibble as override if it contains ffd_x / ffd_char columns.
    trial_fp_landing <- shiny::reactive({
      fix <- trial_fixations()
      wb  <- trial_wb()
      cb  <- trial_cb()

      if (is.null(fix) || nrow(fix) == 0L) return(NULL)
      if (is.null(wb) || nrow(wb) == 0L) return(NULL)
      if (!all(c("x_start", "x_end", "y_start", "y_end") %in% names(wb))) return(NULL)

      tnr <- current_tnr()

      # If the measures tibble has explicit landing position columns, use them.
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
        if (all(c("ffd_x", "ffd_char") %in% names(wm)) && nrow(wm) > 0L) {
          keep   <- intersect(c("word_id", "ffd_x", "ffd_y", "ffd_char"), names(wm))
          result <- wm[!is.na(wm$ffd_x), keep, drop = FALSE]
          if (nrow(result) > 0L) {
            names(result)[names(result) == "ffd_x"]   <- "fp_x"
            if ("ffd_y" %in% names(result))
              names(result)[names(result) == "ffd_y"] <- "fp_y"
            names(result)[names(result) == "ffd_char"] <- "fp_char_id"
            return(result)
          }
        }
      }

      # Auto-compute from fixations (Feature 4)
      fix_work <- fix
      if (!"trial_nr" %in% names(fix_work)) fix_work$trial_nr <- tnr
      wb_work  <- wb
      if (!"trial_nr" %in% names(wb_work)) wb_work$trial_nr <- tnr
      cb_work  <- cb
      if (!is.null(cb_work) && nrow(cb_work) > 0L &&
          !"trial_nr" %in% names(cb_work)) {
        cb_work$trial_nr <- tnr
      }

      tryCatch({
        landing   <- get_landing_info(fix_work, wb_work,
                                      character_boundaries = cb_work)
        first_fix <- landing[
          !is.na(landing$fixation_type) & landing$fixation_type == "first",
          , drop = FALSE
        ]
        if (nrow(first_fix) == 0L) return(NULL)
        keep_cols <- intersect(c("word_id", "avg_x", "avg_y", "fixated_char"),
                               names(first_fix))
        result <- first_fix[, keep_cols, drop = FALSE]
        names(result)[names(result) == "avg_x"]      <- "fp_x"
        if ("avg_y" %in% names(result))
          names(result)[names(result) == "avg_y"]    <- "fp_y"
        names(result)[names(result) == "fixated_char"] <- "fp_char_id"
        result
      }, error = function(e) NULL)
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
      samp <- trial_samples()
      fix  <- trial_fixations()
      wb   <- trial_wb()
      cb   <- trial_cb()

      # Optionally apply animation time limit
      if (!is.null(input$time_limit) && "time_rel" %in% names(samp)) {
        samp <- dplyr::filter(samp, .data$time_rel <= input$time_limit)
        if (!is.null(fix)) {
          fix <- dplyr::filter(fix, .data$time_rel_start <= input$time_limit)
          if (nrow(fix) == 0L) fix <- NULL
        }
      }

      # Removed fixations (for display when checkbox is ticked)
      removed_fix <- if (has_removed_col && isTRUE(input$show_removed_fixations)) {
        trial_removed_fixations()
      } else {
        NULL
      }
      if (!is.null(removed_fix) && !is.null(input$time_limit) &&
          "time_rel_start" %in% names(removed_fix)) {
        removed_fix <- dplyr::filter(removed_fix,
                                     .data$time_rel_start <= input$time_limit)
        if (nrow(removed_fix) == 0L) removed_fix <- NULL
      }

      has_full_roi <- !is.null(wb) && nrow(wb) > 0L &&
        all(c("x_start", "x_end", "y_start", "y_end") %in% names(wb))
      has_x_end    <- !is.null(wb) && nrow(wb) > 0L && "x_end" %in% names(wb)
      has_words    <- !is.null(wb) && nrow(wb) > 0L && "word" %in% names(wb)
      has_full_cb  <- !is.null(cb) && nrow(cb) > 0L &&
        all(c("x_start", "x_end", "y_start", "y_end") %in% names(cb))
      # When both region types are visible, word regions are drawn on top of
      # char regions (higher opacity) so it is clear which word each char belongs to.
      show_both_regions <- isTRUE(input$show_char_regions) && has_full_cb &&
        isTRUE(input$show_word_regions) && has_full_roi

      # Median y for label positioning
      y_median <- if (nrow(samp) > 0L) {
        stats::median(samp$y, na.rm = TRUE)
      } else if (has_full_roi) {
        mean(c(wb$y_start[[1L]], wb$y_end[[1L]]))
      } else {
        540
      }

      # --- Start Plotly Object ---
      p <- plotly::plot_ly(source = "gaze_plot")
      
      # Base layout
      p <- plotly::layout(
        p,
        title = if (has_subject_selector) {
          paste0("Subject ", current_subject(), " - Trial ", tnr, "  (eye: ", input$eye, ")")
        } else {
          paste0("Trial ", tnr, "  (eye: ", input$eye, ")")
        },
        xaxis = list(title = "Screen X (px)", showgrid = TRUE, zeroline = FALSE),
        yaxis = list(title = "Screen Y (px)", showgrid = TRUE, autorange = "reversed", zeroline = FALSE)
      )

      # Background image
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
          p <- plotly::layout(p, images = bg_layout_images)
        }
      }

      # Shapes are collected in separate lists then combined so that char
      # shapes (bottom) are drawn underneath word shapes (top), ensuring each
      # character is visually associated with its word when both are visible.
      char_shapes <- list()
      word_shapes <- list()
      bar_shapes  <- list()
      # invisible points for tooltips on ROIs and bars
      hover_x <- numeric()
      hover_y <- numeric()
      hover_txt <- character()

      # Character region rectangles / boundary lines (drawn first = underneath)
      if (isTRUE(input$show_char_regions) && !is.null(cb) && nrow(cb) > 0L) {
        has_cb_x_end <- "x_end" %in% names(cb)
        if (has_full_cb) {
          for (i in seq_len(nrow(cb))) {
            cb_text <- paste0(
              if ("char" %in% names(cb)) cb$char[[i]] else cb$char_id[[i]],
              " (word ", cb$word_id[[i]], ")"
            )
            char_shapes <- append(char_shapes, list(list(
              type = "rect",
              x0 = cb$x_start[[i]], x1 = cb$x_end[[i]],
              y0 = cb$y_start[[i]], y1 = cb$y_end[[i]],
              fillcolor = "rgba(255, 165, 0, 0.08)",
              line = list(color = "darkorange", width = 1.0),
              layer = "below"
            )))
            hover_x <- c(hover_x, (cb$x_start[[i]] + cb$x_end[[i]]) / 2)
            hover_y <- c(hover_y, (cb$y_start[[i]] + cb$y_end[[i]]) / 2)
            hover_txt <- c(hover_txt, cb_text)
          }
        } else if (has_cb_x_end) {
          for (i in seq_len(nrow(cb))) {
            char_shapes <- append(char_shapes, list(list(
              type = "line",
              x0 = cb$x_end[[i]], x1 = cb$x_end[[i]],
              y0 = 0, y1 = 1, yref = "paper",
              line = list(color = "darkorange", width = 1.0, dash = "dot"),
              layer = "below"
            )))
          }
        }
      }

      # Word region rectangles / boundary lines (drawn on top of char regions)
      if (isTRUE(input$show_word_regions) && !is.null(wb) && nrow(wb) > 0L) {
        # When both region types are shown, use a more visible fill so the
        # word colour is clearly overlaid on the character regions.
        word_fill       <- if (show_both_regions) "rgba(70, 130, 180, 0.22)" else "rgba(70, 130, 180, 0.07)"
        word_line_width <- if (show_both_regions) 0.8 else 0.3

        if (has_full_roi) {
          for (i in seq_len(nrow(wb))) {
            wb_text <- if (has_words) paste0("Word: ", wb$word[[i]]) else paste0("Region: ", wb$word_id[[i]])
            word_shapes <- append(word_shapes, list(list(
              type = "rect",
              x0 = wb$x_start[[i]], x1 = wb$x_end[[i]],
              y0 = wb$y_start[[i]], y1 = wb$y_end[[i]],
              fillcolor = word_fill,
              line = list(color = "steelblue", width = word_line_width),
              layer = "below"
            )))
            hover_x <- c(hover_x, (wb$x_start[[i]] + wb$x_end[[i]]) / 2)
            hover_y <- c(hover_y, (wb$y_start[[i]] + wb$y_end[[i]]) / 2)
            hover_txt <- c(hover_txt, wb_text)
          }
        } else if (has_x_end) {
          for (i in seq_len(nrow(wb))) {
            word_shapes <- append(word_shapes, list(list(
              type = "line",
              x0 = wb$x_end[[i]], x1 = wb$x_end[[i]],
              y0 = 0, y1 = 1, yref = "paper",
              line = list(color = "gray", width = 1, dash = "dash"),
              layer = "below"
            )))
          }
        }
      }

      # Word text labels
      if (isTRUE(input$show_word_labels) && has_words) {
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
        # When character regions are also shown, shift word labels slightly above
        # the center of the word region (~20% of word height) so they sit above
        # the character letter labels without hiding them.
        if (isTRUE(input$show_char_regions) && has_full_roi) {
          word_height <- wb$y_end - wb$y_start
          label_y <- label_y - word_height * 0.20
        }
        p <- plotly::add_text(
          p, x = label_x, y = label_y, text = wb$word,
          textfont = list(color = "gray20", size = 11),
          hoverinfo = "none", showlegend = FALSE
        )
      }

      # Character letter labels (one per char region when char boundaries shown)
      if (isTRUE(input$show_char_regions) && has_full_cb && "char" %in% names(cb)) {
        char_label_x <- (cb$x_start + cb$x_end) / 2
        char_label_y <- (cb$y_start + cb$y_end) / 2
        p <- plotly::add_text(
          p, x = char_label_x, y = char_label_y,
          text = as.character(cb$char),
          textfont = list(color = "darkorange", size = 8),
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

              # FFD
              if (!is.na(m_df$ffd[[i]])) {
                bx0 <- x0 + gap
                bx1 <- x0 + bw
                by0 <- y1 - m_df$ffd[[i]] * bar_scale
                by1 <- y1
                bar_shapes <- append(bar_shapes, list(list(
                  type = "rect", x0 = bx0, x1 = bx1, y0 = by0, y1 = by1,
                  fillcolor = "rgba(46, 204, 113, 0.6)",
                  line = list(color = "#27ae60", width = 0.3)
                )))
                hover_x <- c(hover_x, (bx0 + bx1) / 2)
                hover_y <- c(hover_y, by0)
                hover_txt <- c(hover_txt, paste0("FFD: ", round(m_df$ffd[[i]]), " ms"))
              }
              # GD
              if (!is.na(m_df$gd[[i]])) {
                bx0 <- x0 + bw + gap * 2
                bx1 <- x0 + bw * 2 + gap
                by0 <- y1 - m_df$gd[[i]] * bar_scale
                by1 <- y1
                bar_shapes <- append(bar_shapes, list(list(
                  type = "rect", x0 = bx0, x1 = bx1, y0 = by0, y1 = by1,
                  fillcolor = "rgba(155, 89, 182, 0.6)",
                  line = list(color = "#8e44ad", width = 0.3)
                )))
                hover_x <- c(hover_x, (bx0 + bx1) / 2)
                hover_y <- c(hover_y, by0)
                hover_txt <- c(hover_txt, paste0("GD: ", round(m_df$gd[[i]]), " ms"))
              }
              # TVT
              if (!is.na(m_df$tvt[[i]])) {
                bx0 <- x0 + bw * 2 + gap * 3
                bx1 <- x0 + bw * 3 + gap * 2
                by0 <- y1 - m_df$tvt[[i]] * bar_scale
                by1 <- y1
                bar_shapes <- append(bar_shapes, list(list(
                  type = "rect", x0 = bx0, x1 = bx1, y0 = by0, y1 = by1,
                  fillcolor = "rgba(52, 152, 219, 0.6)",
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

      # Combine shapes: char (bottom) → word (middle) → bars (top within "below" layer)
      shapes <- c(char_shapes, word_shapes, bar_shapes)
      p <- plotly::layout(p, shapes = shapes)

      # Add dummy scatter trace for shapes tooltips
      if (length(hover_x) > 0) {
        p <- plotly::add_markers(
          p, x = hover_x, y = hover_y, text = hover_txt,
          hoverinfo = "text", showlegend = FALSE,
          marker = list(opacity = 0, size = 1)
        )
      }

      # Raw gaze samples using scattergl
      if (input$show_samples && nrow(samp) > 0L) {
        # Using add_trace scattergl makes a massive performance difference
        p <- plotly::add_trace(
          p, data = samp,
          x = ~x, y = ~y,
          type = "scattergl",
          mode = "lines+markers",
          line = list(color = "rgba(100, 100, 100, 0.5)", width = 0.5),
          marker = list(color = "rgba(100, 100, 100, 0.5)", size = 2),
          text = ~paste0(
            "t: ", round(time_rel), " ms<br>",
            "X: ", round(x), "<br>",
            "Y: ", round(y)
          ),
          hoverinfo = "text",
          name = "Samples"
        )
      }

      # Helper: snap a vector of avg_y values to 20% above the vertical
      # centre of their nearest word ROI row (or fall back to y_median - 20 px).
      .fix_display_y <- function(avg_y_vec) {
        if (has_full_roi && nrow(wb) > 0L) {
          sapply(avg_y_vec, function(fy) {
            match_idx <- which(fy >= wb$y_start & fy <= wb$y_end)
            row <- if (length(match_idx) > 0L) {
              wb[match_idx[1L], , drop = FALSE]
            } else {
              dists <- pmin(abs(fy - wb$y_start), abs(fy - wb$y_end))
              wb[which.min(dists), , drop = FALSE]
            }
            center_y <- (row$y_start + row$y_end) / 2
            word_h   <- row$y_end - row$y_start
            center_y - word_h * 0.20
          })
        } else {
          rep(y_median - 20, length(avg_y_vec))
        }
      }

      # Fixation path + circles
      if (input$show_fixations && !is.null(fix) && nrow(fix) > 0L) {

        # Normalised display-y: slightly above the word-line centre for clarity
        fix_display_y <- .fix_display_y(fix$avg_y)

        # Line Path connecting fixations
        p <- plotly::add_trace(
          p, data = fix,
          x = ~avg_x, y = fix_display_y,
          type = "scatter",
          mode = "lines",
          line = list(color = "rgba(255, 0, 0, 0.5)", width = 1, dash = "dash"),
          hoverinfo = "none",
          name = "Fixation Path"
        )
        
        # Tooltip content setup
        hover_txt_fix <- paste0(
          "Fix #", fix$fixation_nr, "<br>",
          "Dur: ", round(fix$duration), " ms<br>",
          "Start: ", round(fix$time_rel_start), " ms<br>",
          "X: ", round(fix$avg_x), "<br>",
          "Y: ", round(fix$avg_y)
        )

        # Include rich HTML tooltips containing calculated ROI measures if user provided measures
        # and regions align. "rich HTML tooltips containing your calculated ROI measures."
        if (!is.null(wb) && has_full_roi && nrow(wb) > 0) {
          # Calculate which word each fixation fell into
          # Quick point in polygon / bbox match
          associated_roi <- sapply(seq_len(nrow(fix)), function(i) {
            fx <- fix$avg_x[i]
            fy <- fix$avg_y[i]
            match_idx <- which(fx >= wb$x_start & fx <= wb$x_end & fy >= wb$y_start & fy <= wb$y_end)
            if (length(match_idx) > 0) {
              if (has_words) wb$word[match_idx[1]] else wb$word_id[match_idx[1]]
            } else {
              NA
            }
          })
          
          # Append ROI info to tooltip
          hover_txt_fix <- ifelse(
            is.na(associated_roi),
            hover_txt_fix,
            paste0(hover_txt_fix, "<br><b>Assigned ROI:</b> ", associated_roi)
          )
        }
        
        # Scaling duration to marker size
        # Setting a min size of 5 and max of 30 just for visual clarity
        dur_min <- min(fix$duration, na.rm=TRUE)
        dur_max <- max(fix$duration, na.rm=TRUE)
        if (dur_max > dur_min) {
           marker_sizes <- 5 + 25 * ((fix$duration - dur_min) / (dur_max - dur_min))
        } else {
           marker_sizes <- rep(10, nrow(fix))
        }

        # Scatter points for Fixations
        p <- plotly::add_trace(
          p, data = fix,
          x = ~avg_x, y = fix_display_y,
          type = "scatter",
          mode = "markers",
          marker = list(
            color = "rgba(255, 0, 0, 0.65)",
            size = marker_sizes,
            line = list(color = "rgba(255, 0, 0, 0.8)", width = 1)
          ),
          text = hover_txt_fix,
          hoverinfo = "text",
          name = "Fixations"
        )
      }

      # Removed fixations (grey circles, size proportional to duration)
      if (!is.null(removed_fix) && nrow(removed_fix) > 0L) {
        reason_col <- if ("reason" %in% names(removed_fix)) {
          removed_fix$reason
        } else {
          rep(NA_character_, nrow(removed_fix))
        }
        hover_txt_removed <- paste0(
          "Removed Fix<br>",
          "Reason: ", ifelse(is.na(reason_col), "unknown", reason_col), "<br>",
          "Dur: ", round(removed_fix$duration), " ms<br>",
          "X: ", round(removed_fix$avg_x), "<br>",
          "Y: ", round(removed_fix$avg_y)
        )

        # Normalise display-y using the same helper as kept fixations
        removed_display_y <- .fix_display_y(removed_fix$avg_y)

        # Duration-proportional sizing (same scale as kept fixations)
        rdur_min <- min(removed_fix$duration, na.rm = TRUE)
        rdur_max <- max(removed_fix$duration, na.rm = TRUE)
        removed_sizes <- if (rdur_max > rdur_min) {
          5 + 25 * ((removed_fix$duration - rdur_min) / (rdur_max - rdur_min))
        } else {
          rep(10, nrow(removed_fix))
        }

        p <- plotly::add_trace(
          p, data = removed_fix,
          x = ~avg_x, y = removed_display_y,
          type = "scatter",
          mode = "markers",
          marker = list(
            color = "rgba(150, 150, 150, 0.55)",
            size = removed_sizes,
            symbol = "circle",
            line = list(color = "rgba(120, 120, 120, 0.8)", width = 1)
          ),
          text = hover_txt_removed,
          hoverinfo = "text",
          name = "Removed Fixations"
        )
      }

      # First pass landing position: downward triangle at the fixated character
      if (isTRUE(input$show_fp_landing) && has_full_cb) {
        fp <- trial_fp_landing()
        if (!is.null(fp) && nrow(fp) > 0L) {
          arrow_x   <- numeric(0)
          arrow_y   <- numeric(0)
          arrow_txt <- character(0)

          for (i in seq_len(nrow(fp))) {
            wid     <- fp$word_id[i]
            char_id <- if ("fp_char_id" %in% names(fp)) fp$fp_char_id[i] else NA_integer_
            if (is.na(wid)) next

            # Locate character bounding box
            cb_word <- cb[cb$word_id == wid, , drop = FALSE]
            if (nrow(cb_word) == 0L) next

            if (!is.na(char_id) && "char_id" %in% names(cb_word)) {
              cb_char <- cb_word[cb_word$char_id == char_id, , drop = FALSE]
            } else {
              cb_char <- data.frame()
            }
            if (nrow(cb_char) == 0L) cb_char <- cb_word[1L, , drop = FALSE]

            cx <- (cb_char$x_start[1L] + cb_char$x_end[1L]) / 2
            cy <- cb_char$y_start[1L]   # top of character region (visually above char)

            word_lbl <- if (has_words) {
              wrow <- wb[wb$word_id == wid, , drop = FALSE]
              if (nrow(wrow) > 0L) as.character(wrow$word[1L]) else as.character(wid)
            } else {
              as.character(wid)
            }
            arrow_x   <- c(arrow_x, cx)
            arrow_y   <- c(arrow_y, cy)
            arrow_txt <- c(arrow_txt, paste0(
              "First pass landing<br>",
              "Word: ", word_lbl, "<br>",
              "Char: ", if (!is.na(char_id)) char_id else "?"
            ))
          }

          if (length(arrow_x) > 0L) {
            p <- plotly::add_markers(
              p, x = arrow_x, y = arrow_y,
              marker = list(
                symbol = "triangle-down",
                size   = 14,
                color  = "rgba(0, 100, 220, 0.85)",
                line   = list(color = "rgba(0, 60, 160, 1)", width = 1.5)
              ),
              text      = arrow_txt,
              hoverinfo = "text",
              name      = "First Pass Landing",
              showlegend = FALSE
            )
          }
        }
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
      DT::datatable(
        tbl,
        rownames = FALSE,
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
      DT::datatable(
        tbl,
        rownames = FALSE,
        options  = list(pageLength = 25L, scrollX = TRUE),
        class    = "compact stripe hover"
      )
    })

    # -- raw samples table ----------------------------------------------------
    output$sample_dt <- DT::renderDT(server = FALSE, {
      shiny::req(input$show_sample_table)
      samp <- trial_samples()
      if (nrow(samp) == 0L) {
        return(DT::datatable(
          dplyr::tibble(message = "No samples found for this trial/eye."),
          rownames = FALSE
        ))
      }
      cols <- intersect(c("time", "time_rel", "x", "y"), names(samp))
      tbl  <- samp[, cols, drop = FALSE]
      tbl  <- dplyr::mutate(tbl, dplyr::across(
        dplyr::where(is.numeric), ~ round(.x, 1L)
      ))
      DT::datatable(
        tbl,
        rownames = FALSE,
        options  = list(pageLength = 25L, scrollX = TRUE),
        class    = "compact stripe hover"
      )
    })
  }

  # ---- launch app -----------------------------------------------------------
  app <- shiny::shinyApp(ui, server,
                         options = list(launch.browser = launch.browser))
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
  
  # Find first DF with trial info
  for (df in dfs) {
    t_col <- if ("trial_nr" %in% names(df)) "trial_nr" else if ("trial" %in% names(df)) "trial" else NULL
    if (!is.null(t_col)) {
      # Build mapping
      mapping_cols <- intersect(c("subject", t_col, "sentence_nr"), names(df))
      mapping <- unique(df[, mapping_cols, drop = FALSE])
      # Standardize column name
      if (t_col != "trial_nr") {
        names(mapping)[names(mapping) == t_col] <- "trial_nr"
      }
      
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
  
  # Fallback
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
    if ("trial_nr" %in% names(df)) {
      sort(unique(df$trial_nr))
    } else if ("trial" %in% names(df)) {
      sort(unique(df$trial))
    } else {
      NULL
    }
  })
  
  trial_lists <- trial_lists[!vapply(trial_lists, is.null, logical(1))]
  if (length(trial_lists) <= 1L) return()

  first_trials <- trial_lists[[1L]]
  for (i in seq_along(trial_lists)[-1L]) {
    if (!identical(trial_lists[[i]], first_trials)) {
      warning("The provided data frames (samples, fixations, rois, measures, trial_db) do not contain the exact same set of trials.")
      break
    }
  }
}
