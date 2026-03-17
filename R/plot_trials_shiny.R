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
#'   containing at minimum `samples` and `events`.  When the list also
#'   includes `word_boundaries` and/or `trial_db` those are used
#'   automatically.
#' @param roi A data frame of word regions of interest as returned by
#'   \code{\link{read_roi}}, or any compatible data frame with columns
#'   `word_id`, `x_start`, `x_end`, `y_start`, `y_end`.  When supplied it
#'   takes precedence over `asc_result$word_boundaries` for boundary and
#'   label overlays.  Pass `NULL` (the default) to use
#'   `asc_result$word_boundaries` instead.
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
#'   \item{Trial Number}{Select any trial by its `trial_nr` (or sequential
#'     index when no `trial_nr` column is present). Use the
#'     **Previous** / **Next** buttons to step through trials one by one.}
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
#' @importFrom dplyr filter mutate tibble
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
plot_trials_shiny <- function(asc_result, roi = NULL, launch.browser = TRUE) {

  # ---- dependency checks ----------------------------------------------------
  for (pkg in c("shiny", "plotly", "DT", "ggplot2")) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop(
        "Package '", pkg, "' is required for plot_trials_shiny(). ",
        "Install it with: install.packages('", pkg, "')"
      )
    }
  }

  stopifnot(is.list(asc_result))
  if (!all(c("samples", "events") %in% names(asc_result))) {
    stop("asc_result must contain 'samples' and 'events' elements.")
  }

  # ---- determine available trials -------------------------------------------
  trial_nrs <- .shiny_trial_nrs(asc_result)
  n_trials  <- length(trial_nrs)

  # ---- build UI ---------------------------------------------------------------
  ui <- shiny::fluidPage(
    shiny::titlePanel("Eye-Tracking Trial Visualisation"),
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        shiny::numericInput("trial_idx", "Trial index:",
                            value = 1L, min = 1L, max = n_trials, step = 1L),
        shiny::actionButton("prevBtn", "Previous"),
        shiny::actionButton("nextBtn", "Next"),
        shiny::hr(),
        shiny::selectInput("eye", "Eye to plot:", choices = c("R", "L"),
                           selected = "R"),
        shiny::hr(),
        shiny::checkboxInput("filter_display_on",
                             "Remove samples before display onset",
                             value = TRUE),
        shiny::checkboxInput("filter_display_off",
                             "Remove samples after display offset",
                             value = TRUE),
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

    # -- navigation buttons ---------------------------------------------------
    shiny::observeEvent(input$prevBtn, {
      v <- max(1L, input$trial_idx - 1L)
      shiny::updateNumericInput(session, "trial_idx", value = v)
    })
    shiny::observeEvent(input$nextBtn, {
      v <- min(n_trials, input$trial_idx + 1L)
      shiny::updateNumericInput(session, "trial_idx", value = v)
    })

    # -- reactive helpers -------------------------------------------------------

    # Current trial_nr (actual value from the data)
    current_tnr <- shiny::reactive({
      idx <- max(1L, min(n_trials, as.integer(input$trial_idx)))
      trial_nrs[[idx]]
    })

    # Resolve display window timestamps for the current trial
    display_times <- shiny::reactive({
      tnr  <- current_tnr()
      tdb  <- asc_result$trial_db
      t_on <- NA_real_
      t_off <- NA_real_
      if (!is.null(tdb)) {
        row <- if ("trial_nr" %in% names(tdb)) {
          tdb[tdb$trial_nr == tnr, , drop = FALSE]
        } else {
          # No trial_nr column: use the 1-based trial index as a row selector
          idx <- max(1L, min(nrow(tdb), as.integer(input$trial_idx)))
          tdb[idx, , drop = FALSE]
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
      samp  <- asc_result$samples
      if ("trial_nr" %in% names(samp)) {
        samp <- dplyr::filter(samp, .data$trial_nr == tnr)
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
      evts <- asc_result$events
      if ("trial_nr" %in% names(evts)) {
        evts <- dplyr::filter(evts, .data$trial_nr == tnr)
      }
      fix <- tryCatch(
        get_eyelink_fixations(evts),
        error = function(e) NULL
      )
      if (is.null(fix) || nrow(fix) == 0L) return(NULL)
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
      if (!is.null(roi)) {
        wb <- if ("trial_nr" %in% names(roi)) {
          dplyr::filter(roi, .data$trial_nr == tnr)
        } else {
          roi
        }
      } else if (!is.null(asc_result$word_boundaries)) {
        wb <- if ("trial_nr" %in% names(asc_result$word_boundaries)) {
          dplyr::filter(asc_result$word_boundaries, .data$trial_nr == tnr)
        } else {
          asc_result$word_boundaries
        }
      }
      wb
    })

    # Word measures (FFD, GD, TVT) – computed only when needed
    word_measures <- shiny::reactive({
      shiny::req(input$show_measures)
      wb  <- trial_wb()
      fix <- trial_fixations()
      tnr <- current_tnr()
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
          title = paste0("Trial ", tnr, "  (eye: ", input$eye, ")"),
          x = "Screen X (px)",
          y = "Screen Y (px)"
        )

      # Background image (encoded as base64 for plotly)
      # The image is sized to match the full display resolution. When the
      # image dimensions can be read, those are used; otherwise the common
      # 1920 × 1080 default is applied.
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
          # Determine image pixel dimensions for correct coordinate mapping
          bg_w <- 1920L  # default display width
          bg_h <- 1080L  # default display height
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
          # Build tooltip text outside aes() to keep the mapping readable
          wb_text <- if (has_words) {
            paste0("Word: ", wb$word)
          } else {
            paste0("Region: ", wb$word_id)
          }
          wb_plot <- dplyr::mutate(wb, .wb_tip = wb_text)
          p <- p + ggplot2::geom_rect(
            data = wb_plot,
            ggplot2::aes(
              xmin = .data$x_start, xmax = .data$x_end,
              ymin = .data$y_start, ymax = .data$y_end,
              text = .data$.wb_tip
            ),
            fill      = "steelblue",
            alpha     = 0.07,
            color     = "steelblue",
            linewidth = 0.3
          )
        } else if (has_x_end) {
          p <- p + ggplot2::geom_vline(
            data     = wb,
            ggplot2::aes(xintercept = .data$x_end),
            linetype = "dashed",
            color    = "grey50",
            alpha    = 0.7
          )
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
        label_df <- dplyr::tibble(x = label_x, y = label_y, label = wb$word)
        p <- p + ggplot2::geom_text(
          data = label_df,
          ggplot2::aes(x = .data$x, y = .data$y, label = .data$label),
          vjust = 0.5,
          size  = 3.5,
          color = "grey20"
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
            # bar_scale: pixels of bar height per millisecond of fixation time
            bar_scale <- 0.5

            if (any(!is.na(m_df$ffd))) {
              ffd_df <- m_df[!is.na(m_df$ffd), , drop = FALSE]
              bw_f   <- (ffd_df$x_end - ffd_df$x_start) * 0.25
              gap_f  <- bw_f * 0.08
              p <- p + ggplot2::geom_rect(
                data = ffd_df,
                ggplot2::aes(
                  xmin = .data$x_start + gap_f,
                  xmax = .data$x_start + bw_f,
                  ymin = .data$y_start - .data$ffd * bar_scale,
                  ymax = .data$y_start,
                  text = paste0("FFD: ", round(.data$ffd), " ms")
                ),
                fill = "#2ecc71", alpha = 0.6, color = "#27ae60", linewidth = 0.3
              )
            }
            if (any(!is.na(m_df$gd))) {
              gd_df  <- m_df[!is.na(m_df$gd), , drop = FALSE]
              bw_g   <- (gd_df$x_end - gd_df$x_start) * 0.25
              gap_g  <- bw_g * 0.08
              p <- p + ggplot2::geom_rect(
                data = gd_df,
                ggplot2::aes(
                  xmin = .data$x_start + bw_g + gap_g * 2,
                  xmax = .data$x_start + bw_g * 2 + gap_g,
                  ymin = .data$y_start - .data$gd * bar_scale,
                  ymax = .data$y_start,
                  text = paste0("GD: ", round(.data$gd), " ms")
                ),
                fill = "#9b59b6", alpha = 0.6, color = "#8e44ad", linewidth = 0.3
              )
            }
            if (any(!is.na(m_df$tvt))) {
              tvt_df <- m_df[!is.na(m_df$tvt), , drop = FALSE]
              bw_t   <- (tvt_df$x_end - tvt_df$x_start) * 0.25
              gap_t  <- bw_t * 0.08
              p <- p + ggplot2::geom_rect(
                data = tvt_df,
                ggplot2::aes(
                  xmin = .data$x_start + bw_t * 2 + gap_t * 3,
                  xmax = .data$x_start + bw_t * 3 + gap_t * 2,
                  ymin = .data$y_start - .data$tvt * bar_scale,
                  ymax = .data$y_start,
                  text = paste0("TVT: ", round(.data$tvt), " ms")
                ),
                fill = "#3498db", alpha = 0.6, color = "#2980b9", linewidth = 0.3
              )
            }
          }
        }
      }

      # Raw gaze samples
      if (input$show_samples && nrow(samp) > 0L) {
        p <- p + ggplot2::geom_point(
          data = samp,
          ggplot2::aes(
            x    = .data$x,
            y    = .data$y,
            text = paste0(
              "t: ", round(.data$time_rel), " ms\n",
              "X: ", round(.data$x), "\n",
              "Y: ", round(.data$y)
            )
          ),
          color = "grey40",
          size  = 0.5,
          alpha = 0.5
        )
      }

      # Fixation path + circles
      if (input$show_fixations && !is.null(fix) && nrow(fix) > 0L) {
        p <- p +
          ggplot2::geom_path(
            data = fix,
            ggplot2::aes(x = .data$avg_x, y = .data$avg_y),
            color    = "red",
            linetype = "dashed",
            alpha    = 0.5
          ) +
          ggplot2::geom_point(
            data = fix,
            ggplot2::aes(
              x    = .data$avg_x,
              y    = .data$avg_y,
              size = .data$duration,
              text = paste0(
                "Fix #", .data$fixation_nr, "\n",
                "Dur: ", round(.data$duration), " ms\n",
                "Start: ", round(.data$time_rel_start), " ms\n",
                "X: ", round(.data$avg_x), "\n",
                "Y: ", round(.data$avg_y)
              )
            ),
            color = "red",
            alpha = 0.65
          ) +
          ggplot2::scale_size_continuous(name = "Duration (ms)", range = c(2, 12))
      }

      # --- convert to plotly -------------------------------------------------
      plt <- suppressWarnings(
        plotly::ggplotly(p, tooltip = "text") |>
          plotly::layout(
            yaxis = list(autorange = "reversed"),
            images = bg_layout_images
          ) |>
          plotly::config(displayModeBar = TRUE, displaylogo = FALSE)
      )
      plt
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

#' Extract the vector of trial_nr values from an asc_result list
#' @noRd
.shiny_trial_nrs <- function(asc_result) {
  if (!is.null(asc_result$trial_db) && "trial_nr" %in% names(asc_result$trial_db)) {
    return(asc_result$trial_db$trial_nr)
  }
  if ("trial_nr" %in% names(asc_result$samples)) {
    return(sort(unique(asc_result$samples$trial_nr)))
  }
  # Fallback: single trial numbered 0
  0L
}
