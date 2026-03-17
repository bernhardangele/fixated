#' @title Plot a single eye-tracking trial
#'
#' @description
#' Creates a \pkg{ggplot2} visualisation of eye-tracking data for a single
#' trial.  The plot can show raw gaze samples, EyeLink-detected fixations,
#' word boundary lines, word text labels, and word-level reading measures
#' (FFD, GD, TVT).  A background image (PNG or JPEG) may optionally be placed
#' behind the data for stimulus-layout verification.
#'
#' @param asc_result A named list as returned by \code{\link{read_asc}},
#'   containing at minimum `samples` and `events`.  When the list also
#'   includes `word_boundaries` and/or `trial_db` those are used
#'   automatically.
#' @param trial_nr Integer scalar.  Which trial to visualise.  Must match a
#'   value in the `trial_nr` column of `asc_result$samples` (added
#'   automatically by \code{\link{read_asc}} when trial structure is
#'   detected).
#' @param eye Character scalar.  Which eye's data to plot (`"L"` or `"R"`).
#'   Defaults to `"R"`.
#' @param show_samples Logical.  Plot raw gaze samples as semi-transparent
#'   grey points.  Defaults to `TRUE`.
#' @param show_fixations Logical.  Overlay EyeLink-detected fixations as
#'   red circles (size proportional to fixation duration) connected by a
#'   dashed path.  Defaults to `TRUE`.
#' @param show_word_boundaries Logical.  Draw dashed vertical lines at word
#'   right-boundary positions.  Requires either `asc_result$word_boundaries`
#'   or `roi` to be provided.  Defaults to `TRUE`.
#' @param show_word_labels Logical.  Print word text labels at the horizontal
#'   centre of each word region.  Requires `asc_result$word_boundaries` or
#'   `roi` to be provided, and both must contain a `word` column.  Defaults
#'   to `TRUE`.
#' @param show_measures Logical.  Annotate each word with its FFD, GD, and
#'   TVT values in milliseconds.  Requires `roi` (a full ROI table with
#'   `x_start`, `x_end`, `y_start`, `y_end` columns) or
#'   `asc_result$word_boundaries` with those columns present.  Defaults to
#'   `FALSE`.
#' @param roi A data frame of word regions of interest as returned by
#'   \code{\link{read_roi}}, or any compatible data frame with columns
#'   `word_id`, `x_start`, `x_end`, `y_start`, `y_end`.  When supplied it
#'   takes precedence over `asc_result$word_boundaries` for boundary, label,
#'   and measure overlays.  Pass `NULL` (the default) to use
#'   `asc_result$word_boundaries` instead.
#' @param x_limits Numeric vector of length 2 giving the horizontal axis
#'   range in pixels.  When `NULL` (default) the range is inferred from the
#'   data with a small margin.
#' @param y_limits Numeric vector of length 2 `c(min, max)` giving the
#'   vertical axis range in pixels.  When `NULL` (default) the range is
#'   inferred from the data with a small margin.  The y-axis is always
#'   displayed in reversed order (origin at top-left) to match the EyeLink
#'   coordinate convention.
#' @param bg_image_path Character scalar.  Path to a PNG or JPEG file to
#'   render as a background image (for stimulus-layout verification).
#'   Requires the \pkg{png} package for PNG files or the \pkg{jpeg} package
#'   for JPEG files.  Defaults to `NULL` (no background image).
#' @param display_window_only Logical.  When `TRUE` (default) and
#'   `asc_result$trial_db` contains `t_display_on` / `t_display_off`
#'   columns, samples and fixations outside the display window are removed
#'   before plotting.
#'
#' @return A \pkg{ggplot2} [ggplot][ggplot2::ggplot] object.  Print it,
#'   pass it to \code{ggplot2::ggsave()}, or add further layers with `+`.
#'
#' @details
#' \subsection{Word boundaries}{
#'   The function looks for ROI / boundary data in the following order:
#'   \enumerate{
#'     \item The explicit `roi` argument (when non-`NULL`).
#'     \item `asc_result$word_boundaries`, which is automatically populated
#'       by \code{\link{read_asc}} from OpenSesame `TRIAL … RIGHT_BOUNDARY`
#'       messages.
#'   }
#'   When the available data contains `x_start`, `x_end`, `y_start`, and
#'   `y_end` columns a shaded rectangle is drawn for each word region.  When
#'   only `x_end` is available, only vertical boundary lines are drawn.
#' }
#' \subsection{Word measures}{
#'   FFD, GD, and TVT are computed internally by calling
#'   \code{\link{compute_eye_measures}} on the EyeLink-detected fixations and
#'   the word ROI.  The measure values are displayed only when
#'   `show_measures = TRUE` and a full ROI (with all four boundary columns)
#'   is available.
#' }
#' \subsection{Background image}{
#'   The image is drawn as a raster layer beneath all other plot elements via
#'   \code{ggplot2::annotation_raster()}.  It spans the full `x_limits` ×
#'   `y_limits` extent, so the image should represent the full stimulus
#'   display at the experiment resolution.
#' }
#'
#' @importFrom rlang .data
#' @importFrom dplyr filter mutate left_join tibble bind_rows
#'
#' @export
#'
#' @examples
#' asc_file <- system.file("extdata", "sub_1_example.asc", package = "fixated")
#' if (requireNamespace("ggplot2", quietly = TRUE) && file.exists(asc_file)) {
#'   result <- read_asc(asc_file)
#'   p <- plot_trial(result, trial_nr = 0L)
#'   print(p)
#' }
plot_trial <- function(
    asc_result,
    trial_nr,
    eye                  = "R",
    show_samples         = TRUE,
    show_fixations       = TRUE,
    show_word_boundaries = TRUE,
    show_word_labels     = TRUE,
    show_measures        = FALSE,
    roi                  = NULL,
    x_limits             = NULL,
    y_limits             = NULL,
    bg_image_path        = NULL,
    display_window_only  = TRUE
) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop(
      "Package 'ggplot2' is required for plot_trial(). ",
      "Install it with: install.packages('ggplot2')"
    )
  }
  stopifnot(is.list(asc_result))
  if (!all(c("samples", "events") %in% names(asc_result))) {
    stop("asc_result must contain 'samples' and 'events' elements.")
  }

  tnr <- as.integer(trial_nr)
  eye <- match.arg(eye, c("L", "R"))

  # ---- extract samples for this trial ---------------------------------------
  samples <- asc_result$samples
  if ("trial_nr" %in% names(samples)) {
    samples <- dplyr::filter(samples, .data$trial_nr == tnr)
  }
  samples <- dplyr::filter(samples, .data$eye == !!eye)

  if (nrow(samples) == 0L) {
    warning("No samples found for trial ", tnr, " eye '", eye, "'.")
  }

  # ---- trim to display window ------------------------------------------------
  t_display_on  <- NA_real_
  t_display_off <- NA_real_
  if (display_window_only && !is.null(asc_result$trial_db)) {
    tdb <- asc_result$trial_db
    if ("trial_nr" %in% names(tdb)) {
      row <- tdb[tdb$trial_nr == tnr, , drop = FALSE]
    } else {
      row <- tdb[min(tnr + 1L, nrow(tdb)), , drop = FALSE]
    }
    if (nrow(row) > 0L) {
      if ("t_display_on" %in% names(row))  t_display_on  <- as.numeric(row$t_display_on[[1L]])
      if ("t_display_off" %in% names(row)) t_display_off <- as.numeric(row$t_display_off[[1L]])
    }
    if (!is.na(t_display_on))  samples <- dplyr::filter(samples, .data$time >= t_display_on)
    if (!is.na(t_display_off)) samples <- dplyr::filter(samples, .data$time <= t_display_off)
  }

  # ---- time relative to display-on ------------------------------------------
  t0 <- if (!is.na(t_display_on)) {
    t_display_on
  } else if (nrow(samples) > 0L) {
    min(samples$time, na.rm = TRUE)
  } else {
    0
  }
  if (nrow(samples) > 0L) {
    samples$time_rel <- samples$time - t0
  }

  # ---- extract fixations for this trial -------------------------------------
  fixations <- NULL
  if (show_fixations || show_measures) {
    events_trial <- asc_result$events
    if ("trial_nr" %in% names(events_trial)) {
      events_trial <- dplyr::filter(events_trial, .data$trial_nr == tnr)
    }
    fix_all <- get_eyelink_fixations(events_trial)
    fix_all <- dplyr::filter(fix_all, .data$eye == !!eye)
    if (!is.na(t_display_on))  fix_all <- dplyr::filter(fix_all, .data$start_time >= t_display_on)
    if (!is.na(t_display_off)) fix_all <- dplyr::filter(fix_all, .data$end_time   <= t_display_off)

    if (nrow(fix_all) > 0L) {
      fix_all$time_rel_start <- fix_all$start_time - t0
      fixations <- fix_all
    }
  }

  # ---- resolve ROI / word-boundary data ------------------------------------
  wb <- NULL  # working word-region data for this trial
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

  # Determine whether a full ROI (all four boundary columns) is available
  has_full_roi <- !is.null(wb) && nrow(wb) > 0L &&
    all(c("x_start", "x_end", "y_start", "y_end") %in% names(wb))
  has_x_end    <- !is.null(wb) && nrow(wb) > 0L && "x_end" %in% names(wb)

  # ---- compute word measures ------------------------------------------------
  word_measures <- NULL
  if (show_measures && has_full_roi && !is.null(fixations) && nrow(fixations) > 0L) {
    # Build fixation table in the format compute_eye_measures expects
    fix_for_meas <- fixations
    if (!"trial_nr" %in% names(fix_for_meas)) fix_for_meas$trial_nr <- tnr

    roi_for_meas <- wb
    if (!"trial_nr" %in% names(roi_for_meas)) roi_for_meas$trial_nr <- tnr

    word_measures <- tryCatch(
      compute_eye_measures(
        fix_for_meas,
        roi_for_meas,
        trial_col = "trial_nr",
        eye_col   = NULL
      ),
      error = function(e) {
        warning("Could not compute word measures: ", conditionMessage(e))
        NULL
      }
    )
  }

  # ---- y position for labels (median of gaze samples) ----------------------
  y_median <- if (nrow(samples) > 0L) {
    stats::median(samples$y, na.rm = TRUE)
  } else if (has_full_roi) {
    mean(c(wb$y_start[[1L]], wb$y_end[[1L]]))
  } else {
    540
  }

  # ---- determine axis limits ------------------------------------------------
  if (is.null(x_limits)) {
    all_x <- c(
      if (nrow(samples)   > 0L)          samples$x,
      if (!is.null(fixations))            fixations$avg_x,
      if (has_x_end)                      wb$x_end
    )
    all_x  <- all_x[is.finite(all_x)]
    x_limits <- if (length(all_x) > 0L) {
      rng <- range(all_x)
      c(max(0, rng[[1L]] - 50), rng[[2L]] + 50)
    } else {
      c(0, 1920)
    }
  }

  if (is.null(y_limits)) {
    all_y <- c(
      if (nrow(samples) > 0L) samples$y,
      if (!is.null(fixations)) fixations$avg_y
    )
    all_y  <- all_y[is.finite(all_y)]
    y_limits <- if (length(all_y) > 0L) {
      rng <- range(all_y)
      c(max(0, rng[[1L]] - 50), rng[[2L]] + 50)
    } else {
      c(0, 1080)
    }
  }

  # ---- build the plot -------------------------------------------------------
  p <- ggplot2::ggplot() +
    ggplot2::scale_x_continuous(
      name   = "Screen X (px)",
      limits = x_limits,
      expand = ggplot2::expansion(0)
    ) +
    ggplot2::scale_y_reverse(
      name   = "Screen Y (px)",
      limits = y_limits,
      expand = ggplot2::expansion(0)
    ) +
    ggplot2::theme_minimal() +
    ggplot2::labs(title = paste("Trial", tnr))

  # ---- background image -----------------------------------------------------
  if (!is.null(bg_image_path)) {
    raster_img <- .load_raster_image(bg_image_path)
    if (!is.null(raster_img)) {
      p <- p + ggplot2::annotation_raster(
        raster_img,
        xmin = x_limits[[1L]], xmax = x_limits[[2L]],
        ymin = y_limits[[1L]], ymax = y_limits[[2L]]
      )
    }
  }

  # ---- word region rectangles -----------------------------------------------
  if (has_full_roi) {
    p <- p + ggplot2::geom_rect(
      data = wb,
      ggplot2::aes(
        xmin = .data$x_start, xmax = .data$x_end,
        ymin = .data$y_start, ymax = .data$y_end
      ),
      fill      = "steelblue",
      alpha     = 0.05,
      color     = "steelblue",
      linewidth = 0.3
    )
  }

  # ---- word boundary vertical lines -----------------------------------------
  if (show_word_boundaries && has_x_end) {
    p <- p + ggplot2::geom_vline(
      data     = wb,
      ggplot2::aes(xintercept = .data$x_end),
      linetype = "dashed",
      color    = "grey50",
      alpha    = 0.7
    )
  }

  # ---- word text labels -----------------------------------------------------
  if (show_word_labels && !is.null(wb) && nrow(wb) > 0L && "word" %in% names(wb)) {
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
      size  = 3,
      color = "grey20"
    )
  }

  # ---- word measure annotations (FFD / GD / TVT) ----------------------------
  if (show_measures && !is.null(word_measures) && nrow(word_measures) > 0L &&
      !is.null(wb) && "word_id" %in% names(wb)) {

    m_df <- dplyr::left_join(
      wb,
      word_measures[, intersect(names(word_measures), c("word_id", "ffd", "gd", "tvt"))],
      by = "word_id"
    )
    m_df <- m_df[!is.na(m_df$ffd) | !is.na(m_df$gd) | !is.na(m_df$tvt), , drop = FALSE]

    if (nrow(m_df) > 0L) {
      cx <- if (has_full_roi && "x_start" %in% names(m_df)) {
        (m_df$x_start + m_df$x_end) / 2
      } else if ("x_end" %in% names(m_df)) {
        m_df$x_end - 20
      } else {
        rep(0, nrow(m_df))
      }
      # Place labels above the word region (smaller y value = higher on screen)
      label_top <- if (has_full_roi && "y_start" %in% names(m_df)) {
        m_df$y_start - 15
      } else {
        rep(y_median - 80, nrow(m_df))
      }

      make_label <- function(ffd, gd, tvt) {
        parts <- character(0)
        if (!is.na(ffd)) parts <- c(parts, paste0("FFD:", round(ffd), " ms"))
        if (!is.na(gd))  parts <- c(parts, paste0("GD:",  round(gd),  " ms"))
        if (!is.na(tvt)) parts <- c(parts, paste0("TVT:", round(tvt), " ms"))
        paste(parts, collapse = "\n")
      }

      labels <- mapply(
        make_label,
        ffd = if ("ffd" %in% names(m_df)) m_df$ffd else rep(NA_real_, nrow(m_df)),
        gd  = if ("gd"  %in% names(m_df)) m_df$gd  else rep(NA_real_, nrow(m_df)),
        tvt = if ("tvt" %in% names(m_df)) m_df$tvt else rep(NA_real_, nrow(m_df)),
        SIMPLIFY = TRUE
      )

      measure_df <- dplyr::tibble(x = cx, y = label_top, label = labels)
      p <- p + ggplot2::geom_text(
        data = measure_df,
        ggplot2::aes(x = .data$x, y = .data$y, label = .data$label),
        vjust      = 1,
        size       = 2.5,
        color      = "grey30",
        lineheight = 0.9
      )
    }
  }

  # ---- raw gaze samples -----------------------------------------------------
  if (show_samples && nrow(samples) > 0L) {
    p <- p + ggplot2::geom_point(
      data = samples,
      ggplot2::aes(x = .data$x, y = .data$y),
      color = "grey40",
      size  = 0.4,
      alpha = 0.5
    )
  }

  # ---- fixations ------------------------------------------------------------
  if (show_fixations && !is.null(fixations) && nrow(fixations) > 0L) {
    p <- p +
      ggplot2::geom_path(
        data = fixations,
        ggplot2::aes(x = .data$avg_x, y = .data$avg_y),
        color    = "red",
        alpha    = 0.5,
        linetype = "dashed"
      ) +
      ggplot2::geom_point(
        data = fixations,
        ggplot2::aes(x = .data$avg_x, y = .data$avg_y, size = .data$duration),
        color = "red",
        alpha = 0.5
      ) +
      ggplot2::scale_size_continuous(name = "Duration (ms)", range = c(1, 8))
  }

  p
}


# ---------------------------------------------------------------------------
# Internal helper: load a PNG / JPEG image as an R raster object
# ---------------------------------------------------------------------------

#' Load a PNG or JPEG file as an R raster for use with annotation_raster()
#' @noRd
.load_raster_image <- function(path) {
  stopifnot(is.character(path), length(path) == 1L)
  if (!file.exists(path)) {
    warning("Background image file not found: ", path)
    return(NULL)
  }
  ext <- tolower(tools::file_ext(path))
  if (ext == "png") {
    if (!requireNamespace("png", quietly = TRUE)) {
      warning(
        "Package 'png' is required to load PNG background images. ",
        "Install it with: install.packages('png')"
      )
      return(NULL)
    }
    img <- png::readPNG(path)
  } else if (ext %in% c("jpg", "jpeg")) {
    if (!requireNamespace("jpeg", quietly = TRUE)) {
      warning(
        "Package 'jpeg' is required to load JPEG background images. ",
        "Install it with: install.packages('jpeg')"
      )
      return(NULL)
    }
    img <- jpeg::readJPEG(path)
  } else {
    warning("Unsupported image format '", ext, "'. Use PNG or JPEG.")
    return(NULL)
  }
  grDevices::as.raster(img)
}
