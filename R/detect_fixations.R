#' @title Detect fixations from raw gaze samples
#'
#' @description
#' Applies a **dispersion-threshold identification (I-DT)** algorithm to a
#' sequence of raw gaze samples and returns a data frame of detected
#' fixations.  The algorithm groups consecutive samples into a fixation
#' candidate whenever the spatial dispersion (max – min of x and y within a
#' sliding window) falls below `max_dispersion`, with the window containing
#' at least `min_duration` milliseconds of data.
#'
#' The I-DT algorithm was originally described in:
#' Salvucci, D. D., & Goldberg, J. H. (2000). Identifying fixations and
#' saccades in eye-tracking protocols.  *ETRA*, pp. 71–78.
#'
#' @param samples A data frame of raw gaze samples with (at minimum) columns:
#'   * `time`  – integer timestamp in milliseconds.
#'   * `x`     – horizontal gaze position in pixels.
#'   * `y`     – vertical gaze position in pixels.
#' @param min_duration Numeric scalar.  Minimum fixation duration in
#'   milliseconds.  Defaults to `100`.
#' @param max_dispersion Numeric scalar.  Maximum allowed dispersion (radius
#'   in pixels) within a fixation window; computed as half the sum of the
#'   x-range and y-range.  Defaults to `25`.
#' @param trial_col Character scalar or `NULL`.  Name of the column that
#'   identifies trials.  When supplied, fixation detection is run separately
#'   within each trial.  Defaults to `"trial"`.
#' @param eye_col Character scalar or `NULL`.  Name of the column that
#'   identifies which eye each sample belongs to.  When supplied, fixation
#'   detection is run separately for each eye.  Defaults to `"eye"`.
#'
#' @return A [tibble][tibble::tibble] with one row per detected fixation and
#'   columns:
#'   \describe{
#'     \item{`start_time`}{Integer.  Timestamp of the first sample in the
#'       fixation (ms).}
#'     \item{`end_time`}{Integer.  Timestamp of the last sample in the
#'       fixation (ms).}
#'     \item{`duration`}{Integer.  Fixation duration in ms
#'       (`end_time - start_time`).}
#'     \item{`avg_x`}{Double.  Mean x position during the fixation (px).}
#'     \item{`avg_y`}{Double.  Mean y position during the fixation (px).}
#'     \item{`n_samples`}{Integer.  Number of samples contributing to the
#'       fixation.}
#'   }
#'   If `trial_col` or `eye_col` are provided and present in `samples`, those
#'   columns are also included in the output.
#'
#' @importFrom dplyr tibble mutate filter group_by summarise bind_rows
#'   arrange select n
#' @importFrom rlang .data
#'
#' @export
#'
#' @examples
#' samples <- dplyr::tibble(
#'   time  = seq(0L, 990L, by = 10L),
#'   x     = c(rep(300, 60), rep(600, 40)) + stats::rnorm(100, 0, 2),
#'   y     = c(rep(400, 60), rep(400, 40)) + stats::rnorm(100, 0, 2)
#' )
#' fixations <- detect_fixations(samples, min_duration = 100, max_dispersion = 25)
#' print(fixations)
detect_fixations <- function(
    samples,
    min_duration   = 100,
    max_dispersion = 25,
    trial_col      = "trial",
    eye_col        = "eye"
) {
  stopifnot(is.data.frame(samples))
  required_cols <- c("time", "x", "y")
  missing_cols  <- setdiff(required_cols, names(samples))
  if (length(missing_cols) > 0L) {
    stop("samples is missing required columns: ", paste(missing_cols, collapse = ", "))
  }

  group_cols <- character(0)
  if (!is.null(trial_col) && trial_col %in% names(samples)) {
    group_cols <- c(group_cols, trial_col)
  }
  if (!is.null(eye_col) && eye_col %in% names(samples)) {
    group_cols <- c(group_cols, eye_col)
  }

  if (length(group_cols) == 0L) {
    return(.idt_fixations(samples, min_duration, max_dispersion))
  }

  # Split by group columns, run I-DT within each group, reassemble
  group_keys <- unique(samples[, group_cols, drop = FALSE])
  results    <- vector("list", nrow(group_keys))

  for (i in seq_len(nrow(group_keys))) {
    mask <- rep(TRUE, nrow(samples))
    for (col in group_cols) {
      mask <- mask & (samples[[col]] == group_keys[[col]][[i]])
    }
    subset_df <- samples[mask, , drop = FALSE]
    fix_df    <- .idt_fixations(subset_df, min_duration, max_dispersion)
    # Attach group key columns
    for (col in group_cols) {
      fix_df[[col]] <- group_keys[[col]][[i]]
    }
    results[[i]] <- fix_df
  }

  out <- dplyr::bind_rows(results)
  # Reorder columns: group cols first, then measure cols
  measure_cols <- c("start_time", "end_time", "duration", "avg_x", "avg_y", "n_samples")
  col_order    <- c(group_cols, measure_cols)
  col_order    <- col_order[col_order %in% names(out)]
  out[, col_order, drop = FALSE]
}

# ---------------------------------------------------------------------------
# Internal I-DT implementation
# ---------------------------------------------------------------------------

#' Run I-DT fixation detection on a single (ungrouped) sample sequence
#' @noRd
.idt_fixations <- function(samples, min_duration, max_dispersion) {
  samples <- samples[order(samples$time), , drop = FALSE]
  n       <- nrow(samples)

  if (n == 0L) return(.empty_fixations_tibble())

  time_vec <- samples$time
  x_vec    <- samples$x
  y_vec    <- samples$y

  fixation_list <- list()
  i <- 1L

  while (i <= n) {
    # Find minimum window with enough duration
    j <- i
    while (j <= n && (time_vec[[j]] - time_vec[[i]]) < min_duration) {
      j <- j + 1L
    }

    if (j > n) break  # can't form another window

    # Check dispersion of window [i, j]
    win_x <- x_vec[i:j]
    win_y <- y_vec[i:j]
    disp  <- .dispersion(win_x, win_y)

    if (disp > max_dispersion) {
      # Window exceeds dispersion – advance start pointer
      i <- i + 1L
    } else {
      # Expand window as long as dispersion stays within threshold
      while (j < n) {
        next_x <- c(win_x, x_vec[[j + 1L]])
        next_y <- c(win_y, y_vec[[j + 1L]])
        if (.dispersion(next_x, next_y) > max_dispersion) break
        j      <- j + 1L
        win_x  <- next_x
        win_y  <- next_y
      }

      # Record fixation
      fixation_list[[length(fixation_list) + 1L]] <- list(
        start_time = time_vec[[i]],
        end_time   = time_vec[[j]],
        duration   = as.integer(time_vec[[j]] - time_vec[[i]]),
        avg_x      = mean(win_x, na.rm = TRUE),
        avg_y      = mean(win_y, na.rm = TRUE),
        n_samples  = as.integer(j - i + 1L)
      )

      i <- j + 1L
    }
  }

  if (length(fixation_list) == 0L) return(.empty_fixations_tibble())

  dplyr::bind_rows(lapply(fixation_list, dplyr::as_tibble))
}

#' Compute dispersion (half of x-range + y-range) for a set of points
#' @noRd
.dispersion <- function(x, y) {
  (max(x, na.rm = TRUE) - min(x, na.rm = TRUE) +
     max(y, na.rm = TRUE) - min(y, na.rm = TRUE)) / 2
}

#' Empty fixations tibble prototype
#' @noRd
.empty_fixations_tibble <- function() {
  dplyr::tibble(
    start_time = integer(0),
    end_time   = integer(0),
    duration   = integer(0),
    avg_x      = numeric(0),
    avg_y      = numeric(0),
    n_samples  = integer(0)
  )
}
