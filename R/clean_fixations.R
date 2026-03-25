#' @title Clean and merge a fixation sequence
#'
#' @description
#' Post-processes a data frame of fixations (e.g. from `detect_fixations()` or
#' from `read_asc()`) by:
#'
#' 1. **Removing outliers** – fixations whose duration or spatial position
#'    fall outside user-specified bounds.
#' 2. **Merging adjacent fixations** – consecutive fixations (within the same
#'    trial and eye) that are separated by a short saccade and land near the
#'    same location are merged into a single fixation.  This corrects for
#'    "fixation splitting" caused by small blinks or noise.
#'
#' @param fixations A data frame of fixations with (at minimum) columns
#'   `start_time`, `end_time`, `duration`, `avg_x`, and `avg_y`.  May also
#'   contain `trial_nr` and `eye` columns.
#' @param min_duration Numeric scalar.  Fixations shorter than this value (ms)
#'   are removed *before* merging.  Defaults to `80`.
#' @param max_duration Numeric scalar.  Fixations longer than this value (ms)
#'   are removed.  Set to `Inf` to disable.  Defaults to `1200`.
#' @param x_bounds Numeric vector of length 2 `c(min, max)`.  Fixations
#'   outside these horizontal bounds (pixels) are removed.  Set to
#'   `c(-Inf, Inf)` to disable.  Defaults to `c(-Inf, Inf)`.
#' @param y_bounds Numeric vector of length 2 `c(min, max)`.  Fixations
#'   outside these vertical bounds (pixels) are removed.  Set to
#'   `c(-Inf, Inf)` to disable.  Defaults to `c(-Inf, Inf)`.
#' @param merge_distance Numeric scalar.  Maximum spatial distance (pixels,
#'   Euclidean) between two consecutive fixation centres for them to be
#'   candidates for merging.  Defaults to `40`.
#' @param merge_max_gap Numeric scalar.  Maximum temporal gap (ms) between
#'   `end_time` of fixation *i* and `start_time` of fixation *i+1* for
#'   them to be candidates for merging.  Defaults to `75`.
#' @param trial_col Character scalar or `NULL`.  Name of the column
#'   identifying trials.  Merging only occurs within trials.  Defaults to
#'   `"trial_nr"`.
#' @param eye_col Character scalar or `NULL`.  Name of the column identifying
#'   the recorded eye.  Merging only occurs within the same eye.  Defaults to
#'   `"eye"`.
#'
#' @param annotate Logical scalar.  If `FALSE` (default), only cleaned
#'   fixations are returned.  If `TRUE`, all original fixations are returned
#'   with two additional columns: `removed` (logical) indicating whether the
#'   fixation should be removed, and `reason` (character) indicating why
#'   (`"too short"`, `"too short and merged"`, `"too long"`, `"out of bounds"`,
#'   or `NA` if not removed).
#'
#' @return A [tibble][tibble::tibble] of cleaned fixations with the same
#'   columns as `fixations`, sorted by `start_time` within each group.
#'   The `duration` column is recomputed as `end_time - start_time` after
#'   merging.  For merged fixations, `avg_x` and `avg_y` are
#'   duration-weighted means of the contributing fixations.
#'   When `annotate = TRUE`, two additional columns are included:
#'   `removed` (logical) and `reason` (character).
#'
#' @importFrom dplyr tibble mutate filter arrange bind_rows select
#' @importFrom rlang .data
#'
#' @export
#'
#' @examples
#' fixations <- dplyr::tibble(
#'   start_time = c(0L, 150L, 160L, 400L),
#'   end_time   = c(120L, 155L, 280L, 600L),
#'   duration   = c(120L, 5L, 120L, 200L),
#'   avg_x      = c(300, 305, 308, 700),
#'   avg_y      = c(400, 402, 401, 400)
#' )
#' clean <- clean_fixations(fixations, min_duration = 80, merge_distance = 40)
#' print(clean)
#'
#' # Annotation mode: see which fixations are removed and why
#' annotated <- clean_fixations(fixations, min_duration = 80, merge_distance = 40,
#'                              annotate = TRUE)
#' print(annotated)
clean_fixations <- function(
    fixations,
    min_duration    = 80,
    max_duration    = 1200,
    x_bounds        = c(-Inf, Inf),
    y_bounds        = c(-Inf, Inf),
    merge_distance  = 40,
    merge_max_gap   = 75,
    trial_col       = "trial_nr",
    eye_col         = "eye",
    annotate        = FALSE
) {
  stopifnot(is.data.frame(fixations))
  required_cols <- c("start_time", "end_time", "duration", "avg_x", "avg_y")
  missing_cols  <- setdiff(required_cols, names(fixations))
  if (length(missing_cols) > 0L) {
    stop("fixations is missing required columns: ", paste(missing_cols, collapse = ", "))
  }
  stopifnot(length(x_bounds) == 2L, length(y_bounds) == 2L)

  # Smart fallback for trial column if default is not found
  if (identical(trial_col, "trial_nr") && !("trial_nr" %in% names(fixations)) && "trial" %in% names(fixations)) {
    trial_col <- "trial"
  } else if (identical(trial_col, "trial") && !("trial" %in% names(fixations)) && "trial_nr" %in% names(fixations)) {
    trial_col <- "trial_nr"
  }

  group_cols <- character(0)
  if (!is.null(trial_col) && trial_col %in% names(fixations)) {
    group_cols <- c(group_cols, trial_col)
  }
  if (!is.null(eye_col) && eye_col %in% names(fixations)) {
    group_cols <- c(group_cols, eye_col)
  }

  if (isTRUE(annotate)) {
    return(.clean_fixations_annotate(
      fixations, min_duration, max_duration, x_bounds, y_bounds,
      merge_distance, merge_max_gap, group_cols
    ))
  }

  # Step 1 – remove outliers
  out <- dplyr::filter(
    fixations,
    .data$duration >= min_duration,
    .data$duration <= max_duration,
    .data$avg_x >= x_bounds[[1]], .data$avg_x <= x_bounds[[2]],
    .data$avg_y >= y_bounds[[1]], .data$avg_y <= y_bounds[[2]]
  )

  if (nrow(out) == 0L) return(dplyr::as_tibble(fixations[0, , drop = FALSE]))

  # Step 2 – merge within groups
  if (length(group_cols) == 0L) {
    return(.merge_fixations(out, merge_distance, merge_max_gap))
  }

  group_keys <- unique(out[, group_cols, drop = FALSE])
  results    <- vector("list", nrow(group_keys))

  for (i in seq_len(nrow(group_keys))) {
    mask <- rep(TRUE, nrow(out))
    for (col in group_cols) {
      mask <- mask & (out[[col]] == group_keys[[col]][[i]])
    }
    merged <- .merge_fixations(out[mask, , drop = FALSE], merge_distance, merge_max_gap)
    results[[i]] <- merged
  }

  dplyr::bind_rows(results)
}

# ---------------------------------------------------------------------------
# Annotation mode helper
# ---------------------------------------------------------------------------

#' Annotate all fixations with removal status and reason
#' @noRd
.clean_fixations_annotate <- function(
    fixations, min_duration, max_duration, x_bounds, y_bounds,
    merge_distance, merge_max_gap, group_cols
) {
  n <- nrow(fixations)
  removed <- rep(FALSE, n)
  reason  <- rep(NA_character_, n)

  if (n == 0L) {
    fixations$removed <- logical(0)
    fixations$reason  <- character(0)
    return(dplyr::as_tibble(fixations))
  }

  # Mark removals by duration and bounds
  too_short <- fixations$duration < min_duration
  too_long  <- fixations$duration > max_duration
  out_x     <- fixations$avg_x < x_bounds[[1]] | fixations$avg_x > x_bounds[[2]]
  out_y     <- fixations$avg_y < y_bounds[[1]] | fixations$avg_y > y_bounds[[2]]

  removed[too_short] <- TRUE
  reason[too_short]  <- "too short"
  removed[too_long]  <- TRUE
  reason[too_long]   <- "too long"
  removed[out_x | out_y] <- TRUE
  reason[out_x | out_y]  <- "out of bounds"

  # Mark short fixations consumed during merging
  if (length(group_cols) == 0L) {
    merged_info <- .merge_fixations(fixations, merge_distance, merge_max_gap,
                                    min_duration = min_duration)
    removed[merged_info$removed_indices] <- TRUE
    reason[merged_info$removed_indices]  <- "too short and merged"
  } else {
    group_keys <- unique(fixations[, group_cols, drop = FALSE])
    for (i in seq_len(nrow(group_keys))) {
      mask <- rep(TRUE, n)
      for (col in group_cols) {
        mask <- mask & (fixations[[col]] == group_keys[[col]][[i]])
      }
      idx <- which(mask)
      merged_info <- .merge_fixations(fixations[idx, , drop = FALSE],
                                      merge_distance, merge_max_gap,
                                      min_duration = min_duration)
      removed[idx[merged_info$removed_indices]] <- TRUE
      reason[idx[merged_info$removed_indices]]  <- "too short and merged"
    }
  }

  fixations$removed <- removed
  fixations$reason  <- reason
  dplyr::as_tibble(fixations[order(fixations$start_time), , drop = FALSE])
}

# ---------------------------------------------------------------------------
# Internal merge helper
# ---------------------------------------------------------------------------

#' Merge adjacent fixations in a single (ungrouped) sequence
#' @noRd
.merge_fixations <- function(fixations, merge_distance, merge_max_gap,
                             min_duration = NULL) {
  track_removed <- !is.null(min_duration)
  removed_indices <- integer(0)

  if (nrow(fixations) == 0L) {
    if (track_removed) {
      return(list(fixations = dplyr::as_tibble(fixations),
                  removed_indices = removed_indices))
    }
    return(dplyr::as_tibble(fixations))
  }

  fix <- fixations[order(fixations$start_time), , drop = FALSE]
  n   <- nrow(fix)

  merged_list <- list()
  current     <- as.list(fix[1L, , drop = FALSE])
  current$duration <- as.integer(current$end_time - current$start_time)

  if (n > 1L) {
    for (i in seq(2L, n)) {
      candidate <- as.list(fix[i, , drop = FALSE])
      gap       <- as.numeric(candidate$start_time) - as.numeric(current$end_time)
      dist      <- sqrt(
        (candidate$avg_x - current$avg_x)^2 +
          (candidate$avg_y - current$avg_y)^2
      )

      if (gap <= merge_max_gap && dist <= merge_distance) {
        # Track short fixations consumed by merging
        if (track_removed && as.numeric(candidate$duration) < min_duration) {
          removed_indices <- c(removed_indices, i)
        }
        # Merge: weighted mean position, extend time span
        dur_cur  <- as.numeric(current$duration)
        dur_cand <- as.numeric(candidate$end_time - candidate$start_time)
        total    <- dur_cur + dur_cand
        current$avg_x    <- (current$avg_x    * dur_cur + candidate$avg_x    * dur_cand) / total
        current$avg_y    <- (current$avg_y    * dur_cur + candidate$avg_y    * dur_cand) / total
        current$end_time <- candidate$end_time
        current$duration <- as.integer(current$end_time - current$start_time)
        # Accumulate n_samples if present
        if (!is.null(current$n_samples) && !is.null(candidate$n_samples)) {
          current$n_samples <- as.integer(current$n_samples + candidate$n_samples)
        }
      } else {
        merged_list[[length(merged_list) + 1L]] <- current
        current <- candidate
        current$duration <- as.integer(current$end_time - current$start_time)
      }
    }
  }
  merged_list[[length(merged_list) + 1L]] <- current

  result <- dplyr::bind_rows(lapply(merged_list, dplyr::as_tibble))

  if (track_removed) {
    return(list(fixations = result, removed_indices = removed_indices))
  }
  result
}
