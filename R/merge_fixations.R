#' Merge Short Fixations with Adjacent Fixations
#'
#' Merges fixations whose duration is below `max_duration` ms with either the
#' preceding or the subsequent fixation, provided the two fixations are no more
#' than `max_distance` pixels apart.  When both neighbours qualify, the
#' preceding fixation is preferred.
#'
#' This function is designed for fixation data in the EyeLink column-naming
#' convention (`t_start`, `t_end`, `x`, `y`, `duration`).  For the
#' tidier-API version that works with `start_time`, `end_time`, `avg_x`,
#' `avg_y`, see [clean_fixations()].
#'
#' @param df A data frame with (at minimum) columns `t_start`, `t_end`,
#'   `duration`, `x`, `y`.  Additional columns are passed through unchanged.
#' @param max_duration Numeric scalar.  Fixations shorter than this value (ms)
#'   are candidates for merging.  Defaults to `80`.
#' @param max_distance Numeric scalar.  Maximum Euclidean distance (pixels)
#'   between the centres of two adjacent fixations for them to be merged.
#'   Defaults to `12`.
#' @param rate Numeric scalar.  Eye-tracker sampling rate (Hz).  Used only
#'   when `add_eyelink_correction = TRUE` to add half a sample duration to
#'   the recomputed durations (EyeLink adds this correction internally).
#'   Defaults to `1000`.
#' @param add_eyelink_correction Logical.  When `TRUE`, adds `1000 / rate` ms
#'   to the recomputed duration of merged fixations to replicate EyeLink's
#'   internal timing convention.  Defaults to `TRUE`.
#'
#' @return A modified copy of `df` with merged fixations.  A logical column
#'   `merged` is added (or overwritten) indicating whether each fixation was
#'   involved in a merge.
#'
#' @importFrom dplyr mutate lag lead select
#'
#' @export
#'
#' @examples
#' fixations <- dplyr::tibble(
#'   t_start  = c(100L, 400L, 450L, 600L),
#'   t_end    = c(370L, 440L, 580L, 800L),
#'   duration = c(270L,  40L, 130L, 200L),
#'   x        = c(300,  310,  320,  600),
#'   y        = c(540,  541,  540,  540)
#' )
#' merge_fixations(fixations, max_duration = 80, max_distance = 20, rate = 1000,
#'                 add_eyelink_correction = FALSE)
merge_fixations <- function(df,
                            max_duration           = 80,
                            max_distance           = 12,
                            rate                   = 1000,
                            add_eyelink_correction = TRUE) {
  stopifnot(is.data.frame(df))
  required <- c("t_start", "t_end", "duration", "x", "y")
  missing  <- setdiff(required, names(df))
  if (length(missing) > 0L) {
    stop("df is missing required columns: ", paste(missing, collapse = ", "))
  }

  correction <- if (add_eyelink_correction) 1000 / rate else 0

  df |>
    dplyr::mutate(
      x_prev           = dplyr::lag(.data$x),
      y_prev           = dplyr::lag(.data$y),
      x_next           = dplyr::lead(.data$x),
      y_next           = dplyr::lead(.data$y),
      distance_to_prev = sqrt((.data$x - .data$x_prev)^2 +
                                (.data$y - .data$y_prev)^2),
      distance_to_next = sqrt((.data$x - .data$x_next)^2 +
                                (.data$y - .data$y_next)^2),
      merge_prev       = .data$duration < max_duration &
        .data$distance_to_prev < max_distance,
      # don't merge twice: merge_next is FALSE when merge_prev is TRUE
      merge_next       = !.data$merge_prev &
        .data$duration < max_duration &
        .data$distance_to_next < max_distance,
      t_start_merged   = .data$t_start,
      t_end_merged     = .data$t_end,
      t_end_merged     = ifelse(
        !is.na(dplyr::lead(.data$merge_prev)) & dplyr::lead(.data$merge_prev),
        dplyr::lead(.data$t_end_merged),
        .data$t_end
      ),
      t_start_merged   = ifelse(
        !is.na(dplyr::lag(.data$merge_next)) & dplyr::lag(.data$merge_next),
        dplyr::lag(.data$t_start_merged),
        .data$t_start
      ),
      merged           = (!is.na(.data$merge_prev) & .data$merge_prev) |
        (!is.na(.data$merge_next) & .data$merge_next),
      t_start          = .data$t_start_merged,
      t_end            = .data$t_end_merged,
      duration         = .data$t_end - .data$t_start + correction
    ) |>
    dplyr::select(
      -"x_prev", -"y_prev", -"x_next", -"y_next",
      -"distance_to_prev", -"distance_to_next",
      -"merge_prev", -"merge_next",
      -"t_start_merged", -"t_end_merged"
    )
}
