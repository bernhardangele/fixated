#' Evaluate Blink Overlap or Proximity to Fixation
#'
#' This function evaluates whether a single fixation and a single blink are close in time, indicating an overlap or proximity between them based on a specified time difference threshold.
#'
#' @param fixation_start Start timestamp of the fixation in milliseconds.
#' @param fixation_end End timestamp of the fixation in milliseconds.
#' @param blink_start Start timestamp of the blink in milliseconds.
#' @param blink_end End timestamp of the blink in milliseconds.
#' @param t_diff_threshold Time difference threshold (in milliseconds) for considering overlap or proximity.
#'
#' @return Logical value indicating whether the fixation and blink overlap or are near each other in time.
#'
#' @examples
#' # Check if a fixation and a blink are close
#' fixation_near_blink <- blink_overlap_or_near(100, 180, 190, 220, t_diff_threshold = 100)
#'
#' @export
blink_overlap_or_near <- function(fixation_start,
                                  fixation_end,
                                  blink_start,
                                  blink_end,
                                  t_diff_threshold = 100) {
  # Function code here
}

blink_overlap_or_near <-
  function(fixation_start,
           fixation_end,
           blink_start,
           blink_end,
           t_diff_threshold = 100) {
    # tests whether a blink overlaps a fixation or starts/ends soon before/after one
    overlap <-
      fixation_end >= blink_start & blink_end >= fixation_start
    time_diff_start <- abs(fixation_start - blink_end)
    time_diff_end <- abs(fixation_end - blink_start)
    overlap |
      time_diff_start <= t_diff_threshold |
      time_diff_end <= t_diff_threshold
  }

