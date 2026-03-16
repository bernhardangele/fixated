#' Evaluate Blink Overlap or Proximity to a Fixation
#'
#' Tests whether a fixation and a blink overlap in time or are within a
#' specified time-difference threshold of each other.  This is used to flag
#' fixations that may be contaminated by blink artifacts.
#'
#' @param fixation_start Numeric.  Start timestamp of the fixation (ms).
#' @param fixation_end Numeric.  End timestamp of the fixation (ms).
#' @param blink_start Numeric (scalar or vector).  Start timestamp(s) of the
#'   blink(s) (ms).
#' @param blink_end Numeric (scalar or vector).  End timestamp(s) of the
#'   blink(s) (ms).
#' @param t_diff_threshold Numeric scalar.  Time difference threshold (ms).
#'   A fixation is flagged when either its start is within this many ms of a
#'   blink end, or its end is within this many ms of a blink start.  Defaults
#'   to `100`.
#'
#' @return A logical vector of the same length as `blink_start` / `blink_end`,
#'   where `TRUE` means the corresponding blink overlaps or is near the
#'   fixation.
#'
#' @export
#'
#' @examples
#' # Fixation 100–200 ms, blink 190–240 ms (overlapping)
#' blink_overlap_or_near(100, 200, 190, 240)
#'
#' # Fixation 100–200 ms, blink 250–300 ms (50 ms gap, within threshold=100)
#' blink_overlap_or_near(100, 200, 250, 300, t_diff_threshold = 100)
#'
#' # Fixation 100–200 ms, blink 400–500 ms (not near)
#' blink_overlap_or_near(100, 200, 400, 500, t_diff_threshold = 100)
blink_overlap_or_near <- function(fixation_start,
                                  fixation_end,
                                  blink_start,
                                  blink_end,
                                  t_diff_threshold = 100) {
  overlap         <- fixation_end >= blink_start & blink_end >= fixation_start
  time_diff_start <- abs(fixation_start - blink_end)
  time_diff_end   <- abs(fixation_end   - blink_start)
  overlap | time_diff_start <= t_diff_threshold | time_diff_end <= t_diff_threshold
}
