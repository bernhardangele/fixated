#' Mark Fixations Close to Blinks
#'
#' This function takes a data frame of fixations and a data frame of detected blinks and identifies which fixations are close in time to any of the detected blinks based on a specified time difference threshold.
#'
#' @param fixations A data frame containing fixation data.
#' @param blinks A data frame containing detected blink data.
#' @param t_diff_threshold Time difference threshold (in milliseconds) for identifying fixations close to blinks. How far away does the start or end of a fixation have to be from the start/end of a blink to be marked as close to a blink?
#'
#' @return A modified data frame of fixations with an additional column indicating whether each fixation is close to a blink.
#'
#' @import dplyr
#'
#' @export
mark_fixations_close_to_blinks <-
  function(fixations, blinks, t_diff_threshold = 100) {
    fixations_blinks <- fixations %>%
      rowwise() %>%
      mutate(near_blink = any(
        blink_overlap_or_near(t_start, t_end, blinks$t_start, blinks$t_end)
      ))
    return(fixations_blinks)
  }
