#' Calculate Fixation Time Measures
#'
#' This function calculates fixation time measures based on a data frame of fixations and associated word information. The calculated measures include measures related to first pass reading, gaze duration, and total viewing time for each word.
#'
#' @param fixations A data frame containing fixation data.
#' @param words A data frame containing word information.
#'
#' @return A data frame containing fixation time measures for each word. The resulting data frame includes the following columns:
#' 
#' \describe{
#'   \item{word_nr}{Word number associated with the fixation.}
#'   \item{ffd}{First Fixation Duration (in milliseconds) - Duration of the first fixation on the word.}
#'   \item{gd}{Gaze Duration (in milliseconds) - Total duration of fixations (first fixation and refixations) on the word during first-pass.}
#'   \item{sfd}{Single Fixation Duration (in milliseconds) - If the first fixation duration is equal to gaze duration, then SFD is the same as FFD, otherwise NA.}
#'   \item{gopast}{Go-past time (in milliseconds) - Total time spent from first entering word from the left to leaving word to the right, including any regressions. Also known as regression path duration.}
#'   \item{tvt}{Total Viewing Time (in milliseconds) - Total time spent on the word during the entire trial.}
#'   \item{word}{Word text associated with the word number.}
#' }
#'
#' @import dplyr
#'
#' @export

calculate_fixation_time_measures <- function(fixations, words) {
  # Function code here
}

calculate_fixation_time_measures <- function(fixations, words) {
  fixations <-
    fixations %>% ungroup() %>% filter(!too_short &
                                         !too_long &
                                         !merged &
                                         !(duration < 100 &
                                             near_blink)) %>% mutate(word_max = cummax(word_nr),
                                                                     first_pass = categorize_fixation(word_nr))
  first_pass_measures <- fixations %>%
    filter(first_pass != "other fixation") %>%
    group_by(word_nr) %>%
    summarize(
      ffd = duration[1],
      gd = sum(duration),
      sfd = ifelse(ffd == gd, ffd, NA)
    )
  
  gopast <- fixations %>% group_by(word_max) %>%
    summarize(gopast = sum(duration))
  #mutate(word_nr = word_max)
  
  tvt <- fixations %>% group_by(word_nr) %>%
    summarize(tvt = sum(duration))
  
  words %>%
    left_join(first_pass_measures, join_by(word_nr)) %>%
    left_join(gopast, join_by(word_nr == word_max)) %>%
    left_join(tvt, join_by(word_nr)) %>% dplyr::select(-word_right_x_boundary)
}
