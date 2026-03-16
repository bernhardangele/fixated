#' Merge Fixations in a Data Frame
#'
#' This function merges fixations with a duration of less than \code{max_duration}
#' milliseconds with either the previous or the subsequent fixation, as long as that
#' fixation is less than \code{max_distance} pixels away. If there are two possibilities for merging,
#' the previous fixation is chosen.
#'
#' @param df A data frame containing fixation information with columns 'eye', 't_start',
#'   't_end', 'duration', 'x', 'y', and 'pupil'.
#' @param max_duration The maximum duration in milliseconds for a fixation to be considered
#'   for merging (default is 200 ms).
#' @param max_distance The maximum distance in pixels between fixations for them to be
#'   considered for merging (default is 50 pixels).
#'
#' @return A modified data frame with merged fixations and a new column 'merged' indicating
#'   whether a fixation was merged.
#'
#' @examples
#' fixations <- data.frame(
#'   eye = c("R", "R", "R", "R", "R"),
#'   t_start = c(5716010, 5716442, 5716566, 5717018, 5717202),
#'   t_end = c(5716378, 5716550, 5716986, 5717174, 5717398),
#'   duration = c(372, 112, 424, 160, 200),
#'   x = c(962, 202, 160, 294, 231),
#'   y = c(539, 554, 538, 555, 553),
#'   pupil = c(649, 651, 630, 622, 612)
#' )
#' merged_fixations <- merge_fixations(fixations)
#' print(merged_fixations)
#'
#' @export

merge_fixations <-
  function(df,
           max_duration = 80,
           max_distance = 12,
           rate = 1000,
           add_eyelink_correction = TRUE) {
    # apparently, eyelink adds half a sample duration to each side when calculating the duration
    # so since we are re-calculating, we need to add one sample duration (1000/rate ms)
    if(add_eyelink_correction){
      correction <- 1000/rate  
    } else {
      correction <- 0
    }
    
    df %>% mutate(x_prev = lag(x), y_prev = lag(y), x_next = lead(x), y_next = lead(y),
                  distance_to_prev = sqrt((x - x_prev) ^ 2 + (y - y_prev) ^2),
                  distance_to_next = sqrt((x - x_next) ^ 2 + (y - y_next) ^2),
                  merge_prev = duration < max_duration & distance_to_prev < max_distance,
                  # don't merge twice, so merge_next is always FALSE if merge_prev is true
                  merge_next = !merge_prev & duration < max_duration & distance_to_next < max_distance,
                  t_start_merged = t_start,
                  t_end_merged = t_end,
                  t_end_merged = ifelse(!is.na(lead(merge_prev)) & lead(merge_prev) == TRUE, lead(t_end_merged), t_end),
                  t_start_merged = ifelse(!is.na(lag(merge_next)) & lag(merge_next) == TRUE, lag(t_start_merged), t_start),
                  merged = (!is.na(merge_prev) & merge_prev) | (!is.na(merge_next) & merge_next),
                  t_start = t_start_merged,
                  t_end = t_end_merged,
                  
                  duration = t_end - t_start + correction) %>%
      dplyr::select(-x_prev, -y_prev, -x_next, -y_next, -distance_to_prev, -distance_to_next, -merge_prev, -merge_next, -t_start_merged, -t_end_merged)
  }
