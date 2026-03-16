#' Process Eye-Tracking Trial Data
#'
#' This function processes eye-tracking trial data from a reading experiment. It performs various steps including sample filtering, downsampling, fixation detection (using the algorithm by Engbert & Kliegl, 2004, as implemented in the saccades package), fixation merging, and adding word information to the fixations.
#'
#' @param trial_samples A data frame containing eye-tracking samples for the trial.
#' @param trial_words A data frame containing word information for the trial.
#' @param trial_db_for_trial A data frame containing trial-level information for the trial.
#' @param use_eye Character indicating which eye to use for analysis ("L" for left, "R" for right).
#' @param min_duration_for_elimination Minimum duration (in milliseconds) for long fixations to be considered for elimination.
#' @param max_duration_for_elimination_or_merge Maximum duration (in milliseconds) for short fixations to be considered for elimination or merging.
#' @param max_distance_for_merge Maximum distance (in pixels) between fixations for merging.
#' @param sentence_start_x X-coordinate (in pixels) indicating the starting position of the sentence on the screen.
#' @param lambda Lambda parameter for fixation detection algorithm (see \code{\link[saccades]{detect.fixations}}.
#' @param smooth.coords Logical indicating whether to apply coordinate smoothing during fixation detection (see \code{\link[saccades]{detect.fixations}}.
#' @param smooth.saccades Logical indicating whether to apply saccade smoothing during fixation detection (see \code{\link[saccades]{detect.fixations}}.
#' @param downsampling_factor Downsampling factor for reducing the data. This is for evaluating the performance of the saccade detection algorithm with lower sampling rates.
#' @param downsampling_method What method to use to remove samples? Default is "drop": Keep every n-th sample. Alternative: "average": average over n samples (where n is the downsampling factor)
#'
#' @return A data frame containing fixations with added word information.
#'
#' The returned data frame contains the following columns:
#'
#' \describe{
#'   \item{trial}{Trial number of the eye-tracking data.}
#'   \item{start}{Timestamp of the fixation start in milliseconds.}
#'   \item{end}{Timestamp of the fixation end in milliseconds.}
#'   \item{x}{X-coordinate of the fixation point on the screen in pixels.}
#'   \item{y}{Y-coordinate of the fixation point on the screen in pixels.}
#'   \item{mad.x}{Median absolute deviation of x-coordinates during the fixation.}
#'   \item{mad.y}{Median absolute deviation of y-coordinates during the fixation.}
#'   \item{peak.vx}{Peak velocity in the x-direction during the fixation.}
#'   \item{peak.vy}{Peak velocity in the y-direction during the fixation.}
#'   \item{dur}{Duration of the fixation in milliseconds.}
#'   \item{event}{Type of eye movement event, e.g., "fixation".}
#'   \item{duration}{Duration of the fixation in milliseconds (duplicate of 'dur' column).}
#'   \item{t_start}{Timestamp of the fixation start in milliseconds (duplicate of 'start' column).}
#'   \item{t_end}{Timestamp of the fixation end in milliseconds (duplicate of 'end' column).}
#'   \item{too_short}{Logical indicating if the fixation duration is too short.}
#'   \item{too_long}{Logical indicating if the fixation duration is too long.}
#'   \item{near_blink}{Logical indicating if the fixation is near a blink region.}
#'   \item{merged}{Logical indicating if the fixation was merged with an adjacent fixation.}
#'   \item{word_nr}{Word number associated with the fixation (-1 for no word).}
#'   \item{word}{Word text fixated (NA if no word fixated).}
#'   \item{sentence_nr}{Sentence number associated with the fixation.}
#'   \item{trial_nr}{Trial number associated with the fixation.}
#'   \item{subject}{Subject identifier for the trial.}
#' }
#'
#' @import dplyr
#' @importFrom saccades detect.fixations merge_fixations assign_word_info
#'
#' @export

#'
#' @import dplyr
#' @importFrom saccades detect.fixations
#'
#' @examples
#' # Load necessary packages
#' library(dplyr)
#' library(saccades)
#'
#' # Process trial data
#' processed_data <- process_EK_trial(trial_samples, trial_words, trial_db_for_trial)
#'
#' @export
process_EK_trial <- function(trial_samples,
                             trial_words,
                             trial_db_for_trial,
                             use_eye = "R",
                             min_duration_for_elimination = 800,
                             max_duration_for_elimination_or_merge = 80,
                             max_distance_for_merge = 12,
                             sentence_start_x = 125,
                             lambda = 6,
                             smooth.coords = FALSE,
                             smooth.saccades = TRUE,
                             downsampling_factor = 1,
                             downsampling_method = "drop")
{
  samples <-
    trial_samples %>%  dplyr::filter(
      timestamp > trial_db_for_trial$timestamp_display_on &
        timestamp < trial_db_for_trial$timestamp_display_off
    ) %>%
    mutate(
      time = timestamp,
      trial = trial_db_for_trial$sentence_number,
      x = if (use_eye == "L") {
        x_l
      } else{
        x_r
      },
      y = if (use_eye == "L") {
        y_l
      } else{
        y_r
      }
    ) %>%
    dplyr::select(time, trial, x, y)
  
  if (downsampling_factor > 1) {
    # these are the indices of the samples we want to keep (e.g., every second, third, fourth, eighth, ... sample)
    if (downsampling_method == "drop") {
      selected_sample_indices <-
        seq(from = 1,
            to = nrow(samples),
            by = downsampling_factor)
      samples <- samples[selected_sample_indices, ]
    } else if(downsampling_method == "average"){
      samples <- samples %>%
        group_by(trial) %>%
        mutate(sample_group = ceiling(row_number() / downsampling_factor)) %>%
        group_by(trial, sample_group) %>%
        summarise(time = mean(time),
                  x = round(mean(x), 1),
                  y = round(mean(y), 1), .groups = "drop_last") %>%
        dplyr::select(time, trial, x, y)
    }
  }
  
  
  
  # don't try to detect fixations if there are not enough samples that are actually on the sentence
  # if there is an error with fixation detection turn it into a warning and return 0
  tryCatch({
    if(sum(!is.na(samples$x) & samples$x < 1700) > 8 & sum(!is.na(samples$y & samples$y > 900)) > 8){
      
      fixationsEK <-
        detect.fixations(
          samples,
          lambda = lambda,
          smooth.coordinates = smooth.coords,
          smooth.saccades = smooth.saccades
        ) %>%
        filter(!is.na(x)) %>% # remove fixations with no x value (these are blinks)
        # rename columns so it works with the other functions
        mutate(
          duration = dur,
          t_start = start,
          t_end = end,
          too_short = duration < max_duration_for_elimination_or_merge,
          too_long = duration > min_duration_for_elimination,
          near_blink = FALSE
        )
      
      merged_fixations <-
        merge_fixations(
          fixationsEK,
          max_duration = max_duration_for_elimination_or_merge,
          max_distance = 12,
          rate = as.numeric(trial_db_for_trial$recording_sampling_rate),
          add_eyelink_correction = FALSE
        )
      
      
      if(nrow(merged_fixations) > 0){
        merged_fixations_with_word_info <- assign_word_info(merged_fixations, trial_words, sentence_start_x)
        return(merged_fixations_with_word_info)
      }
      else{
        warning(paste("No fixations found for", use_eye, "eye, Subject", trial_db_for_trial$subject_nr, "Trial", rownames(trial_db_for_trial), "Sentence", trial_db_for_trial$sentence_number, ". Returning NULL"))
        return(NULL)
      }
      
    } else {
      warning(paste("No (or less than 9) valid samples found for", use_eye, "eye, Subject", trial_db_for_trial$subject_nr, "Trial", rownames(trial_db_for_trial), "Sentence", trial_db_for_trial$sentence_number, ". Returning NULL"))
      return(NULL)
    }
  }, error = function(e){
    warning("Error in fixation detection for ", paste(use_eye, "eye, Subject", trial_db_for_trial$subject_nr, "Trial", rownames(trial_db_for_trial), "Sentence", trial_db_for_trial$sentence_number), ". Returning NULL. Error message: ", e)
    return(NULL)
  })
  
}
