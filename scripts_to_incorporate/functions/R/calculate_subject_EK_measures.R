#' Calculate Eye-Tracking Measures for a Subject
#'
#' This function calculates various eye-tracking measures for a subject's experimental trials. It processes each trial's eye-tracking data, performs downsampling if required, detects fixations using the Engbert & Kliegl (2004) algorithm as implemented in the \code{saccades} package, and calculates fixation-related measures such as fixation time measures and other trial-specific information.
#'
#' @param subject A list containing subject-level data elements, usually created by the \code{read_asc_file} function.
#'   The list should include the following elements:
#'   
#'   \item{filename}{Character string representing the subject's filename or identifier.}
#'   \item{rate}{Character string specifying the recording sampling rate used in the experiment (e.g., "1000" for 1000 Hz).}
#'   \item{trial_db}{A data frame containing trial-level information for each trial in the experiment.}
#'   \item{trial_samples}{A list containing data frames, where each data frame represents eye-tracking samples for a trial.}
#'   \item{trial_words}{A list containing data frames, where each data frame holds word information for a trial.}
#'   \item{trial_fixations}{A list containing data frames, where each data frame contains fixation data for a trial (optional, used if \code{return_trial_fixations} is \code{TRUE}).}
#'   \item{trial_saccades}{A list containing data frames, where each data frame holds saccade information for a trial.}
#'   \item{trial_blinks}{A list containing data frames, where each data frame represents blink information for a trial.}
#'   
#'   These elements are used to process each trial's eye-tracking data and calculate fixation-related measures for analysis.
#' @param reduce_sampling_rate_to Numeric value specifying the target sampling rate after downsampling (Hz). \code{NULL} if no downsampling is desired.
#' @param downsampling_method Method used for downsampling. \code{drop} will keep one of every X samples (where X, the downsampling factor, is sampling rate/target sampling rate). \code{average} will average over every X samples.' 
#' @param lambda Lambda parameter for the Engbert & Kliegl (2004) fixation detection algorithm as implemented in the saccades package.
#' @param return_trial_fixations Logical indicating whether to return trial-specific fixation data.
#' @param use_eye Character indicating which eye to use for analysis ("L" for left, "R" for right).
#'
#' @return A data frame containing fixation time measures for each trial.
#' @return A list with fixation data frames for each trial (if \code{return_trial_fixations} is \code{TRUE}).
#'
#' @import dplyr
#'
#' @export

calculate_subject_EK_measures <-
  function(subject,
           reduce_sampling_rate_to = NULL,
           downsampling_method = "drop",
           lambda = 6,
           return_trial_fixations = FALSE,
           use_eye = "R") {
    cat("Subject: ", subject$filename, " ")
    trial_fixations = list()
    fixations_by_word = list()
    trial_db <- subject$trial_db
    original_sampling_rate <-
      as.numeric(trial_db$recording_sampling_rate)
    if (length(unique(original_sampling_rate)) > 1)
    {
      stop("Subject has trials with more than one sampling rate.")
    } else {
      original_sampling_rate <- original_sampling_rate[1]
    }
    
    downsampling_factor <- 1
    
    downsampling_factor <- determine_downsampling_factor(original_sampling_rate, reduce_sampling_rate_to, downsampling_method = downsampling_method)
    
    new_sampling_rate <-
      original_sampling_rate / downsampling_factor
    
    for (i in 1:nrow(trial_db)) {
      cat(i, " ")
      # do not process practice trials and those that finished before the stimulus presentation ended
      if (trial_db[i,]$trial_type == "experimental" &
          trial_db[i,]$has_display_off == TRUE) {
        trial_samples <- subject$trial_samples[[i]]
        
        modified_timestamps <- trial_samples$timestamp
        if (original_sampling_rate == 2000) {
          # in the 2000 Hz files, there are two timestamps per millisecond. Disambiguate them by adding .5 to the second one
          # this works if there are no breaks in recording
          duplicated_indices <-
            which(duplicated(modified_timestamps))
          
          # Add 0.5 to the second occurrence of each duplicated value
          modified_timestamps[duplicated_indices] <-
            modified_timestamps[duplicated_indices] + 0.5
        }
        trial_samples$timestamp <- modified_timestamps
        
        fixations_for_measures <-
          process_EK_trial(
            trial_samples = trial_samples,
            #eyelink_blinks = subject$trial_blinks[[i]],
            trial_words = subject$trial_words[[i]],
            trial_db_for_trial = trial_db[i,],
            lambda = 6,
            downsampling_factor = downsampling_factor,
            downsampling_method = downsampling_method,
            use_eye = use_eye,
          ) 
        
        if(!is.null(fixations_for_measures)){
        
          fixations_for_measures <- fixations_for_measures %>% mutate(subject = subject$filename)
          fixation_time_measures <-
            calculate_fixation_time_measures(fixations_for_measures, subject$trial_words[[i]])
          fixation_time_measures$subject <- trial_db[i,]$subject_nr
          fixation_time_measures$target_word_nr <-
            as.numeric(trial_db[i,]$target_word_nr) + 1 # python counts from 0
          fixation_time_measures$acceptability <-
            trial_db[i,]$response
          fixation_time_measures$rate <- new_sampling_rate
          fixation_time_measures$cond <- trial_db[i,]$cond
          trial_fixations[[i]] <- fixations_for_measures
          fixations_by_word[[i]] <- fixation_time_measures
          
        } else {
          warning(paste("No fixations found for", use_eye, "eye, Subject", trial_db[i,]$subject_nr, "Trial", rownames(trial_db[i,]), "Sentence",  trial_db[i,]$sentence_number, ". Returning NULL"))
          trial_fixations[[i]] <- NULL
          fixations_by_word[[i]] <- NULL
        }
      }
      else {
        trial_fixations[[i]] <- NULL
        fixations_by_word[[i]] <- NULL
      }
    }
    
    if (!return_trial_fixations) {
      return(bind_rows(fixations_by_word))
    } else {
      return(list(
        fixations_by_word = bind_rows(fixations_by_word),
        all_fixations = bind_rows(trial_fixations)))
    }
  }

