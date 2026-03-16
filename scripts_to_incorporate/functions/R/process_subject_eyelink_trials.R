process_subject_eyelink_trials <- function(subject){
  trial_db <- subject$trial_db
  eyelink_fixations_trials <- list()
  for(i in 1:nrow(trial_db)){
    cat(i, " ")
    # do not process practice trials and those that finished before the stimulus presentation ended
    if(trial_db[i,]$trial_type == "experimental" & trial_db[i,]$has_display_off == TRUE){
      eyelink_fixations_trials[[i]] <- process_eyelink_trial(eyelink_fixations = subject$trial_fixations[[i]],
                                                             eyelink_blinks = subject$trial_blinks[[i]],
                                                             trial_words = subject$trial_words[[i]],
                                                             trial_db_for_trial = trial_db[i,])
    }
  }
  return(eyelink_fixations_trials)
}