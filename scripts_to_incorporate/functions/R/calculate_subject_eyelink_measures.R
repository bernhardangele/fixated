calculate_subject_eyelink_measures <- function(subject){
  cat("Subject: ", subject$filename)
  trial_db <- subject$trial_db
  fixations_by_word <- list()
  for(i in 1:nrow(trial_db)){
    cat(i, " ")
    # do not process practice trials and those that finished before the stimulus presentation ended
    if(trial_db[i,]$trial_type == "experimental" & trial_db[i,]$has_display_off == TRUE){
      fixations_for_measures <- process_eyelink_trial(eyelink_fixations = subject$trial_fixations[[i]],
                                                      eyelink_blinks = subject$trial_blinks[[i]],
                                                      trial_words = subject$trial_words[[i]],
                                                      trial_db_for_trial = trial_db[i,])
      fixation_time_measures <- calculate_fixation_time_measures(fixations_for_measures, subject$trial_words[[i]])
      fixation_time_measures$subject <- trial_db[i,]$subject_nr
      fixation_time_measures$target_word_nr <- as.numeric(trial_db[i,]$target_word_nr) + 1 # python counts from 0
      fixation_time_measures$acceptability <- trial_db[i,]$response
      fixation_time_measures$rate <- trial_db[i,]$recording_sampling_rate
      fixation_time_measures$cond <- trial_db[i,]$cond
      fixations_by_word[[i]] <- fixation_time_measures}
    else {
      fixations_by_word[[i]] <- NULL}
  }
  return(bind_rows(fixations_by_word))
}
