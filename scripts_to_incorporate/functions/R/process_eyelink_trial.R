process_eyelink_trial <- function(eyelink_fixations, eyelink_blinks, trial_words, trial_db_for_trial, use_eye = "R", min_duration_for_elimination = 800, max_duration_for_elimination_or_merge = 80, max_distance_for_merge = 12, sentence_start_x = 125){
  # process a single using the fixations detected by Eyelink and the information extracted from the ASC file
  # use only data from one eye (default: right) and exclude fixations before DISPLAY ON and after DISPLAY OFF
  fixations <- eyelink_fixations %>%  filter(eye == use_eye & t_start > trial_db_for_trial$timestamp_display_on & t_end < trial_db_for_trial$timestamp_display_off)
  
  # if there are blinks, filter them too
  fixations$near_blink <- FALSE
  
  # if there are no blinks, the blinks list just contains NA, which is not a tibble
  if(is_tibble(eyelink_blinks) & length(eyelink_blinks) > 0){  
    blinks <- eyelink_blinks %>% filter(eye == use_eye & t_start > trial_db_for_trial$timestamp_display_on & t_end < trial_db_for_trial$timestamp_display_off)
    if(length(blinks > 0)){
      fixations <- mark_fixations_close_to_blinks(fixations, blinks, t_diff_threshold = 50) 
    }
  }
  
  # short fixations < 80 ms and < 100 ms near blinks should be eliminated. Ones < 80 ms not near blinks may be merged with preceding or subsequent fixations if they are less than one character (12 px) away
  fixations <- fixations %>% mutate(too_short = duration < max_duration_for_elimination_or_merge, too_long = duration > min_duration_for_elimination)
  
  merged_fixations <- merge_fixations(fixations, max_duration = max_duration_for_elimination_or_merge, max_distance = 12, rate = as.numeric(trial_db_for_trial$recording_sampling_rate))
  
  # for diagnostics -- is tidyverse function the same as loop?
  #merged_fixations_loop <- merge_fixations_loop(fixations, max_duration = max_duration_for_elimination_or_merge, max_distance = 12, rate = as.numeric(trial_db_for_trial$recording_sampling_rate))
  
  #if(!identical(merged_fixations, merge_fixations_loop(fixations, max_duration = max_duration_for_elimination_or_merge, max_distance = 12))){
  #  warning("Merged fixations not identical!")
  #}
  
  if(nrow(merged_fixations) > 0){
    merged_fixations_with_word_info <- assign_word_info(merged_fixations, trial_words, sentence_start_x)
    return(merged_fixations_with_word_info)
  }
  else{
    warning(paste("No fixations found for", use_eye, "eye, Subject", trial_db_for_trial$subject_nr, "Trial", rownames(trial_db_for_trial), "Sentence", trial_db_for_trial$sentence_number, ". Returning NULL"))
    return(NULL)
  }
}
