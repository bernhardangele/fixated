add_manipulation_info_a <- function(fix_time_measures, trial_manipulation_info = manipulation_info){
  fix_time_measures_with_info <- fix_time_measures %>% left_join(trial_manipulation_info, by = c("sentence_nr" = "sentence_number"), suffix = c("", ".y")) %>% 
    filter(!is.na(sentence_nr)) # there are two trials with no fixations (only NAs -- remove)
  
  # fix trials where the conditions was miscoded
  fix_time_measures_with_info[fix_time_measures_with_info$frq.delta < 0, c("A1", "A2", "sentence_high", "sentence_low", "frq.A1", "frq.A2", "zipf.A1", "zipf.A2", "log_frqN.A1", "log_frqN.A2", "Q_high", "Q_low")] <- fix_time_measures_with_info[fix_time_measures_with_info$frq.delta < 0, c("A2", "A1", "sentence_low", "sentence_high", "frq.A2", "frq.A1", "zipf.A2", "zipf.A1", "log_frqN.A2", "log_frqN.A1", "Q_low", "Q_high")]
  fix_time_measures_with_info[fix_time_measures_with_info$frq.delta < 0, "cond"] <- ifelse(fix_time_measures_with_info[fix_time_measures_with_info$frq.delta < 0, "cond"] == "high", "low", "high")
  fix_time_measures_with_info <- fix_time_measures_with_info %>% mutate(frq.delta = frq.A2 - frq.A1)
  return(fix_time_measures_with_info)
}

add_manipulation_info_b <- function(fix_time_measures, trial_manipulation_info = manipulation_info){
  fix_time_measures_with_info <- fix_time_measures %>% left_join(trial_manipulation_info, by = c("sentence_nr" = "sentence_number"))
  
  # fix trials where the conditions was miscoded
  fix_time_measures_with_info[fix_time_measures_with_info$frq.delta < 0, c("A1", "A2", "sentence_high", "sentence_low", "frq.A1", "frq.A2", "zipf.A1", "zipf.A2", "log_frqN.A1", "log_frqN.A2", "Q_high", "Q_low")] <- fix_time_measures_with_info[fix_time_measures_with_info$frq.delta < 0, c("A2", "A1", "sentence_low", "sentence_high", "frq.A2", "frq.A1", "zipf.A2", "zipf.A1", "log_frqN.A2", "log_frqN.A1", "Q_low", "Q_high")]
  fix_time_measures_with_info[fix_time_measures_with_info$frq.delta < 0, "cond"] <- ifelse(fix_time_measures_with_info[fix_time_measures_with_info$frq.delta < 0, "cond"] == "high", "low", "high")
  fix_time_measures_with_info <- fix_time_measures_with_info %>% mutate(frq.delta = frq.A2 - frq.A1)
  return(fix_time_measures_with_info)
}
