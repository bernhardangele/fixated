options(warn=1)
packages= c( "MASS", "tidyverse", "dplyr", "ggpubr", "lme4", "readr","stringr","performance", "furrr") 


for(i in 1:length(packages)){
  
  if(packages[i] %in% rownames(installed.packages())==FALSE){
    install.packages(packages[i])
    library(packages[i], character.only=TRUE)
  }else{
    library(packages[i], character.only=TRUE)
  }
}

source("functions_for_processing_fixations.R")

# compressed file -- loads slowly
# load(file = "sampling_rate_subjects.RData")


# for faster saving and loading, don't compress the files
subjects_250 <- readRDS(file = "sampling_rate_subjects_250.rds")
subjects_500 <- readRDS(file = "sampling_rate_subjects_500.rds")
subjects_1000 <- readRDS(file = "sampling_rate_subjects_1000.rds")
subjects_2000 <- readRDS(file = "sampling_rate_subjects_2000.rds")

#eyelink_trials <- bind_rows(subject$trial_fixations, .id = "trial")

manipulation_info <- read_csv("all_sentences_freq_delta.csv")

plan(multisession(workers = 6), gc = TRUE)

fix_time_measures_250 <- subjects_250 %>% future_map(~ calculate_subject_eyelink_measures(.)) %>% bind_rows() %>% add_manipulation_info_a()
fix_time_measures_500 <- subjects_500 %>% future_map(~ calculate_subject_eyelink_measures(.)) %>% bind_rows() %>% add_manipulation_info_a()
fix_time_measures_1000 <- subjects_1000 %>% future_map(~ calculate_subject_eyelink_measures(.)) %>% bind_rows() %>% add_manipulation_info_a()
fix_time_measures_2000 <- subjects_2000 %>% future_map(~ calculate_subject_eyelink_measures(.)) %>% bind_rows() %>% add_manipulation_info_a()

saveRDS(fix_time_measures_250, file = "eyelink_fixation_time_measures_250.rds")
saveRDS(fix_time_measures_500, file = "eyelink_fixation_time_measures_500.rds")
saveRDS(fix_time_measures_1000, file = "eyelink_fixation_time_measures_1000.rds")
saveRDS(fix_time_measures_2000, file = "eyelink_fixation_time_measures_2000.rds")

