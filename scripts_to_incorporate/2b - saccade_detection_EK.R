options(warn = 1)
packages = c("devtools",
             "R.utils",
             "MASS",
             "ggpubr",
             "lme4",
             "performance",
             "furrr",
             "saccades",
             "tidyverse")



#install_github("tmalsburg/saccades/saccades", dependencies=TRUE,force= T)

for (i in 1:length(packages)) {
  if (packages[i] %in% rownames(installed.packages()) == FALSE) {
    install.packages(packages[i])
    library(packages[i], character.only = TRUE)
  } else{
    library(packages[i], character.only = TRUE)
  }
}

# read functions for processing data
R.utils::sourceDirectory("functions/R/", modifiedOnly=TRUE);


# compressed file -- loads slowly
#load(file = "sampling_rate_subjects.RData")


# for faster saving and loading, don't compress the files
subjects_250 <- readRDS(file = "sampling_rate_subjects_250.rds")
subjects_500 <- readRDS(file = "sampling_rate_subjects_500.rds")
subjects_1000 <- readRDS(file = "sampling_rate_subjects_1000.rds")
subjects_2000 <- readRDS(file = "sampling_rate_subjects_2000.rds")

manipulation_info <- read_csv("all_sentences_freq_delta.csv")

plan(multisession(workers = 10), gc = TRUE)


# There are no valid R eye samples in the 2000 Hz condition for Subject 23, Trials 47 and 50
# Test that the functions work anyway
# calculate_subject_EK_measures(subjects_2000[[23]])

EK_fix_time_measures_250 <-
  subjects_250 %>% future_map(~ calculate_subject_EK_measures(., lambda = 6)) %>% bind_rows() %>% add_manipulation_info_b()
EK_fix_time_measures_500 <-
  subjects_500 %>% future_map(~ calculate_subject_EK_measures(., lambda = 6)) %>% bind_rows() %>% add_manipulation_info_b()
EK_fix_time_measures_1000 <-
  subjects_1000 %>% future_map(~ calculate_subject_EK_measures(., lambda = 6)) %>% bind_rows() %>% add_manipulation_info_b()
EK_fix_time_measures_2000 <-
  subjects_2000 %>% future_map(~ calculate_subject_EK_measures(., lambda = 6)) %>% bind_rows() %>% add_manipulation_info_b()

saveRDS(EK_fix_time_measures_250, file = "EK_fixation_time_measures_250.rds")
saveRDS(EK_fix_time_measures_500, file = "EK_fixation_time_measures_500.rds")
saveRDS(EK_fix_time_measures_1000, file = "EK_fixation_time_measures_1000.rds")
saveRDS(EK_fix_time_measures_2000, file = "EK_fixation_time_measures_2000.rds")


# simulate 125 Hz
# we will combine data from all sampling rates for this

# options(error = recover)

#calculate_subject_EK_measures(subjects_2000[[22]], lambda = 6, reduce_sampling_rate_to = 125, downsampling_method = "average")

EK_fix_time_measures_250_to_125 <-
  subjects_250 %>% future_map(~ calculate_subject_EK_measures(., lambda = 6, reduce_sampling_rate_to = 125)) %>% bind_rows() %>% add_manipulation_info_b()
EK_fix_time_measures_500_to_125 <-
  subjects_500 %>% future_map(~ calculate_subject_EK_measures(., lambda = 6, reduce_sampling_rate_to = 125)) %>% bind_rows() %>% add_manipulation_info_b()
EK_fix_time_measures_1000_to_125 <-
  subjects_1000 %>% future_map(~ calculate_subject_EK_measures(., lambda = 6, reduce_sampling_rate_to = 125)) %>% bind_rows() %>% add_manipulation_info_b()
EK_fix_time_measures_2000_to_125 <-
  subjects_2000 %>% future_map(~ calculate_subject_EK_measures(., lambda = 6, reduce_sampling_rate_to = 125)) %>% bind_rows() %>% add_manipulation_info_b()

saveRDS(
  rbind(
    EK_fix_time_measures_250_to_125,
    EK_fix_time_measures_500_to_125,
    EK_fix_time_measures_1000_to_125,
    EK_fix_time_measures_2000_to_125
  ),
  "EK_fixation_time_measures_125.rds"
)

# simulate 50 Hz
# we will combine data from all sampling rates for this

options(error = recover)

calculate_subject_EK_measures(subjects_250[[27]], lambda = 6, reduce_sampling_rate_to = 50)

EK_fix_time_measures_250_to_50 <-
  subjects_250 %>% future_map(~ calculate_subject_EK_measures(., lambda = 6, reduce_sampling_rate_to = 50)) %>% bind_rows() %>% add_manipulation_info_b()
EK_fix_time_measures_500_to_50 <-
  subjects_500 %>% future_map(~ calculate_subject_EK_measures(., lambda = 6, reduce_sampling_rate_to = 50)) %>% bind_rows() %>% add_manipulation_info_b()
EK_fix_time_measures_1000_to_50 <-
  subjects_1000 %>% future_map(~ calculate_subject_EK_measures(., lambda = 6, reduce_sampling_rate_to = 50)) %>% bind_rows() %>% add_manipulation_info_b()
EK_fix_time_measures_2000_to_50 <-
  subjects_2000 %>% future_map(~ calculate_subject_EK_measures(., lambda = 6, reduce_sampling_rate_to = 50)) %>% bind_rows() %>% add_manipulation_info_b()

saveRDS(
  rbind(
    EK_fix_time_measures_250_to_50,
    EK_fix_time_measures_500_to_50,
    EK_fix_time_measures_1000_to_50,
    EK_fix_time_measures_2000_to_50
  ),
  "EK_fixation_time_measures_50.rds"
)


# simulate 25 Hz
# we will combine data from all sampling rates for this

EK_fix_time_measures_250_to_30 <-
  subjects_250 %>% future_map(~ calculate_subject_EK_measures(., lambda = 6, reduce_sampling_rate_to = 30)) %>% bind_rows() %>% add_manipulation_info_b()
EK_fix_time_measures_500_to_30 <-
  subjects_500 %>% future_map(~ calculate_subject_EK_measures(., lambda = 6, reduce_sampling_rate_to = 30)) %>% bind_rows() %>% add_manipulation_info_b()
EK_fix_time_measures_1000_to_30 <-
  subjects_1000 %>% future_map(~ calculate_subject_EK_measures(., lambda = 6, reduce_sampling_rate_to = 30)) %>% bind_rows() %>% add_manipulation_info_b()
EK_fix_time_measures_2000_to_30 <-
  subjects_2000 %>% future_map(~ calculate_subject_EK_measures(., lambda = 6, reduce_sampling_rate_to = 30)) %>% bind_rows() %>% add_manipulation_info_b()

saveRDS(
  rbind(
    EK_fix_time_measures_250_to_30,
    EK_fix_time_measures_500_to_30,
    EK_fix_time_measures_1000_to_30,
    EK_fix_time_measures_2000_to_30
  ),
  "EK_fixation_time_measures_30.rds"
)


### downsample using average


# simulate 125 Hz
# we will combine data from all sampling rates for this

# options(error = recover)

#calculate_subject_EK_measures(subjects_2000[[22]], lambda = 6, reduce_sampling_rate_to = 125)

EK_fix_time_measures_250_to_125_average <-
  subjects_250 %>% future_map(~ calculate_subject_EK_measures(., lambda = 6, reduce_sampling_rate_to = 125, downsampling_method = "average")) %>% bind_rows() %>% add_manipulation_info_b()
EK_fix_time_measures_500_to_125_average <-
  subjects_500 %>% future_map(~ calculate_subject_EK_measures(., lambda = 6, reduce_sampling_rate_to = 125, downsampling_method = "average")) %>% bind_rows() %>% add_manipulation_info_b()
EK_fix_time_measures_1000_to_125_average <-
  subjects_1000 %>% future_map(~ calculate_subject_EK_measures(., lambda = 6, reduce_sampling_rate_to = 125, downsampling_method = "average")) %>% bind_rows() %>% add_manipulation_info_b()
EK_fix_time_measures_2000_to_125_average <-
  subjects_2000 %>% future_map(~ calculate_subject_EK_measures(., lambda = 6, reduce_sampling_rate_to = 125, downsampling_method = "average")) %>% bind_rows() %>% add_manipulation_info_b()

saveRDS(
  rbind(
    EK_fix_time_measures_250_to_125_average,
    EK_fix_time_measures_500_to_125_average,
    EK_fix_time_measures_1000_to_125_average,
    EK_fix_time_measures_2000_to_125_average
  ),
  "EK_fixation_time_measures_125_average.rds"
)

# simulate 50 Hz
# we will combine data from all sampling rates for this

options(error = recover)

# calculate_subject_EK_measures(subjects_250[[27]], lambda = 6, reduce_sampling_rate_to = 50)

EK_fix_time_measures_250_to_50_average <-
  subjects_250 %>% future_map(~ calculate_subject_EK_measures(., lambda = 6, reduce_sampling_rate_to = 50, downsampling_method = "average")) %>% bind_rows() %>% add_manipulation_info_b()
EK_fix_time_measures_500_to_50_average <-
  subjects_500 %>% future_map(~ calculate_subject_EK_measures(., lambda = 6, reduce_sampling_rate_to = 50, downsampling_method = "average")) %>% bind_rows() %>% add_manipulation_info_b()
EK_fix_time_measures_1000_to_50_average <-
  subjects_1000 %>% future_map(~ calculate_subject_EK_measures(., lambda = 6, reduce_sampling_rate_to = 50, downsampling_method = "average")) %>% bind_rows() %>% add_manipulation_info_b()
EK_fix_time_measures_2000_to_50_average <-
  subjects_2000 %>% future_map(~ calculate_subject_EK_measures(., lambda = 6, reduce_sampling_rate_to = 50, downsampling_method = "average")) %>% bind_rows() %>% add_manipulation_info_b()

saveRDS(
  rbind(
    EK_fix_time_measures_250_to_50_average,
    EK_fix_time_measures_500_to_50_average,
    EK_fix_time_measures_1000_to_50_average,
    EK_fix_time_measures_2000_to_50_average
  ),
  "EK_fixation_time_measures_50_average.rds"
)


# simulate 25 Hz
# we will combine data from all sampling rates for this

EK_fix_time_measures_250_to_30_average <-
  subjects_250 %>% future_map(~ calculate_subject_EK_measures(., lambda = 6, reduce_sampling_rate_to = 30, downsampling_method = "average")) %>% bind_rows() %>% add_manipulation_info_b()
EK_fix_time_measures_500_to_30_average <-
  subjects_500 %>% future_map(~ calculate_subject_EK_measures(., lambda = 6, reduce_sampling_rate_to = 30, downsampling_method = "average")) %>% bind_rows() %>% add_manipulation_info_b()
EK_fix_time_measures_1000_to_30_average <-
  subjects_1000 %>% future_map(~ calculate_subject_EK_measures(., lambda = 6, reduce_sampling_rate_to = 30, downsampling_method = "average")) %>% bind_rows() %>% add_manipulation_info_b()
EK_fix_time_measures_2000_to_30_average <-
  subjects_2000 %>% future_map(~ calculate_subject_EK_measures(., lambda = 6, reduce_sampling_rate_to = 30, downsampling_method = "average")) %>% bind_rows() %>% add_manipulation_info_b()

saveRDS(
  rbind(
    EK_fix_time_measures_250_to_30_average,
    EK_fix_time_measures_500_to_30_average,
    EK_fix_time_measures_1000_to_30_average,
    EK_fix_time_measures_2000_to_30_average
  ),
  "EK_fixation_time_measures_30_average.rds"
)

options(error=recover)
options(show.error.locations=TRUE)

#calculate_subject_EK_measures(subject, lambda = 6, reduce_sampling_rate_to = 30, downsampling_method = "average")

