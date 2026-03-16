library(tidyverse)
library(stringr)
library(stringi)
library(saccades)
library(future)
library(furrr)
library(qs2)

options(warn=1)

functions_sources = list.files(path = "functions/R/",full.names = TRUE)
functions_loaded <- sapply(functions_sources, source)

data_files_250 <- get_files("data/whole_exp/rate_250/")

data_files_500 <- get_files("data/whole_exp/rate_500/")

data_files_1000 <- get_files("data/whole_exp/rate_1000/")

data_files_2000 <- get_files("data/whole_exp/rate_2000/")


plan(multisession(workers = 8))

subjects_250 <- data_files_250 %>% 
  future_map(~ read_asc_file(., rate = 250))

subjects_500 <- data_files_500 %>% 
  future_map(~ read_asc_file(., rate = 500))

subjects_1000 <- data_files_1000 %>% 
  future_map(~ read_asc_file(., rate = 1000))

subjects_2000 <- data_files_2000 %>% 
  future_map(~ read_asc_file(., rate = 2000))

# compressed file, slow
# save(subjects_250, subjects_500, subjects_1000, subjects_2000, file = "sampling_rate_subjects.RData")


# for faster saving and loading, don't compress the files
qs_save(subjects_250, file = "sampling_rate_subjects_250.qs")
qs_save(subjects_500, file = "sampling_rate_subjects_500.qs")
qs_save(subjects_1000, file = "sampling_rate_subjects_1000.qs")
qs_save(subjects_2000, file = "sampling_rate_subjects_2000.qs")
