library(tidyverse)
library(qs2)
library(furrr)
library(tictoc) # For timing

# Load the data
tic("Load Data")
subjects_250 <- qs_read("sampling_rate_subjects_250.qs")
subjects_500 <- qs_read("sampling_rate_subjects_500.qs")
subjects_1000 <- qs_read("sampling_rate_subjects_1000.qs")
subjects_2000 <- qs_read("sampling_rate_subjects_2000.qs")
toc()

plan(multisession(workers = 8))

# Function to extract subject ID from filename
extract_subject_id <- function(filename) {
  as.integer(str_extract(filename, "(?<=sub_)\\d+"))
}

# Calculate degrees of visual angle per pixel (calculate only once)
diagonal_inches <- 24
resolution_width <- 1920
resolution_height <- 1080
viewing_distance_cm <- 60
inches_to_cm <- 2.54
aspect_ratio <- resolution_width / resolution_height
height_inches <- sqrt(diagonal_inches^2 / (aspect_ratio^2 + 1))
width_inches <- aspect_ratio * height_inches
width_cm <- width_inches * inches_to_cm
pixel_width_cm <- width_cm / resolution_width
pixel_size_cm <- pixel_width_cm # Assuming square pixels
visual_angle_radians <- pixel_size_cm / viewing_distance_cm
visual_angle_degrees <- visual_angle_radians * (180 / pi)
deg_per_pix <- visual_angle_degrees

## Accuracy
# get calibration_results for each participant and put them in a data frame (with participant number and sampling rate)

get_calibration_results <- function(subject) {
  calibration_results <- subject$calibration_results
  # get subject number from filename, e.g. "data/whole_exp/rate_250//sub_1.asc"
  calibration_results$subject <- subject$filename %>% 
    str_extract("sub_\\d+") %>% 
    str_extract("\\d+") %>% 
    as.integer()
  return(calibration_results)
}

calibration_results_250 <- subjects_250 %>% 
  future_map(~ get_calibration_results(.)) %>% 
  bind_rows() %>% 
  mutate(rate = 250)

calibration_results_500 <- subjects_500 %>%
  future_map(~ get_calibration_results(.)) %>% 
  bind_rows() %>% 
  mutate(rate = 500)

calibration_results_1000 <- subjects_1000 %>%
  future_map(~ get_calibration_results(.)) %>% 
  bind_rows() %>% 
  mutate(rate = 1000)

calibration_results_2000 <- subjects_2000 %>%
  future_map(~ get_calibration_results(.)) %>% 
  bind_rows() %>% 
  mutate(rate = 2000)

# Combine the calibration results

calibration_results <- bind_rows(calibration_results_250, calibration_results_500, calibration_results_1000, calibration_results_2000)

# save calibration results

write_csv(calibration_results, "analysis/calibration_results.csv")

# Calculate the accuracy for each participant

accuracy_by_participant <- calibration_results %>% 
  group_by(rate, subject, eye) %>% 
  summarize(mean_avg_error = mean(avg_error),
            min_avg_error = min(avg_error),
            max_avg_error = max(avg_error),
            mean_max_error = mean(max_error),
            mean_offset = mean(offset_deg),
            mean_x_offset = mean(x_offset),
            mean_y_offset = mean(y_offset))

accuracy_by_rate <- calibration_results %>% 
  group_by(rate, eye) %>% 
  summarize(mean_avg_error = mean(avg_error),
            min_avg_error = min(avg_error),
            max_avg_error = max(avg_error),
            mean_max_error = mean(max_error),
            mean_offset = mean(offset_deg),
            mean_x_offset = mean(x_offset),
            mean_y_offset = mean(y_offset))

# Save the accuracy data

write_csv(accuracy_by_participant, "analysis/accuracy_by_participant.csv")

calculate_sample_distance_rms <- function(trial_data, deg_per_pix) {
  if (nrow(trial_data) < 2) {
    return(NA_real_)
  }
  trial_data %>%
    arrange(timestamp) %>%
    mutate(
      x_diff = lead(x) - x,
      y_diff = lead(y) - y,
      distance_sq = (x_diff^2 + y_diff^2) * deg_per_pix^2 # Calculate squared distance directly
    ) %>%
    summarize(rms = sqrt(mean(distance_sq, na.rm = TRUE)), .groups = "drop") %>%
    pull(rms)
}

calculate_data_loss <- function(trial_data, sampling_rate, subject_id, trial_num) {
  if (is.null(trial_data) || nrow(trial_data) == 0) {
    return(tibble(eye = c("left", "right"), data_loss_percentage = NA_real_))
  }
  
  # Calculate theoretical number of samples
  start_timestamp <- trial_data$timestamp[1]
  end_timestamp <- trial_data$timestamp[nrow(trial_data)]
  
  theoretical_samples <- if (!is.na(start_timestamp) && !is.na(end_timestamp)) {
    round(((end_timestamp - start_timestamp + 1) / 1000) * sampling_rate)
  } else {
    0
  }
  
  trial_data %>%
    pivot_longer(cols = c(x_l, y_l, x_r, y_r),
                 names_to = c(".value", "eye"),
                 names_pattern = "(.)_(.)") %>%
    group_by(eye) %>%
    summarize(
      valid_samples = sum(!is.na(x) & !is.na(y)),
      .groups = "drop"
    ) %>%
    mutate(
      theoretical_samples = theoretical_samples,
      valid_ratio = ifelse(theoretical_samples > 0, valid_samples / theoretical_samples, 0),
      data_loss_percentage = (1 - valid_ratio) * 100
    ) %>%
    select(eye, data_loss_percentage)
}

calculate_blink_loss_percentage_eye <- function(trial_samples_eye, trial_blinks) {
  if (is.null(trial_samples_eye) || nrow(trial_samples_eye) == 0 || is.null(trial_blinks) || (is.logical(trial_blinks) && is.na(trial_blinks))) {
    return(0)
  }
  if (is.data.frame(trial_blinks) && nrow(trial_blinks) == 0) {
    return(0)
  }
  
  eye_label <- unique(trial_samples_eye$eye)
  if (length(eye_label) != 1) return(0) # Should only be one eye
  
  relevant_blinks <- if (is.data.frame(trial_blinks)) {
    trial_blinks %>% filter(eye == ifelse(eye_label == "right", "R", "L"))
  } else {
    tibble() # No blinks
  }
  
  if (nrow(relevant_blinks) == 0) return(0)
  
  total_samples <- nrow(trial_samples_eye)
  if (total_samples == 0) return(0)
  
  blinked_samples <- relevant_blinks %>%
    rowwise() %>%
    summarize(
      n_blinked = sum(trial_samples_eye$timestamp >= t_start & trial_samples_eye$timestamp <= t_end),
      .groups = "drop"
    ) %>%
    pull(n_blinked) %>%
    sum()
  
  (blinked_samples / total_samples) * 100
}

process_subject <- function(subject, sampling_rate, deg_per_pix) {
  subject_id <- extract_subject_id(subject$filename)
  
  # get data from trial_db so we can do an analysis by condition
  # need to repeat the itemN, acceptability and condition for each eye
  itemN <- rep(subject$trial_db$itemN, each = 2)
  cond <- rep(subject$trial_db$cond, each = 2)
  acceptable <- rep(subject$trial_db$acceptable, each = 2)

  # Process Precision (RMS) and Data Loss
  precision_loss_list <- tibble()
  if (!is.null(subject$trial_samples)) {
    precision_loss_list <- subject$trial_samples %>%
      imap_dfr(
        ~ {
          trial_num <- .y
          trial_data <- .x
          trial_blinks <- subject$trial_blinks[[trial_num]] # Access blink data for the current trial
          
          if (!is.null(trial_data) && nrow(trial_data) > 0) {
            left_eye_data <- trial_data %>% select(timestamp, x = x_l, y = y_l) %>% mutate(eye = "left")
            right_eye_data <- trial_data %>% select(timestamp, x = x_r, y = y_r) %>% mutate(eye = "right")
            
            rms_l <- calculate_sample_distance_rms(left_eye_data, deg_per_pix)
            rms_r <- calculate_sample_distance_rms(right_eye_data, deg_per_pix)
            
            # Calculate Data Loss (overall)
            data_loss <- calculate_data_loss(trial_data, sampling_rate, subject_id, trial_num)
            
            # Calculate Blink Loss Percentage per eye
            blink_loss_l <- calculate_blink_loss_percentage_eye(left_eye_data, trial_blinks)
            blink_loss_r <- calculate_blink_loss_percentage_eye(right_eye_data, trial_blinks)
            
            tibble(
              subject = subject_id,
              rate = sampling_rate,
              trial = trial_num,
              eye = c("left", "right"),
              rms = c(rms_l, rms_r),
              data_loss_percentage = data_loss$data_loss_percentage, # Assuming data_loss is already per eye
              blink_loss_percentage = c(blink_loss_l, blink_loss_r)
            )
          } else {
            tibble(
              subject = subject_id,
              rate = sampling_rate,
              trial = trial_num,
              eye = c("left", "right"),
              rms = NA_real_,
              data_loss_percentage = NA_real_,
              blink_loss_percentage = NA_real_
            )
          }
        }
      )
  }
  precision_loss_list <- precision_loss_list %>% mutate(itemN = itemN, acceptable = acceptable, cond = cond)
  return(precision_loss_list)
}

# Function to process a list of subjects with a given sampling rate
process_subject_list <- function(subject_list, sampling_rate, deg_per_pix) {
  if (length(subject_list) > 0) {
    future_map(subject_list, process_subject, sampling_rate = sampling_rate, deg_per_pix = deg_per_pix, .progress = TRUE)
  } else {
    list()
  }
}

# Process subjects for each sampling rate
tic("Process Subjects (250 Hz)")
results_250 <- process_subject_list(subjects_250, 250, deg_per_pix)
toc()

tic("Process Subjects (500 Hz)")
results_500 <- process_subject_list(subjects_500, 500, deg_per_pix)
toc()

tic("Process Subjects (1000 Hz)")
results_1000 <- process_subject_list(subjects_1000, 1000, deg_per_pix)
toc()

tic("Process Subjects (2000 Hz)")
results_2000 <- process_subject_list(subjects_2000, 2000, deg_per_pix)
toc()

all_precision_loss <- bind_rows(
  bind_rows(results_250),
  bind_rows(results_500),
  bind_rows(results_1000),
  bind_rows(results_2000)
)

# Save the precision loss data

write_csv(all_precision_loss, "analysis/precision_loss.csv")

# save the calibration data

write_csv(calibration_results, "analysis/calibration_results.csv")

# by rate

precision_loss_by_rate <- all_precision_loss %>% filter(eye == "right") %>%
  group_by(rate, cond) %>%
  summarize(
    mean_rms = mean(rms, na.rm = TRUE),
    mean_data_loss_percentage = mean(data_loss_percentage, na.rm = TRUE),
    mean_blink_loss_percentage = mean(blink_loss_percentage, na.rm = TRUE)
  )

# calibration results by rate

calibration_results_by_rate <- calibration_results %>% filter(eye == "RIGHT") %>%
  group_by(rate) %>% 
  summarize(
    mean_avg_error = mean(avg_error),
    mean_max_error = mean(max_error),
    mean_offset = mean(offset_deg),
    mean_x_offset = mean(x_offset),
    mean_y_offset = mean(y_offset)
  )
