library(testthat)
library(fixated)

test_that(".check_trial_mismatch warns on mismatched trials", {
  samples <- dplyr::tibble(trial_nr = 1:5, time = 1:5, x = 1:5, y = 1:5, eye = "R")
  fixations <- dplyr::tibble(trial_nr = 2:6, start_time = 1:5, end_time = 2:6, duration = 1:5, avg_x = 1:5, avg_y = 1:5, eye = "R")
  
  expect_warning(
    fixated:::.check_trial_mismatch(samples, fixations),
    "exact same set of trials"
  )
})

test_that(".check_trial_mismatch does not warn when trials match exactly", {
  samples <- dplyr::tibble(trial_nr = 1:5, time = 1:5, x = 1:5, y = 1:5, eye = "R")
  fixations <- dplyr::tibble(trial_nr = 1:5, start_time = 1:5, end_time = 2:6, duration = 1:5, avg_x = 1:5, avg_y = 1:5, eye = "R")
  rois <- dplyr::tibble(trial_nr = 1:5, word_id = 1:5, x_start = 1:5, x_end = 2:6, y_start = 1:5, y_end = 2:6)
  
  expect_silent(
    fixated:::.check_trial_mismatch(samples, fixations, rois)
  )
})
