test_that("detect_fixations returns a tibble with required columns", {
  samples <- dplyr::tibble(
    time = seq(0L, 990L, by = 10L),
    x    = c(rep(300, 60), rep(600, 40)) + stats::rnorm(100, 0, 1),
    y    = rep(400, 100) + stats::rnorm(100, 0, 1)
  )
  result <- detect_fixations(samples, min_duration = 100, max_dispersion = 25)

  expect_s3_class(result, "tbl_df")
  expected_cols <- c("trial_nr", "start_time", "end_time", "duration", "avg_x", "avg_y", "n_samples")
  expect_true(all(expected_cols %in% names(result)))
})

test_that("detect_fixations finds two distinct fixation clusters", {
  set.seed(42)
  samples <- dplyr::tibble(
    time = seq(0L, 990L, by = 10L),
    x    = c(rep(300, 60), rep(600, 40)) + stats::rnorm(100, 0, 2),
    y    = rep(400, 100) + stats::rnorm(100, 0, 2)
  )
  result <- detect_fixations(samples, min_duration = 100, max_dispersion = 30)

  expect_gte(nrow(result), 2L)
  avg_x_vals <- result$avg_x
  expect_true(any(avg_x_vals < 450))
  expect_true(any(avg_x_vals > 450))
})

test_that("detect_fixations returns empty tibble for empty input", {
  samples <- dplyr::tibble(
    time = integer(0), x = numeric(0), y = numeric(0)
  )
  result <- detect_fixations(samples)

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 0L)
})

test_that("detect_fixations errors on missing required columns", {
  samples <- dplyr::tibble(time = 1:10, x = 1:10)
  expect_error(detect_fixations(samples), "missing required columns")
})

test_that("detect_fixations durations are non-negative", {
  set.seed(1)
  samples <- dplyr::tibble(
    time = seq(0L, 500L, by = 10L),
    x    = rep(300, 51) + stats::rnorm(51, 0, 3),
    y    = rep(400, 51) + stats::rnorm(51, 0, 3)
  )
  result <- detect_fixations(samples, min_duration = 80)

  if (nrow(result) > 0L) {
    expect_true(all(result$duration >= 0L))
  }
})

test_that("detect_fixations respects trial_col grouping", {
  set.seed(7)
  samples <- dplyr::tibble(
    trial_nr = c(rep(1L, 50), rep(2L, 50)),
    time  = c(seq(0L, 490L, 10L), seq(0L, 490L, 10L)),
    x     = rep(300, 100) + stats::rnorm(100, 0, 2),
    y     = rep(400, 100) + stats::rnorm(100, 0, 2)
  )
  result <- detect_fixations(samples, min_duration = 100, trial_col = "trial_nr")

  expect_true("trial_nr" %in% names(result))
  expect_true(all(unique(result$trial_nr) %in% c(1L, 2L)))
})

test_that("detect_fixations n_samples is positive for all fixations", {
  set.seed(3)
  samples <- dplyr::tibble(
    time = seq(0L, 490L, by = 10L),
    x    = rep(300, 50) + stats::rnorm(50, 0, 2),
    y    = rep(400, 50) + stats::rnorm(50, 0, 2)
  )
  result <- detect_fixations(samples, min_duration = 80)

  if (nrow(result) > 0L) {
    expect_true(all(result$n_samples > 0L))
  }
})

test_that("detect_fixations handles NA in trial_nr and issues a warning", {
  set.seed(42)
  samples <- dplyr::tibble(
    trial_nr = c(rep(1L, 30), rep(NA_integer_, 30)),
    time  = seq(0L, 590L, 10L),
    x     = rep(300, 60) + stats::rnorm(60, 0, 1),
    y     = rep(400, 60) + stats::rnorm(60, 0, 1)
  )
  
  expect_warning(
    result <- detect_fixations(samples, trial_col = "trial_nr"),
    "There are NA values in the 'trial_nr' column"
  )
  
  expect_true(any(is.na(result$trial_nr)))
  expect_true(any(result$trial_nr == 1L, na.rm = TRUE))
})
