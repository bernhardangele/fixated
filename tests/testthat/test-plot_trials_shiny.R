# Tests for plot_trials_shiny()
# These tests verify error handling and the internal helper .shiny_trial_nrs().
# Full Shiny/plotly integration tests require an interactive session and are
# therefore skipped in non-interactive environments.

# ---------------------------------------------------------------------------
# Dependency / argument validation
# ---------------------------------------------------------------------------

test_that("plot_trials_shiny errors when shiny is unavailable", {
  skip_if(
    requireNamespace("shiny", quietly = TRUE),
    "shiny is installed; skipping unavailability test"
  )
  result <- list(
    samples = dplyr::tibble(time = 1:3, x = 1:3, y = 1:3, eye = "R"),
    events  = dplyr::tibble()
  )
  expect_error(plot_trials_shiny(result), "shiny")
})

test_that("plot_trials_shiny errors when asc_result lacks required elements", {
  skip_if_not(
    requireNamespace("shiny",  quietly = TRUE) &&
    requireNamespace("plotly", quietly = TRUE) &&
    requireNamespace("DT",     quietly = TRUE),
    "shiny/plotly/DT not fully installed"
  )
  expect_error(
    plot_trials_shiny(list(samples = dplyr::tibble())),
    "samples.*events|events.*samples"
  )
})

test_that("plot_trials_shiny errors when asc_result is not a list", {
  skip_if_not(
    requireNamespace("shiny",  quietly = TRUE) &&
    requireNamespace("plotly", quietly = TRUE) &&
    requireNamespace("DT",     quietly = TRUE),
    "shiny/plotly/DT not fully installed"
  )
  expect_error(plot_trials_shiny("not a list"), class = "simpleError")
})

# ---------------------------------------------------------------------------
# Internal helper: .shiny_trial_nrs
# ---------------------------------------------------------------------------

test_that(".shiny_trial_nrs returns trial_nrs from trial_db when present", {
  asc_result <- list(
    samples  = dplyr::tibble(time = 1:4, x = 1:4, y = 1:4, eye = "R",
                              trial_nr = c(1L, 1L, 2L, 2L)),
    events   = dplyr::tibble(),
    trial_db = dplyr::tibble(trial_nr = c(1L, 2L))
  )
  nrs <- fixated:::.shiny_trial_nrs(asc_result)
  expect_equal(nrs, c(1L, 2L))
})

test_that(".shiny_trial_nrs falls back to samples$trial_nr when no trial_db", {
  asc_result <- list(
    samples = dplyr::tibble(time = 1:4, x = 1:4, y = 1:4, eye = "R",
                             trial_nr = c(3L, 3L, 5L, 5L)),
    events  = dplyr::tibble()
  )
  nrs <- fixated:::.shiny_trial_nrs(asc_result)
  expect_equal(nrs, c(3L, 5L))
})

test_that(".shiny_trial_nrs returns 0L as fallback when no trial info", {
  asc_result <- list(
    samples = dplyr::tibble(time = 1:3, x = 1:3, y = 1:3, eye = "R"),
    events  = dplyr::tibble()
  )
  nrs <- fixated:::.shiny_trial_nrs(asc_result)
  expect_equal(nrs, 0L)
})
