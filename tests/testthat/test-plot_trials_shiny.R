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
# Internal helper: .shiny_trial_choices
# ---------------------------------------------------------------------------

test_that(".shiny_trial_choices returns trials from trial_db when present", {
  samples  <- dplyr::tibble(time = 1:4, x = 1:4, y = 1:4, eye = "R",
                            trial_nr = c(1L, 1L, 2L, 2L))
  trial_db <- dplyr::tibble(trial_nr = c(1L, 2L))
  
  choices <- fixated:::.shiny_trial_choices(samples, NULL, NULL, NULL, trial_db)
  expect_equal(choices$trial_choices, c(`Trial 1` = 1L, `Trial 2` = 2L))
  expect_null(choices$subject_choices)
})

test_that(".shiny_trial_choices includes sentence_nr when present in trial_db", {
  trial_db <- dplyr::tibble(trial_nr = c(1L, 2L), sentence_nr = c(10L, 20L))
  choices <- fixated:::.shiny_trial_choices(NULL, NULL, NULL, NULL, trial_db)
  expect_equal(choices$trial_choices, c(`Trial 1 (Sentence 10)` = 1L, `Trial 2 (Sentence 20)` = 2L))
})

test_that(".shiny_trial_choices falls back to samples$trial_nr when no trial_db", {
  samples <- dplyr::tibble(time = 1:4, x = 1:4, y = 1:4, eye = "R",
                           trial_nr = c(3L, 3L, 5L, 5L))
  choices <- fixated:::.shiny_trial_choices(samples, NULL, NULL, NULL, NULL)
  expect_equal(choices$trial_choices, c(`Trial 3` = 3L, `Trial 5` = 5L))
})

test_that(".shiny_trial_choices returns 0L fallback when no trial info", {
  samples <- dplyr::tibble(time = 1:3, x = 1:3, y = 1:3, eye = "R")
  choices <- fixated:::.shiny_trial_choices(samples, NULL, NULL, NULL, NULL)
  expect_equal(choices$trial_choices, c(`Trial 0` = 0L))
})

test_that(".shiny_trial_choices returns subject choices for multi-subject data", {
  trial_db <- dplyr::tibble(
    subject = c("s1", "s1", "s2"),
    trial_nr = c(1L, 2L, 1L),
    sentence_nr = c(10L, 20L, 10L)
  )
  choices <- fixated:::.shiny_trial_choices(NULL, NULL, NULL, NULL, trial_db)

  expect_equal(unname(choices$subject_choices), c("s1", "s2"))
  expect_equal(choices$trial_choices, c(`Trial 1 (Sentence 10)` = 1L, `Trial 2 (Sentence 20)` = 2L))
})
