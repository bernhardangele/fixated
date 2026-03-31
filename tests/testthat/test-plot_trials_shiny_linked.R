# Tests for plot_trials_shiny_linked()
# These tests verify error handling and the internal helpers.
# Full Shiny/plotly integration tests require an interactive session and are
# therefore skipped in non-interactive environments.

# ---------------------------------------------------------------------------
# Dependency / argument validation
# ---------------------------------------------------------------------------

test_that("plot_trials_shiny_linked errors when shiny is unavailable", {
  skip_if(
    requireNamespace("shiny", quietly = TRUE),
    "shiny is installed; skipping unavailability test"
  )
  result <- list(
    samples = dplyr::tibble(time = 1:3, x = 1:3, y = 1:3, eye = "R"),
    events  = dplyr::tibble()
  )
  expect_error(plot_trials_shiny_linked(result), "shiny")
})

test_that("plot_trials_shiny_linked errors when asc_result lacks required elements", {
  skip_if_not(
    requireNamespace("shiny",  quietly = TRUE) &&
    requireNamespace("plotly", quietly = TRUE) &&
    requireNamespace("DT",     quietly = TRUE),
    "shiny/plotly/DT not fully installed"
  )
  expect_error(
    plot_trials_shiny_linked(list(samples = dplyr::tibble())),
    "samples.*events|events.*samples"
  )
})

test_that("plot_trials_shiny_linked errors when asc_result is not a list", {
  skip_if_not(
    requireNamespace("shiny",  quietly = TRUE) &&
    requireNamespace("plotly", quietly = TRUE) &&
    requireNamespace("DT",     quietly = TRUE),
    "shiny/plotly/DT not fully installed"
  )
  expect_error(plot_trials_shiny_linked("not a list"), class = "simpleError")
})

test_that("plot_trials_shiny_linked errors when no data provided", {
  skip_if_not(
    requireNamespace("shiny",  quietly = TRUE) &&
    requireNamespace("plotly", quietly = TRUE) &&
    requireNamespace("DT",     quietly = TRUE),
    "shiny/plotly/DT not fully installed"
  )
  expect_error(
    plot_trials_shiny_linked(NULL, samples = NULL, fixations = NULL),
    "asc_result|samples.*fixations"
  )
})
