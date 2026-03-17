# Tests for plot_trial()
# These tests skip when ggplot2 is unavailable and when the example ASC file
# is absent so that the package can be checked in minimal environments.

skip_if_no_ggplot2 <- function() {
  skip_if_not(
    requireNamespace("ggplot2", quietly = TRUE),
    "ggplot2 is not installed"
  )
}

asc_path <- system.file("extdata", "sub_1_example.asc", package = "fixated")

# ---------------------------------------------------------------------------
# Error handling
# ---------------------------------------------------------------------------

test_that("plot_trial errors on missing ggplot2", {
  skip_if(
    requireNamespace("ggplot2", quietly = TRUE),
    "ggplot2 is installed; skipping unavailability test"
  )
  result <- list(
    samples = dplyr::tibble(time = 1:3, x = 1:3, y = 1:3, eye = "R"),
    events  = dplyr::tibble()
  )
  expect_error(plot_trial(result, 0L), "ggplot2")
})

test_that("plot_trial errors when asc_result lacks required elements", {
  skip_if_no_ggplot2()
  expect_error(plot_trial(list(samples = dplyr::tibble()), 0L),
               "samples.*events|events.*samples")
})

# ---------------------------------------------------------------------------
# Basic return value
# ---------------------------------------------------------------------------

test_that("plot_trial returns a ggplot object", {
  skip_if_no_ggplot2()
  skip_if_not(file.exists(asc_path), "sub_1_example.asc not found")

  result <- read_asc(asc_path)
  tnr    <- result$trial_db$trial_nr[[1L]]

  p <- plot_trial(result, trial_nr = tnr)
  expect_s3_class(p, "gg")
  expect_s3_class(p, "ggplot")
})

test_that("plot_trial returns a ggplot object even when no samples match", {
  skip_if_no_ggplot2()
  result <- list(
    samples = dplyr::tibble(
      time = 1:5, x = rep(300, 5), y = rep(400, 5),
      eye = rep("R", 5), trial_nr = rep(1L, 5)
    ),
    events = dplyr::tibble(
      type = character(0), eye = character(0),
      start_time = integer(0), end_time = integer(0),
      duration = integer(0), avg_x = numeric(0), avg_y = numeric(0)
    )
  )
  expect_warning(
    p <- plot_trial(result, trial_nr = 99L),
    "No samples"
  )
  expect_s3_class(p, "ggplot")
})

# ---------------------------------------------------------------------------
# show_* toggle flags
# ---------------------------------------------------------------------------

test_that("plot_trial respects show_samples = FALSE", {
  skip_if_no_ggplot2()
  skip_if_not(file.exists(asc_path), "sub_1_example.asc not found")

  result <- read_asc(asc_path)
  tnr    <- result$trial_db$trial_nr[[1L]]

  p_with    <- plot_trial(result, trial_nr = tnr, show_samples = TRUE)
  p_without <- plot_trial(result, trial_nr = tnr, show_samples = FALSE)

  # Fewer layers when samples are suppressed
  expect_true(length(p_with$layers) > length(p_without$layers))
})

test_that("plot_trial respects show_fixations = FALSE", {
  skip_if_no_ggplot2()
  skip_if_not(file.exists(asc_path), "sub_1_example.asc not found")

  result <- read_asc(asc_path)
  tnr    <- result$trial_db$trial_nr[[1L]]

  p_with    <- plot_trial(result, trial_nr = tnr, show_fixations = TRUE)
  p_without <- plot_trial(result, trial_nr = tnr, show_fixations = FALSE)

  expect_true(length(p_with$layers) > length(p_without$layers))
})

test_that("plot_trial respects show_word_boundaries = FALSE", {
  skip_if_no_ggplot2()
  skip_if_not(file.exists(asc_path), "sub_1_example.asc not found")

  result <- read_asc(asc_path)
  tnr    <- result$trial_db$trial_nr[[1L]]

  p_with    <- plot_trial(result, trial_nr = tnr, show_word_boundaries = TRUE)
  p_without <- plot_trial(result, trial_nr = tnr, show_word_boundaries = FALSE)

  expect_true(length(p_with$layers) >= length(p_without$layers))
})

# ---------------------------------------------------------------------------
# Word boundaries from asc_result$word_boundaries
# ---------------------------------------------------------------------------

test_that("plot_trial uses word_boundaries from asc_result when available", {
  skip_if_no_ggplot2()
  skip_if_not(file.exists(asc_path), "sub_1_example.asc not found")

  result <- read_asc(asc_path)
  tnr    <- result$trial_db$trial_nr[[1L]]

  # word_boundaries should be non-NULL for the example binocular file
  skip_if(is.null(result$word_boundaries), "word_boundaries not parsed")

  p <- plot_trial(result, trial_nr = tnr, show_word_boundaries = TRUE)
  expect_s3_class(p, "ggplot")
})

# ---------------------------------------------------------------------------
# show_measures flag
# ---------------------------------------------------------------------------

test_that("plot_trial show_measures = TRUE with valid roi does not error", {
  skip_if_no_ggplot2()
  skip_if_not(file.exists(asc_path), "sub_1_example.asc not found")

  result <- read_asc(asc_path)
  tnr    <- result$trial_db$trial_nr[[1L]]

  skip_if(is.null(result$word_boundaries), "word_boundaries not parsed")
  wb <- dplyr::filter(result$word_boundaries, .data$trial_nr == tnr)
  skip_if(nrow(wb) == 0L, "No word boundaries for this trial")

  # Need full ROI columns for show_measures
  skip_if(!all(c("x_start", "x_end", "y_start", "y_end") %in% names(wb)),
          "word_boundaries lacks full ROI columns")

  p <- plot_trial(result, trial_nr = tnr, roi = wb, show_measures = TRUE)
  expect_s3_class(p, "ggplot")
})

# ---------------------------------------------------------------------------
# Explicit x_limits / y_limits
# ---------------------------------------------------------------------------

test_that("plot_trial respects user-supplied axis limits", {
  skip_if_no_ggplot2()
  skip_if_not(file.exists(asc_path), "sub_1_example.asc not found")

  result <- read_asc(asc_path)
  tnr    <- result$trial_db$trial_nr[[1L]]

  p <- plot_trial(result, trial_nr = tnr,
                  x_limits = c(0, 1920), y_limits = c(0, 1080))
  expect_s3_class(p, "ggplot")
})

# ---------------------------------------------------------------------------
# Background image
# ---------------------------------------------------------------------------

test_that("plot_trial warns when bg_image_path does not exist", {
  skip_if_no_ggplot2()
  skip_if_not(file.exists(asc_path), "sub_1_example.asc not found")

  result <- read_asc(asc_path)
  tnr    <- result$trial_db$trial_nr[[1L]]

  expect_warning(
    plot_trial(result, trial_nr = tnr, bg_image_path = "/nonexistent/img.png"),
    "not found"
  )
})

test_that("plot_trial warns on unsupported image extension", {
  skip_if_no_ggplot2()
  skip_if_not(file.exists(asc_path), "sub_1_example.asc not found")

  result <- read_asc(asc_path)
  tnr    <- result$trial_db$trial_nr[[1L]]

  # Create a dummy file with an unsupported extension
  tmp <- tempfile(fileext = ".bmp")
  writeLines("dummy", tmp)
  on.exit(unlink(tmp), add = TRUE)

  expect_warning(
    plot_trial(result, trial_nr = tnr, bg_image_path = tmp),
    "Unsupported image format"
  )
})

# ---------------------------------------------------------------------------
# .load_raster_image internal helper
# ---------------------------------------------------------------------------

test_that(".load_raster_image warns and returns NULL for missing file", {
  expect_warning(
    result <- fixated:::.load_raster_image("/nonexistent/image.png"),
    "not found"
  )
  expect_null(result)
})

test_that(".load_raster_image warns on unsupported format", {
  tmp <- tempfile(fileext = ".tiff")
  writeLines("dummy", tmp)
  on.exit(unlink(tmp), add = TRUE)

  expect_warning(
    result <- fixated:::.load_raster_image(tmp),
    "Unsupported image format"
  )
  expect_null(result)
})
