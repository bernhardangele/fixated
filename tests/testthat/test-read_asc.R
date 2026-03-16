test_that("read_asc returns a list with samples and events", {
  asc_file <- system.file("extdata", "example.asc", package = "fixated")
  result <- read_asc(asc_file)

  expect_type(result, "list")
  expect_named(result, c("samples", "events"))
})

test_that("read_asc samples has required columns", {
  asc_file <- system.file("extdata", "example.asc", package = "fixated")
  result <- read_asc(asc_file)

  expect_true(all(c("time", "x", "y", "pupil", "eye") %in% names(result$samples)))
})

test_that("read_asc events has required columns", {
  asc_file <- system.file("extdata", "example.asc", package = "fixated")
  result <- read_asc(asc_file)

  expected_cols <- c("type", "eye", "start_time", "end_time", "duration")
  expect_true(all(expected_cols %in% names(result$events)))
})

test_that("read_asc events contains expected event types", {
  asc_file <- system.file("extdata", "example.asc", package = "fixated")
  result <- read_asc(asc_file)

  event_types <- unique(result$events$type)
  expect_true(all(event_types %in% c("FIXATION", "SACCADE", "BLINK")))
  expect_true("FIXATION" %in% event_types)
  expect_true("SACCADE" %in% event_types)
})

test_that("read_asc respects eyes filter", {
  asc_file <- system.file("extdata", "example.asc", package = "fixated")
  result <- read_asc(asc_file, eyes = "L")

  expect_true(all(result$samples$eye == "L"))
  expect_true(all(result$events$eye == "L"))
})

test_that("read_asc samples are sorted by time", {
  asc_file <- system.file("extdata", "example.asc", package = "fixated")
  result <- read_asc(asc_file)

  if (nrow(result$samples) > 1L) {
    expect_true(all(diff(result$samples$time) >= 0L))
  }
})

test_that("read_asc errors on missing file", {
  expect_error(read_asc("/nonexistent/file.asc"))
})

test_that("read_asc fixation durations are positive", {
  asc_file <- system.file("extdata", "example.asc", package = "fixated")
  result <- read_asc(asc_file)
  fix <- result$events[result$events$type == "FIXATION", ]

  if (nrow(fix) > 0L) {
    expect_true(all(fix$duration > 0L))
  }
})
