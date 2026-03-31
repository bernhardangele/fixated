test_that("read_asc returns a list with samples and events", {
  asc_file <- system.file("extdata", "example.asc", package = "fixated")
  result <- read_asc(asc_file)

  expect_type(result, "list")
  expect_true(all(c("samples", "events") %in% names(result)))
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

# ---------------------------------------------------------------------------
# Trial structure (trial_db, trial_nr, get_trial)
# ---------------------------------------------------------------------------

test_that("read_asc returns trial_db for OpenSesame binocular file", {
  asc_file <- system.file("extdata", "sub_1_example.asc", package = "fixated")
  skip_if_not(file.exists(asc_file), "sub_1_example.asc not found")
  result <- read_asc(asc_file)
  expect_true("trial_db" %in% names(result))
  tdb <- result$trial_db
  expect_false(is.null(tdb))
  expect_true(nrow(tdb) >= 1L)
})

test_that("trial_db has required columns", {
  asc_file <- system.file("extdata", "sub_1_example.asc", package = "fixated")
  skip_if_not(file.exists(asc_file), "sub_1_example.asc not found")
  result <- read_asc(asc_file)
  tdb    <- result$trial_db
  expected <- c("trial_nr", "sentence_nr", "t_trial_start",
                "t_recording_start", "t_gaze_target_on", "t_gaze_target_off",
                "t_display_on", "t_display_off", "t_trial_end",
                "has_display_off", "has_display_restart",
                "has_recording_restart")
  expect_true(all(expected %in% names(tdb)))
})

test_that("trial_db captures DISPLAY ON/OFF timestamps", {
  asc_file <- system.file("extdata", "sub_1_example.asc", package = "fixated")
  skip_if_not(file.exists(asc_file), "sub_1_example.asc not found")
  result <- read_asc(asc_file)
  tdb    <- result$trial_db
  expect_true(!is.na(tdb$t_display_on[[1L]]))
  expect_true(!is.na(tdb$t_display_off[[1L]]))
  expect_true(tdb$has_display_off[[1L]])
  # DISPLAY ON must precede DISPLAY OFF
  expect_true(tdb$t_display_on[[1L]] < tdb$t_display_off[[1L]])
})

test_that("trial_db captures GAZE TARGET ON timestamp", {
  asc_file <- system.file("extdata", "sub_1_example.asc", package = "fixated")
  skip_if_not(file.exists(asc_file), "sub_1_example.asc not found")
  result <- read_asc(asc_file)
  tdb    <- result$trial_db
  expect_true(!is.na(tdb$t_gaze_target_on[[1L]]))
})

test_that("trial_db captures trial_end from stop_trial MSG", {
  asc_file <- system.file("extdata", "sub_1_example.asc", package = "fixated")
  skip_if_not(file.exists(asc_file), "sub_1_example.asc not found")
  result <- read_asc(asc_file)
  tdb    <- result$trial_db
  expect_true(!is.na(tdb$t_trial_end[[1L]]))
})

test_that("t_trial_end uses stop_trial timestamp not END timestamp", {
  asc_file <- system.file("extdata", "sub_1_example.asc", package = "fixated")
  skip_if_not(file.exists(asc_file), "sub_1_example.asc not found")
  lines  <- readLines(asc_file, encoding = "UTF-8")
  result <- read_asc(asc_file)
  tdb    <- result$trial_db
  # Extract stop_trial timestamp from the first occurrence in the file
  stop_lines <- grep("stop_trial", lines, value = TRUE)
  stop_ts    <- as.integer(stringr::str_match(stop_lines[[1L]], "^MSG\\t(\\d+)")[, 2L])
  expect_equal(tdb$t_trial_end[[1L]], stop_ts)
})

test_that("clean example has no display or recording restarts", {
  asc_file <- system.file("extdata", "sub_1_example.asc", package = "fixated")
  skip_if_not(file.exists(asc_file), "sub_1_example.asc not found")
  result <- read_asc(asc_file)
  tdb    <- result$trial_db
  expect_true(all(!tdb$has_display_restart))
  expect_true(all(!tdb$has_recording_restart))
})

test_that("read_asc adds trial_nr column to samples for OpenSesame file", {
  asc_file <- system.file("extdata", "sub_1_example.asc", package = "fixated")
  skip_if_not(file.exists(asc_file), "sub_1_example.asc not found")
  result <- read_asc(asc_file)
  expect_true("trial_nr" %in% names(result$samples))
})

test_that("read_asc adds trial_nr column to events for OpenSesame file", {
  asc_file <- system.file("extdata", "sub_1_example.asc", package = "fixated")
  skip_if_not(file.exists(asc_file), "sub_1_example.asc not found")
  result <- read_asc(asc_file)
  expect_true("trial_nr" %in% names(result$events))
})

test_that("trial_db parses OpenSesame var messages when parse_vars = TRUE", {
  asc_file <- system.file("extdata", "sub_1_example.asc", package = "fixated")
  skip_if_not(file.exists(asc_file), "sub_1_example.asc not found")
  result <- read_asc(asc_file, parse_vars = TRUE)
  tdb    <- result$trial_db
  # The example file contains 'var subject_nr 1'
  expect_true("os_subject_nr" %in% names(tdb))
  expect_equal(tdb$os_subject_nr[[1L]], "1")
})

test_that("trial_db has no var columns when parse_vars = FALSE", {
  asc_file <- system.file("extdata", "sub_1_example.asc", package = "fixated")
  skip_if_not(file.exists(asc_file), "sub_1_example.asc not found")
  result <- read_asc(asc_file, parse_vars = FALSE)
  tdb    <- result$trial_db
  base_cols <- c("trial_nr", "sentence_nr", "t_trial_start", "t_recording_start",
                 "t_gaze_target_on", "t_gaze_target_off", "t_display_on",
                 "t_display_off", "t_trial_end", "has_display_off",
                 "has_display_restart", "has_recording_restart")
  expect_equal(sort(names(tdb)), sort(base_cols))
})

test_that("read_asc returns NULL trial_db for non-OpenSesame file", {
  asc_file <- system.file("extdata", "example.asc", package = "fixated")
  skip_if_not(file.exists(asc_file), "example.asc not found")
  result <- read_asc(asc_file)
  expect_null(result$trial_db)
})

test_that("read_asc custom eye_tracker with NULL pattern returns NULL trial_db", {
  asc_file <- system.file("extdata", "example.asc", package = "fixated")
  skip_if_not(file.exists(asc_file), "example.asc not found")
  result <- read_asc(asc_file, eye_tracker = "custom")
  expect_null(result$trial_db)
})

# ---------------------------------------------------------------------------
# get_trial()
# ---------------------------------------------------------------------------

test_that("get_trial returns list with samples and events", {
  asc_file <- system.file("extdata", "sub_1_example.asc", package = "fixated")
  skip_if_not(file.exists(asc_file), "sub_1_example.asc not found")
  result <- read_asc(asc_file)
  trial0 <- get_trial(result, 0L)
  expect_type(trial0, "list")
  expect_true(all(c("samples", "events") %in% names(trial0)))
})

test_that("get_trial filters to correct trial", {
  asc_file <- system.file("extdata", "sub_1_example.asc", package = "fixated")
  skip_if_not(file.exists(asc_file), "sub_1_example.asc not found")
  result <- read_asc(asc_file)
  trial0 <- get_trial(result, 0L)
  # All rows must have trial_nr == 0
  if (nrow(trial0$samples) > 0L) {
    expect_true(all(trial0$samples$trial_nr == 0L))
  }
  if (nrow(trial0$events) > 0L) {
    expect_true(all(trial0$events$trial_nr == 0L))
  }
})

test_that("get_trial errors when trial_nr column is missing", {
  bad_result <- list(
    samples = dplyr::tibble(time = 1:3, x = 1:3, y = 1:3),
    events  = dplyr::tibble(type = "FIXATION")
  )
  expect_error(get_trial(bad_result, 1L), "trial_nr")
})

# ---------------------------------------------------------------------------
# get_eyelink_fixations()
# ---------------------------------------------------------------------------

test_that("get_eyelink_fixations returns FIXATION rows from events", {
  asc_file <- system.file("extdata", "sub_1_example.asc", package = "fixated")
  skip_if_not(file.exists(asc_file), "sub_1_example.asc not found")
  result <- read_asc(asc_file)
  fix    <- get_eyelink_fixations(result$events)
  expect_true(is.data.frame(fix))
  expected_cols <- c("start_time", "end_time", "duration", "avg_x", "avg_y")
  expect_true(all(expected_cols %in% names(fix)))
})

test_that("get_eyelink_fixations preserves trial_nr when present", {
  asc_file <- system.file("extdata", "sub_1_example.asc", package = "fixated")
  skip_if_not(file.exists(asc_file), "sub_1_example.asc not found")
  result <- read_asc(asc_file)
  if ("trial_nr" %in% names(result$events)) {
    fix <- get_eyelink_fixations(result$events)
    expect_true("trial_nr" %in% names(fix))
  }
})

test_that("get_eyelink_fixations preserves sentence_nr when present", {
  events <- dplyr::tibble(
    type        = rep("FIXATION", 3),
    eye         = rep("R", 3),
    start_time  = c(0L, 200L, 400L),
    end_time    = c(100L, 300L, 500L),
    duration    = c(100L, 100L, 100L),
    avg_x       = c(300, 400, 500),
    avg_y       = c(400, 400, 400),
    trial_nr    = c(1L, 1L, 2L),
    sentence_nr = c(1L, 2L, 1L)
  )
  fix <- get_eyelink_fixations(events)
  expect_true("sentence_nr" %in% names(fix))
  expect_equal(fix$sentence_nr, c(1L, 2L, 1L))
})

test_that("get_eyelink_fixations errors on missing required columns", {
  bad_events <- dplyr::tibble(type = "FIXATION", eye = "R")
  expect_error(get_eyelink_fixations(bad_events), "missing required columns")
})

# ---------------------------------------------------------------------------
# detect_fixations() saccades method
# ---------------------------------------------------------------------------

test_that("detect_fixations errors when saccades package is unavailable", {
  skip_if(requireNamespace("saccades", quietly = TRUE),
          "saccades package is installed; skipping unavailability test")
  samples <- dplyr::tibble(
    time = seq(0L, 500L, by = 10L),
    x    = rep(300, 51),
    y    = rep(400, 51)
  )
  expect_error(
    detect_fixations(samples, method = "saccades"),
    "saccades"
  )
})
