asc_file <- system.file("extdata", "eyetrack_example.asc", package = "fixated")

test_that("read_eyetrack_asc: file not found gives error", {
  expect_error(read_eyetrack_asc("/nonexistent/file.asc"))
})

test_that("read_eyetrack_asc: returns a list with three elements", {
  skip_if(!file.exists(asc_file), "sample EyeTrack ASC file not found")
  result <- read_eyetrack_asc(asc_file)
  expect_type(result, "list")
  expect_named(result, c("fixations", "word_boundaries", "trial_db"),
               ignore.order = FALSE)
})

test_that("read_eyetrack_asc: fixations tibble has required columns", {
  skip_if(!file.exists(asc_file), "sample EyeTrack ASC file not found")
  result <- read_eyetrack_asc(asc_file)
  expect_true(all(c("trial_nr", "start_time", "end_time", "duration",
                     "avg_x", "avg_y") %in% names(result$fixations)))
})

test_that("read_eyetrack_asc: word_boundaries tibble has required columns", {
  skip_if(!file.exists(asc_file), "sample EyeTrack ASC file not found")
  result <- read_eyetrack_asc(asc_file)
  expect_true(all(c("trial_nr", "word_id", "word",
                     "x_start", "x_end", "y_start", "y_end") %in%
                     names(result$word_boundaries)))
})

test_that("read_eyetrack_asc: trial_db tibble has required columns", {
  skip_if(!file.exists(asc_file), "sample EyeTrack ASC file not found")
  result <- read_eyetrack_asc(asc_file)
  expect_true(all(c("trial_nr", "trial_id", "cond", "item",
                     "t_display_on", "t_display_off") %in%
                     names(result$trial_db)))
})

test_that("read_eyetrack_asc: correct number of trials parsed", {
  skip_if(!file.exists(asc_file), "sample EyeTrack ASC file not found")
  # eyetrack_example.asc has 2 trials (P1I1D0, P2I2D0)
  result <- read_eyetrack_asc(asc_file)
  expect_equal(nrow(result$trial_db), 2L)
})

test_that("read_eyetrack_asc: trial metadata extracted correctly", {
  skip_if(!file.exists(asc_file), "sample EyeTrack ASC file not found")
  result <- read_eyetrack_asc(asc_file)
  tdb <- result$trial_db
  expect_equal(tdb$trial_id, c("P1I1D0", "P2I2D0"))
  expect_equal(tdb$cond, c(1L, 2L))
  expect_equal(tdb$item, c(1L, 2L))
})

test_that("read_eyetrack_asc: SYNCTIME captured as t_display_on", {
  skip_if(!file.exists(asc_file), "sample EyeTrack ASC file not found")
  result <- read_eyetrack_asc(asc_file)
  tdb <- result$trial_db
  # Both SYNCTIME messages are 1001010 and 1002010 in eyetrack_example.asc
  expect_equal(tdb$t_display_on[[1L]], 1001010L)
  expect_equal(tdb$t_display_on[[2L]], 1002010L)
})

test_that("read_eyetrack_asc: DISPLAY OFF captured as t_display_off", {
  skip_if(!file.exists(asc_file), "sample EyeTrack ASC file not found")
  result <- read_eyetrack_asc(asc_file)
  tdb <- result$trial_db
  expect_equal(tdb$t_display_off[[1L]], 1001500L)
  expect_equal(tdb$t_display_off[[2L]], 1002400L)
})

test_that("read_eyetrack_asc: fixation durations match source file", {
  skip_if(!file.exists(asc_file), "sample EyeTrack ASC file not found")
  result <- read_eyetrack_asc(asc_file)
  fix1 <- result$fixations[result$fixations$trial_nr == 1L, ]
  # Trial 1 has two fixations: 200ms and 170ms
  expect_equal(sort(fix1$duration), c(170L, 200L))
})

test_that("read_eyetrack_asc: word ROIs built correctly from REGION CHAR", {
  skip_if(!file.exists(asc_file), "sample EyeTrack ASC file not found")
  result <- read_eyetrack_asc(asc_file)
  wb1 <- result$word_boundaries[result$word_boundaries$trial_nr == 1L, ]
  # Trial 1: "The cat" → 2 words
  expect_equal(nrow(wb1), 2L)
  expect_equal(wb1$word_id, c(1L, 2L))
  # Word 1 is "The " (including trailing space per convention)
  expect_true(grepl("The", wb1$word[[1L]]))
  # Word 2 is "cat"
  expect_true(grepl("cat", wb1$word[[2L]]))
})

test_that("read_eyetrack_asc: trial_pattern filters trials", {
  skip_if(!file.exists(asc_file), "sample EyeTrack ASC file not found")
  # Pattern "^P1" matches only the first trial (P1I1D0)
  result <- read_eyetrack_asc(asc_file, trial_pattern = "^P1")
  expect_equal(nrow(result$trial_db), 1L)
  expect_equal(result$trial_db$trial_id, "P1I1D0")
})

test_that("read_eyetrack_asc: output is compatible with compute_eye_measures", {
  skip_if(!file.exists(asc_file), "sample EyeTrack ASC file not found")
  result <- read_eyetrack_asc(asc_file)
  # Should run without error
  measures <- compute_eye_measures(
    result$fixations,
    result$word_boundaries,
    trial_col = "trial_nr",
    trial_db = result$trial_db,
    truncate_at_display_on = TRUE
  )
  expect_s3_class(measures, "tbl_df")
  expect_true("ffd" %in% names(measures))
  expect_true("gd" %in% names(measures))
})
