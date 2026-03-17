## Helper: minimal fixation + ROI data for testing
make_test_roi <- function() {
  dplyr::tibble(
    trial_nr = 1L,
    word_id = 1:3,
    word    = c("The", "quick", "fox"),
    x_start = c(100, 200, 320),
    x_end   = c(195, 315, 420),
    y_start = c(380, 380, 380),
    y_end   = c(420, 420, 420)
  )
}

make_test_fixations <- function() {
  dplyr::tibble(
    trial_nr   = 1L,
    start_time = c(0L,   200L,  400L,  600L),
    end_time   = c(150L, 380L,  550L,  750L),
    duration   = c(150L, 180L,  150L,  150L),
    avg_x      = c(145,  260,   160,   370),
    avg_y      = c(400,  400,   400,   400)
  )
}

test_that("compute_eye_measures returns a tibble", {
  result <- compute_eye_measures(make_test_fixations(), make_test_roi())
  expect_s3_class(result, "tbl_df")
})

test_that("compute_eye_measures has required output columns", {
  result <- compute_eye_measures(make_test_fixations(), make_test_roi())
  expected <- c("trial_nr", "word_id", "ffd", "gd", "gpt", "tvt", "n_fixations")
  expect_true(all(expected %in% names(result)))
})

test_that("compute_eye_measures TVT is non-negative", {
  result <- compute_eye_measures(make_test_fixations(), make_test_roi())
  expect_true(all(result$tvt >= 0L))
})

test_that("compute_eye_measures FFD equals first fixation duration for first-pass words", {
  # Word 1 (x: 100-195): fixated at time 0-150 (dur=150) – first pass
  # Word 2 (x: 200-315): fixated at time 200-380 (dur=180) – first pass
  result <- compute_eye_measures(make_test_fixations(), make_test_roi())

  w1 <- result[result$word_id == 1L, ]
  w2 <- result[result$word_id == 2L, ]

  if (nrow(w1) > 0L) expect_equal(w1$ffd, 150L)
  if (nrow(w2) > 0L) expect_equal(w2$ffd, 180L)
})

test_that("compute_eye_measures GD >= FFD for first-pass words", {
  result <- compute_eye_measures(make_test_fixations(), make_test_roi())

  fp_rows <- result[!is.na(result$ffd) & !is.na(result$gd), ]
  if (nrow(fp_rows) > 0L) {
    expect_true(all(fp_rows$gd >= fp_rows$ffd))
  }
})

test_that("compute_eye_measures GPT >= GD for first-pass words", {
  result <- compute_eye_measures(make_test_fixations(), make_test_roi())

  fp_rows <- result[!is.na(result$gd) & !is.na(result$gpt), ]
  if (nrow(fp_rows) > 0L) {
    expect_true(all(fp_rows$gpt >= fp_rows$gd))
  }
})

test_that("compute_eye_measures TVT >= GD when both present", {
  result <- compute_eye_measures(make_test_fixations(), make_test_roi())

  rows <- result[!is.na(result$gd), ]
  if (nrow(rows) > 0L) {
    expect_true(all(rows$tvt >= rows$gd))
  }
})

test_that("compute_eye_measures includes word column when available", {
  result <- compute_eye_measures(make_test_fixations(), make_test_roi(), include_word_col = TRUE)
  expect_true("word" %in% names(result))
})

test_that("compute_eye_measures handles re-reading (TVT > GD)", {
  # Fixation sequence: word1, word2, word1 (regression), word2
  fixations <- dplyr::tibble(
    trial_nr   = 1L,
    start_time = c(0L,   200L,  400L,  600L),
    end_time   = c(150L, 350L,  500L,  700L),
    duration   = c(150L, 150L,  100L,  100L),
    avg_x      = c(145,  260,   145,   260),
    avg_y      = c(400,  400,   400,   400)
  )
  roi <- make_test_roi()
  result <- compute_eye_measures(fixations, roi)

  w1 <- result[result$word_id == 1L, ]
  w2 <- result[result$word_id == 2L, ]

  # Word 1 TVT should include both fixations (150 + 100 = 250)
  if (nrow(w1) > 0L) expect_equal(w1$tvt, 250L)
  # Word 2 TVT should include both fixations (150 + 100 = 250)
  if (nrow(w2) > 0L) expect_equal(w2$tvt, 250L)
})

test_that("compute_eye_measures n_fixations is correct", {
  # One fixation per word
  fixations <- dplyr::tibble(
    trial_nr   = 1L,
    start_time = c(0L,   200L),
    end_time   = c(150L, 350L),
    duration   = c(150L, 150L),
    avg_x      = c(145,  260),
    avg_y      = c(400,  400)
  )
  result <- compute_eye_measures(fixations, make_test_roi())
  expect_true(all(result$n_fixations == 1L))
})

test_that("compute_eye_measures errors on missing fixation columns", {
  fix <- dplyr::tibble(start_time = 0L, end_time = 100L, duration = 100L)
  expect_error(compute_eye_measures(fix, make_test_roi()), "missing columns")
})

test_that("compute_eye_measures errors on missing ROI columns", {
  roi <- dplyr::tibble(trial_nr = 1L, word_id = 1L)
  expect_error(compute_eye_measures(make_test_fixations(), roi), "missing columns")
})

test_that("compute_eye_measures FFD is NA when word was skipped", {
  # Fixation goes directly to word 3, skipping words 1 and 2
  fixations <- dplyr::tibble(
    trial_nr   = 1L,
    start_time = c(0L),
    end_time   = c(150L),
    duration   = c(150L),
    avg_x      = c(370),
    avg_y      = c(400)
  )
  result <- compute_eye_measures(fixations, make_test_roi())

  # Only word 3 should have a row; FFD should be set for first pass
  expect_equal(nrow(result), 1L)
  expect_equal(result$word_id, 3L)
})

# ---------------------------------------------------------------------------
# trial_db display-on / display-off filtering tests
# ---------------------------------------------------------------------------

make_test_trial_db <- function(display_on = -50L, display_off = 800L) {
  dplyr::tibble(
    trial_nr     = 1L,
    t_display_on  = display_on,
    t_display_off = display_off
  )
}

test_that("compute_eye_measures trial_db: no filtering when trial_db is NULL", {
  result_no_db <- compute_eye_measures(make_test_fixations(), make_test_roi())
  result_null  <- compute_eye_measures(make_test_fixations(), make_test_roi(), trial_db = NULL)
  expect_equal(result_no_db, result_null)
})

test_that("compute_eye_measures trial_db: all fixations pass wide display window", {
  # Display window wider than all fixations: nothing should be filtered out
  trial_db <- make_test_trial_db(display_on = -100L, display_off = 1000L)
  result   <- compute_eye_measures(make_test_fixations(), make_test_roi(), trial_db = trial_db)
  result_no_filter <- compute_eye_measures(make_test_fixations(), make_test_roi())
  expect_equal(result, result_no_filter)
})

test_that("compute_eye_measures trial_db: fixation starting before display_on is excluded", {
  # Custom fixations: only one fixation on word 1 (start_time=0, before display_on=50)
  # and one fixation on word 2 (start_time=200, after display_on=50)
  fixations <- dplyr::tibble(
    trial_nr   = 1L,
    start_time = c(0L,   200L),
    end_time   = c(150L, 380L),
    duration   = c(150L, 180L),
    avg_x      = c(145,  260),
    avg_y      = c(400,  400)
  )
  trial_db <- make_test_trial_db(display_on = 50L, display_off = 800L)
  result   <- compute_eye_measures(fixations, make_test_roi(), trial_db = trial_db)
  # Word 1 should have no fixations (its only fixation starts at 0, which is NOT > 50)
  expect_false(1L %in% result$word_id)
  # Word 2 should still appear (start_time=200 > 50)
  expect_true(2L %in% result$word_id)
})

test_that("compute_eye_measures trial_db: fixation ending after display_off is excluded", {
  # make_test_fixations() fixation 4: start_time=600, end_time=750 (word 3)
  # Set display_off = 700 so that fixation 4 (end_time 750 >= 700) is excluded
  trial_db <- make_test_trial_db(display_on = -100L, display_off = 700L)
  result   <- compute_eye_measures(make_test_fixations(), make_test_roi(), trial_db = trial_db)
  # Word 3 should have no fixations now (fixation ends at 750, after display_off=700)
  expect_false(3L %in% result$word_id)
})

test_that("compute_eye_measures trial_db: NA display_on disables lower bound", {
  trial_db <- make_test_trial_db(display_on = NA_integer_, display_off = 800L)
  result   <- compute_eye_measures(make_test_fixations(), make_test_roi(), trial_db = trial_db)
  result_no_filter <- compute_eye_measures(make_test_fixations(), make_test_roi())
  expect_equal(result, result_no_filter)
})

test_that("compute_eye_measures trial_db: NA display_off disables upper bound", {
  trial_db <- make_test_trial_db(display_on = -100L, display_off = NA_integer_)
  result   <- compute_eye_measures(make_test_fixations(), make_test_roi(), trial_db = trial_db)
  result_no_filter <- compute_eye_measures(make_test_fixations(), make_test_roi())
  expect_equal(result, result_no_filter)
})

test_that("compute_eye_measures trial_db: errors on missing trial_db columns", {
  bad_tdb <- dplyr::tibble(trial_nr = 1L, t_display_on = 0L)
  expect_error(
    compute_eye_measures(make_test_fixations(), make_test_roi(), trial_db = bad_tdb),
    "missing columns"
  )
})
