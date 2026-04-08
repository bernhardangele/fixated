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
  # Fixated words have 1 fixation each
  fixated <- result[result$n_fixations > 0, ]
  expect_true(all(fixated$n_fixations == 1L))
  # Unfixated word has 0 fixations
  unfixated <- result[result$n_fixations == 0, ]
  expect_equal(nrow(unfixated), 1L)
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
  # First fixation lands on word 2, then later fixation on word 1
  # Word 1 is "skipped" because a prior fixation was on a later word (word 2)
  fixations <- dplyr::tibble(
    trial_nr   = 1L,
    start_time = c(0L,   200L),
    end_time   = c(150L, 350L),
    duration   = c(150L, 150L),
    avg_x      = c(260,  145),
    avg_y      = c(400,  400)
  )
  result <- compute_eye_measures(fixations, make_test_roi())

  # All 3 ROI words are present
  expect_equal(nrow(result), 3L)
  # Word 1 was skipped (prior fixation on word 2 > word 1)
  w1 <- result[result$word_id == 1L, ]
  expect_true(is.na(w1$ffd))
  # Word 2 was not skipped (first fixation, no prior fixations)
  w2 <- result[result$word_id == 2L, ]
  expect_true(!is.na(w2$ffd))
  # Word 3 was never fixated
  w3 <- result[result$word_id == 3L, ]
  expect_equal(w3$n_fixations, 0L)
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
  # All ROI words are present
  expect_true(1L %in% result$word_id)
  expect_true(2L %in% result$word_id)
  # Word 1 has no fixations (its only fixation starts at 0, which is NOT > 50)
  w1 <- result[result$word_id == 1L, ]
  expect_equal(w1$n_fixations, 0L)
  # Word 2 still has its fixation (start_time=200 > 50)
  w2 <- result[result$word_id == 2L, ]
  expect_equal(w2$n_fixations, 1L)
})

test_that("compute_eye_measures trial_db: fixation ending after display_off is excluded", {
  # make_test_fixations() fixation 4: start_time=600, end_time=750 (word 3)
  # Set display_off = 700 so that fixation 4 (end_time 750 >= 700) is excluded
  trial_db <- make_test_trial_db(display_on = -100L, display_off = 700L)
  result   <- compute_eye_measures(make_test_fixations(), make_test_roi(), trial_db = trial_db)
  # All ROI words are present
  expect_true(3L %in% result$word_id)
  # Word 3 has no fixations (fixation ends at 750, after display_off=700)
  w3 <- result[result$word_id == 3L, ]
  expect_equal(w3$n_fixations, 0L)
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

test_that("compute_eye_measures includes unfixated words with NAs", {
  roi <- dplyr::tibble(
    trial_nr = 1L,
    word_id  = 1:4,
    word     = c("The", "quick", "brown", "fox"),
    x_start  = c(100, 200, 320, 450),
    x_end    = c(195, 315, 415, 550),
    y_start  = c(380, 380, 380, 380),
    y_end    = c(420, 420, 420, 420)
  )
  # Fixations only land on words 1 and 2
  fixations <- dplyr::tibble(
    trial_nr   = 1L,
    start_time = c(0L, 200L),
    end_time   = c(150L, 380L),
    duration   = c(150L, 180L),
    avg_x      = c(145, 260),
    avg_y      = c(400, 400)
  )
  result <- compute_eye_measures(fixations, roi)
  expect_equal(nrow(result), 4L)
  # Words 3 and 4 should have NA measures and 0 fixations
  w3 <- result[result$word_id == 3L, ]
  w4 <- result[result$word_id == 4L, ]
  expect_true(is.na(w3$ffd))
  expect_true(is.na(w3$gd))
  expect_true(is.na(w3$gpt))
  expect_true(is.na(w3$tvt))
  expect_equal(w3$n_fixations, 0L)
  expect_equal(w4$n_fixations, 0L)
  # Words 1 and 2 should have valid measures
  w1 <- result[result$word_id == 1L, ]
  expect_true(!is.na(w1$ffd))
  expect_equal(w1$n_fixations, 1L)
})

# ---------------------------------------------------------------------------
# shorttime parameter tests
# ---------------------------------------------------------------------------

test_that("shorttime: short fixations are excluded from FFD/GD/GPT but not TVT", {
  roi <- make_test_roi()
  # Sequence: word1 (50ms, short), word2 (180ms), word1 (150ms second pass)
  fixations <- dplyr::tibble(
    trial_nr   = 1L,
    start_time = c(0L,    200L,  400L),
    end_time   = c(50L,   380L,  550L),
    duration   = c(50L,   180L,  150L),
    avg_x      = c(145,   260,   145),
    avg_y      = c(400,   400,   400)
  )
  result <- compute_eye_measures(fixations, roi, shorttime = 80L)

  w1 <- result[result$word_id == 1L, ]
  w2 <- result[result$word_id == 2L, ]

  # Word 1 first fixation is 50ms <= 80ms (short): FFD and GD should be NA
  expect_true(is.na(w1$ffd))
  expect_true(is.na(w1$gd))
  # But TVT includes all fixations (50+150=200) since shorttime doesn't affect TVT
  expect_equal(w1$tvt, 200L)
  # n_fixations counts all fixations (including short)
  expect_equal(w1$n_fixations, 2L)

  # Word 2 should have FFD = 180ms (normal, not short)
  expect_equal(w2$ffd, 180L)
})

test_that("shorttime: short fixation past right boundary does not end first pass", {
  roi <- make_test_roi()
  # Sequence: word1 (150ms), short-unassigned (40ms, x=180 between words 1 and 2),
  #           word1 again (100ms) - this should still be part of GD if shorttime=80
  # Actually build a case where a short fixation lands outside word1 but
  # a subsequent fixation returns to word1 — in eyedry this would still be
  # part of GD.
  # avg_x=180 is between 195 (x_end_word1) and 200 (x_start_word2)
  fixations <- dplyr::tibble(
    trial_nr   = 1L,
    start_time = c(0L,  200L, 300L),
    end_time   = c(150L, 260L, 440L),
    duration   = c(150L, 60L,  140L),
    avg_x      = c(145,  197,  145),  # word1, gap (unassigned), word1
    avg_y      = c(400,  400,  400)
  )
  # Without shorttime: gap fixation (avg_x=197 > x_end=195) breaks GD → GD=150
  result_no_st <- compute_eye_measures(fixations, roi)
  w1_no_st <- result_no_st[result_no_st$word_id == 1L, ]
  expect_equal(w1_no_st$gd, 150L)

  # With shorttime=80: short gap fixation is skipped, GD continues to next word1
  result_st <- compute_eye_measures(fixations, roi, shorttime = 80L)
  w1_st <- result_st[result_st$word_id == 1L, ]
  expect_equal(w1_st$gd, 290L)  # 150 + 140
})

# ---------------------------------------------------------------------------
# longtime parameter tests
# ---------------------------------------------------------------------------

test_that("longtime: long fixation on word sets FFD/GD/GPT to NA", {
  roi <- make_test_roi()
  fixations <- dplyr::tibble(
    trial_nr   = 1L,
    start_time = c(0L),
    end_time   = c(1000L),
    duration   = c(1000L),
    avg_x      = c(145),   # word 1
    avg_y      = c(400)
  )
  result <- compute_eye_measures(fixations, roi, longtime = 800L)
  w1 <- result[result$word_id == 1L, ]

  expect_true(is.na(w1$ffd))
  expect_true(is.na(w1$gd))
  expect_true(is.na(w1$gpt))
  # TVT: longtime stops scan but keeps accumulated time if any before the
  # long fixation; here it's the only fixation so TVT = NA
  expect_true(is.na(w1$tvt))
})

test_that("longtime: TVT keeps accumulated time before longtime fixation", {
  roi <- make_test_roi()
  # Two fixations on word1: first 200ms (valid), then 1000ms (long)
  fixations <- dplyr::tibble(
    trial_nr   = 1L,
    start_time = c(0L,    300L),
    end_time   = c(200L,  1300L),
    duration   = c(200L,  1000L),
    avg_x      = c(145,   145),
    avg_y      = c(400,   400)
  )
  result <- compute_eye_measures(fixations, roi, longtime = 800L)
  w1 <- result[result$word_id == 1L, ]

  # FFD: first fixation (200ms) is valid → FFD = 200
  expect_equal(w1$ffd, 200L)
  # GD: long fixation encountered during first pass → region discarded → NA
  expect_true(is.na(w1$gd))
  # TVT: accumulates 200ms, then hits longtime fixation → stops, keeps 200ms
  expect_equal(w1$tvt, 200L)
})

# ---------------------------------------------------------------------------
# ttcutoff parameter tests
# ---------------------------------------------------------------------------

test_that("ttcutoff: TVT exceeding cutoff is set to NA", {
  roi <- make_test_roi()
  # Three fixations on word1: 200+200+200 = 600ms > ttcutoff=500
  fixations <- dplyr::tibble(
    trial_nr   = 1L,
    start_time = c(0L,  300L, 600L),
    end_time   = c(200L, 500L, 800L),
    duration   = c(200L, 200L, 200L),
    avg_x      = c(145,  145,  145),
    avg_y      = c(400,  400,  400)
  )
  result <- compute_eye_measures(fixations, roi, longtime = 800L, ttcutoff = 500L)
  w1 <- result[result$word_id == 1L, ]

  expect_true(is.na(w1$tvt))
})

test_that("ttcutoff: TVT below cutoff is not affected", {
  roi <- make_test_roi()
  fixations <- dplyr::tibble(
    trial_nr   = 1L,
    start_time = c(0L,  300L),
    end_time   = c(200L, 500L),
    duration   = c(200L, 200L),
    avg_x      = c(145,  145),
    avg_y      = c(400,  400)
  )
  result <- compute_eye_measures(fixations, roi, longtime = 800L, ttcutoff = 500L)
  w1 <- result[result$word_id == 1L, ]

  # 200+200=400 < 500 → TVT is preserved
  expect_equal(w1$tvt, 400L)
})

# ---------------------------------------------------------------------------
# truncate_at_display_on tests
# ---------------------------------------------------------------------------

test_that("truncate_at_display_on=FALSE excludes straddling fixation", {
  roi <- make_test_roi()
  # Fixation straddles display_on=100: starts at 50, ends at 200
  fixations <- dplyr::tibble(
    trial_nr   = 1L,
    start_time = c(50L,  300L),
    end_time   = c(200L, 450L),
    duration   = c(150L, 150L),
    avg_x      = c(145,  260),
    avg_y      = c(400,  400)
  )
  trial_db <- dplyr::tibble(
    trial_nr      = 1L,
    t_display_on  = 100L,
    t_display_off = 1000L
  )
  result <- compute_eye_measures(fixations, roi, trial_db = trial_db,
                                 truncate_at_display_on = FALSE)
  w1 <- result[result$word_id == 1L, ]
  # start_time=50 NOT > 100, so fixation is excluded
  expect_equal(w1$n_fixations, 0L)
  expect_true(is.na(w1$ffd))
})

test_that("truncate_at_display_on=TRUE truncates straddling fixation", {
  roi <- make_test_roi()
  # Fixation straddles display_on=100: starts at 50, ends at 200
  fixations <- dplyr::tibble(
    trial_nr   = 1L,
    start_time = c(50L,  300L),
    end_time   = c(200L, 450L),
    duration   = c(150L, 150L),
    avg_x      = c(145,  260),
    avg_y      = c(400,  400)
  )
  trial_db <- dplyr::tibble(
    trial_nr      = 1L,
    t_display_on  = 100L,
    t_display_off = 1000L
  )
  result <- compute_eye_measures(fixations, roi, trial_db = trial_db,
                                 truncate_at_display_on = TRUE)
  w1 <- result[result$word_id == 1L, ]
  # Straddling fixation is kept; duration truncated to 200-100=100ms
  expect_equal(w1$n_fixations, 1L)
  expect_equal(w1$ffd, 100L)
})

# ---------------------------------------------------------------------------
# Regression measures tests
# ---------------------------------------------------------------------------

test_that("compute_eye_measures includes oreg, ireg, pfix, nfix, sp columns", {
  result <- compute_eye_measures(make_test_fixations(), make_test_roi())
  expect_true(all(c("oreg", "ireg", "pfix", "nfix", "sp") %in% names(result)))
})

test_that("pfix and nfix are 0 for unfixated words", {
  roi <- make_test_roi()
  fixations <- dplyr::tibble(
    trial_nr   = 1L,
    start_time = c(0L),
    end_time   = c(150L),
    duration   = c(150L),
    avg_x      = c(145),   # only word 1 fixated
    avg_y      = c(400)
  )
  result <- compute_eye_measures(fixations, roi)
  w3 <- result[result$word_id == 3L, ]   # word 3 never fixated
  expect_equal(w3$pfix, 0L)
  expect_equal(w3$nfix, 0L)
})

test_that("pfix is 1 when word is fixated in first pass", {
  result <- compute_eye_measures(make_test_fixations(), make_test_roi())
  w1 <- result[result$word_id == 1L, ]
  w2 <- result[result$word_id == 2L, ]
  expect_equal(w1$pfix, 1L)
  expect_equal(w2$pfix, 1L)
})

test_that("nfix counts first-pass fixations", {
  roi <- make_test_roi()
  # Two fixations on word 1 during first pass (word 1, word 1, word 2)
  fixations <- dplyr::tibble(
    trial_nr   = 1L,
    start_time = c(0L,  200L, 400L),
    end_time   = c(150L, 350L, 550L),
    duration   = c(150L, 150L, 150L),
    avg_x      = c(145,  145,  260),
    avg_y      = c(400,  400,  400)
  )
  result <- compute_eye_measures(fixations, roi)
  w1 <- result[result$word_id == 1L, ]
  expect_equal(w1$nfix, 2L)
})

test_that("oreg is 1 when first-pass fixation is followed by leftward regression", {
  roi <- make_test_roi()
  # word2 (first pass), then regression to word1 (oreg=1 for word2)
  fixations <- dplyr::tibble(
    trial_nr   = 1L,
    start_time = c(0L,  200L, 400L),
    end_time   = c(150L, 350L, 600L),
    duration   = c(150L, 150L, 200L),
    avg_x      = c(260,  145,  370),
    avg_y      = c(400,  400,  400)
  )
  result <- compute_eye_measures(fixations, roi,
                                 shorttime = 80L, longtime = 800L)
  w2 <- result[result$word_id == 2L, ]
  # first fixation on word2 (260), next fix on word1 (145 < 200=x_start_word2)
  expect_equal(w2$oreg, 1L)
})

test_that("oreg is 0 when no regression occurs", {
  roi <- make_test_roi()
  # Simple left-to-right reading
  fixations <- dplyr::tibble(
    trial_nr   = 1L,
    start_time = c(0L,   200L),
    end_time   = c(150L, 350L),
    duration   = c(150L, 150L),
    avg_x      = c(145,  260),
    avg_y      = c(400,  400)
  )
  result <- compute_eye_measures(fixations, roi,
                                 shorttime = 80L, longtime = 800L)
  w1 <- result[result$word_id == 1L, ]
  expect_equal(w1$oreg, 0L)
})

test_that("ireg is 1 when a fixation arrives from the right", {
  roi <- make_test_roi()
  # word3 (first pass, goes past word2), then fixates word2 from the right
  fixations <- dplyr::tibble(
    trial_nr   = 1L,
    start_time = c(0L,   200L, 400L),
    end_time   = c(150L, 350L, 550L),
    duration   = c(150L, 150L, 150L),
    avg_x      = c(145,  370,  260),   # word1, word3, word2 (arriving from right)
    avg_y      = c(400,  400,  400)
  )
  result <- compute_eye_measures(fixations, roi,
                                 shorttime = 80L, longtime = 800L)
  w2 <- result[result$word_id == 2L, ]
  # word2 is fixated at i=3; previous fixation was at word3 (370 > x_end_word2=315)
  expect_equal(w2$ireg, 1L)
})

test_that("sp measures second-pass time after first rightward exit", {
  roi <- make_test_roi()
  # word1 → word2 → word1 (second pass on word1)
  fixations <- dplyr::tibble(
    trial_nr   = 1L,
    start_time = c(0L,   200L, 400L),
    end_time   = c(150L, 350L, 550L),
    duration   = c(150L, 150L, 150L),
    avg_x      = c(145,  260,  145),
    avg_y      = c(400,  400,  400)
  )
  result <- compute_eye_measures(fixations, roi,
                                 shorttime = 80L, longtime = 800L)
  w1 <- result[result$word_id == 1L, ]
  # sp for word1: after first rightward exit (word2 at t=200), second pass = 150ms
  expect_equal(w1$sp, 150L)
})

test_that("sp is NA when there is no second pass", {
  roi <- make_test_roi()
  # Simple left-to-right, no regression
  fixations <- dplyr::tibble(
    trial_nr   = 1L,
    start_time = c(0L,   200L),
    end_time   = c(150L, 350L),
    duration   = c(150L, 150L),
    avg_x      = c(145,  260),
    avg_y      = c(400,  400)
  )
  result <- compute_eye_measures(fixations, roi,
                                 shorttime = 80L, longtime = 800L)
  w1 <- result[result$word_id == 1L, ]
  expect_true(is.na(w1$sp))
})
