test_that("clean_fixations removes short fixations", {
  fixations <- dplyr::tibble(
    start_time = c(0L,   200L, 400L),
    end_time   = c(50L,  280L, 600L),
    duration   = c(50L,  80L,  200L),
    avg_x      = c(300,  450,  600),
    avg_y      = c(400,  400,  400)
  )
  result <- clean_fixations(fixations, min_duration = 80, merge_distance = 1000)

  expect_equal(nrow(result), 2L)
  expect_true(all(result$duration >= 80L))
})

test_that("clean_fixations removes long fixations", {
  fixations <- dplyr::tibble(
    start_time = c(0L,   200L, 400L),
    end_time   = c(150L, 280L, 1800L),
    duration   = c(150L, 80L,  1400L),
    avg_x      = c(300,  450,  600),
    avg_y      = c(400,  400,  400)
  )
  result <- clean_fixations(fixations, min_duration = 80, max_duration = 1200, merge_distance = 0)

  expect_equal(nrow(result), 2L)
  expect_true(all(result$duration <= 1200L))
})

test_that("clean_fixations merges nearby consecutive fixations", {
  fixations <- dplyr::tibble(
    start_time = c(0L,   120L,  300L),
    end_time   = c(100L, 200L,  500L),
    duration   = c(100L, 80L,   200L),
    avg_x      = c(300,  310,   600),
    avg_y      = c(400,  402,   400)
  )
  # First two fixations are close in space and time
  result <- clean_fixations(
    fixations,
    min_duration   = 80,
    merge_distance = 40,
    merge_max_gap  = 30
  )

  # After merging fixations 1 and 2, we should have 2 fixations
  expect_equal(nrow(result), 2L)
})

test_that("clean_fixations respects x_bounds", {
  fixations <- dplyr::tibble(
    start_time = c(0L,   200L),
    end_time   = c(150L, 380L),
    duration   = c(150L, 180L),
    avg_x      = c(50,   400),
    avg_y      = c(400,  400)
  )
  result <- clean_fixations(fixations, x_bounds = c(100, 1000), merge_distance = 1000)

  expect_equal(nrow(result), 1L)
  expect_equal(result$avg_x, 400)
})

test_that("clean_fixations returns empty tibble when all filtered out", {
  fixations <- dplyr::tibble(
    start_time = c(0L),
    end_time   = c(50L),
    duration   = c(50L),
    avg_x      = c(300),
    avg_y      = c(400)
  )
  result <- clean_fixations(fixations, min_duration = 100)

  expect_equal(nrow(result), 0L)
})

test_that("clean_fixations errors on missing required columns", {
  fixations <- dplyr::tibble(start_time = 0L, end_time = 100L, duration = 100L)
  expect_error(clean_fixations(fixations), "missing required columns")
})

test_that("clean_fixations does not merge fixations that are far apart", {
  fixations <- dplyr::tibble(
    start_time = c(0L,   200L),
    end_time   = c(150L, 380L),
    duration   = c(150L, 180L),
    avg_x      = c(300,  700),
    avg_y      = c(400,  400)
  )
  result <- clean_fixations(
    fixations,
    min_duration   = 80,
    merge_distance = 40,
    merge_max_gap  = 200
  )

  expect_equal(nrow(result), 2L)
})

test_that("clean_fixations merged duration equals end_time - start_time", {
  fixations <- dplyr::tibble(
    start_time = c(0L,   120L),
    end_time   = c(100L, 220L),
    duration   = c(100L, 100L),
    avg_x      = c(300,  305),
    avg_y      = c(400,  402)
  )
  result <- clean_fixations(
    fixations,
    min_duration   = 80,
    merge_distance = 40,
    merge_max_gap  = 30
  )

  if (nrow(result) > 0L) {
    expect_equal(
      result$duration,
      as.integer(result$end_time - result$start_time)
    )
  }
})

test_that("annotate = TRUE returns all rows with removed and reason columns", {
  fixations <- dplyr::tibble(
    start_time = c(0L,   200L, 400L),
    end_time   = c(150L, 280L, 600L),
    duration   = c(150L, 80L,  200L),
    avg_x      = c(300,  450,  600),
    avg_y      = c(400,  400,  400)
  )
  result <- clean_fixations(fixations, min_duration = 80, max_duration = 1200,
                            merge_distance = 0, annotate = TRUE)

  expect_equal(nrow(result), 3L)
  expect_true("removed" %in% names(result))
  expect_true("reason" %in% names(result))
  expect_type(result$removed, "logical")
  expect_type(result$reason, "character")
})

test_that("annotate marks short fixations as 'too short'", {
  fixations <- dplyr::tibble(
    start_time = c(0L,   200L),
    end_time   = c(50L,  350L),
    duration   = c(50L,  150L),
    avg_x      = c(300,  600),
    avg_y      = c(400,  400)
  )
  result <- clean_fixations(fixations, min_duration = 80, merge_distance = 0,
                            annotate = TRUE)

  expect_equal(nrow(result), 2L)
  expect_true(result$removed[1])
  expect_equal(result$reason[1], "too short")
  expect_false(result$removed[2])
  expect_true(is.na(result$reason[2]))
})

test_that("annotate marks long fixations as 'too long'", {
  fixations <- dplyr::tibble(
    start_time = c(0L,    200L),
    end_time   = c(1500L, 350L),
    duration   = c(1500L, 150L),
    avg_x      = c(300,   600),
    avg_y      = c(400,   400)
  )
  result <- clean_fixations(fixations, max_duration = 1200, merge_distance = 0,
                            annotate = TRUE)

  expect_equal(nrow(result), 2L)
  expect_true(result$removed[1])
  expect_equal(result$reason[1], "too long")
  expect_false(result$removed[2])
})

test_that("annotate marks out-of-bounds fixations", {
  fixations <- dplyr::tibble(
    start_time = c(0L,   200L),
    end_time   = c(150L, 350L),
    duration   = c(150L, 150L),
    avg_x      = c(50,   400),
    avg_y      = c(400,  400)
  )
  result <- clean_fixations(fixations, x_bounds = c(100, 1000),
                            merge_distance = 0, annotate = TRUE)

  expect_equal(nrow(result), 2L)
  expect_true(result$removed[1])
  expect_equal(result$reason[1], "out of bounds")
  expect_false(result$removed[2])
})

test_that("annotate marks short merged fixations as 'too short and merged'", {
  fixations <- dplyr::tibble(
    start_time = c(0L,   110L,  300L),
    end_time   = c(100L, 150L,  500L),
    duration   = c(100L, 40L,   200L),
    avg_x      = c(300,  310,   600),
    avg_y      = c(400,  402,   400)
  )
  result <- clean_fixations(
    fixations,
    min_duration   = 80,
    merge_distance = 40,
    merge_max_gap  = 20,
    annotate       = TRUE
  )

  expect_equal(nrow(result), 3L)
  # Fixation 2 is short and gets merged into fixation 1
  expect_true(result$removed[2])
  expect_equal(result$reason[2], "too short and merged")
  # Fixations 1 and 3 are clean
  expect_false(result$removed[1])
  expect_false(result$removed[3])
})

test_that("annotate works with empty input", {
  fixations <- dplyr::tibble(
    start_time = integer(0),
    end_time   = integer(0),
    duration   = integer(0),
    avg_x      = numeric(0),
    avg_y      = numeric(0)
  )
  result <- clean_fixations(fixations, annotate = TRUE)

  expect_equal(nrow(result), 0L)
  expect_true("removed" %in% names(result))
  expect_true("reason" %in% names(result))
})
