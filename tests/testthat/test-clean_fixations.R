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
