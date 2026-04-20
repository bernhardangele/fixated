library(testthat)
library(fixated)

test_that("process_subject_files combines subjects and adds subject column", {
  f1 <- tempfile(fileext = ".asc")
  f2 <- tempfile(fileext = ".asc")
  writeLines("dummy", f1)
  writeLines("dummy", f2)

  dummy_reader <- function(path) {
    list(
      samples = dplyr::tibble(
        trial_nr = c(1L, 1L),
        time = c(10L, 20L),
        x = c(100, 120),
        y = c(500, 505),
        eye = c("R", "R")
      ),
      events = dplyr::tibble(
        type = "FIXATION",
        eye = "R",
        start_time = 10L,
        end_time = 20L,
        duration = 10L,
        avg_x = 110,
        avg_y = 502
      ),
      word_boundaries = dplyr::tibble(
        trial_nr = 1L,
        word_id = 1L,
        word = "The",
        x_start = 90,
        x_end = 150,
        y_start = 450,
        y_end = 550
      ),
      trial_db = dplyr::tibble(
        trial_nr = 1L,
        t_display_on = 0L,
        t_display_off = 100L
      )
    )
  }

  out <- process_subject_files(
    paths = c(f1, f2),
    subject_ids = c("s1", "s2"),
    read_fun = dummy_reader
  )

  expect_true(all(c("s1", "s2") %in% names(out$subjects)))
  expect_true("subject" %in% names(out$samples))
  expect_true("subject" %in% names(out$fixations))
  expect_true("subject" %in% names(out$measures))
  expect_equal(sort(unique(out$samples$subject)), c("s1", "s2"))
})

test_that("process_subject_files supports reader output with fixations element", {
  f1 <- tempfile(fileext = ".asc")
  writeLines("dummy", f1)

  dummy_reader <- function(path) {
    list(
      fixations = dplyr::tibble(
        trial_nr = 1L,
        start_time = 10L,
        end_time = 20L,
        duration = 10L,
        avg_x = 110,
        avg_y = 502
      ),
      word_boundaries = dplyr::tibble(
        trial_nr = 1L,
        word_id = 1L,
        word = "The",
        x_start = 90,
        x_end = 150,
        y_start = 450,
        y_end = 550
      ),
      trial_db = dplyr::tibble(
        trial_nr = 1L,
        t_display_on = 0L,
        t_display_off = 100L
      )
    )
  }

  out <- process_subject_files(
    paths = f1,
    subject_ids = "s1",
    read_fun = dummy_reader
  )

  expect_true(is.data.frame(out$fixations))
  expect_equal(unique(out$fixations$subject), "s1")
  expect_true(is.data.frame(out$measures))
})
