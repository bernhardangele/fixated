library(testthat)
library(fixated)

test_that("compute_eye_measures includes sentence_nr when present in ROI", {
  roi <- dplyr::tibble(
    trial_nr    = 1L,
    sentence_nr = 5L,
    word_id     = 1:2,
    word        = c("The", "fox"),
    x_start     = c(100, 200),
    x_end       = c(190, 290),
    y_start     = c(400, 400),
    y_end       = c(440, 440)
  )
  
  fixations <- dplyr::tibble(
    trial_nr   = 1L,
    start_time = c(0L, 200L),
    end_time   = c(150L, 350L),
    duration   = c(150L, 150L),
    avg_x      = c(145, 245),
    avg_y      = c(420, 420),
    eye        = "R"
  )
  
  measures <- compute_eye_measures(fixations, roi)
  
  expect_true("sentence_nr" %in% names(measures))
  expect_equal(measures$sentence_nr[[1L]], 5L)
  expect_equal(names(measures)[1:3], c("trial_nr", "sentence_nr", "eye"))
})

test_that("read_roi parses sentence_nr", {
  csv_content <- "trial_nr,sentence_nr,word_id,word,x_start,x_end,y_start,y_end\n1,5,1,The,100,190,400,440"
  tmp_csv <- tempfile(fileext = ".csv")
  writeLines(csv_content, tmp_csv)
  
  roi <- read_roi(tmp_csv)
  expect_true("sentence_nr" %in% names(roi))
  expect_equal(roi$sentence_nr[[1L]], 5L)
  
  unlink(tmp_csv)
})
