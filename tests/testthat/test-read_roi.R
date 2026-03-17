test_that("read_roi returns a tibble with required columns", {
  roi_file <- system.file("extdata", "example_roi.csv", package = "fixated")
  result <- read_roi(roi_file)

  expect_s3_class(result, "tbl_df")
  expected_cols <- c("trial_nr", "word_id", "x_start", "x_end", "y_start", "y_end")
  expect_true(all(expected_cols %in% names(result)))
})

test_that("read_roi word_id is integer", {
  roi_file <- system.file("extdata", "example_roi.csv", package = "fixated")
  result <- read_roi(roi_file)

  expect_type(result$word_id, "integer")
})

test_that("read_roi includes word column when present", {
  roi_file <- system.file("extdata", "example_roi.csv", package = "fixated")
  result <- read_roi(roi_file)

  expect_true("word" %in% names(result))
})

test_that("read_roi sorts by trial then word_id", {
  roi_file <- system.file("extdata", "example_roi.csv", package = "fixated")
  result <- read_roi(roi_file)

  expect_equal(result, dplyr::arrange(result, .data$trial_nr, .data$word_id))
})

test_that("read_roi supports col_map renaming", {
  tmp <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp))
  writeLines(
    c("sent,pos,text,left,right,top,bottom",
      "1,1,The,100,195,380,420",
      "1,2,cat,200,310,380,420"),
    tmp
  )
  result <- read_roi(
    tmp,
    col_map = c(
      trial_nr = "sent",
      word_id = "pos",
      word    = "text",
      x_start = "left",
      x_end   = "right",
      y_start = "top",
      y_end   = "bottom"
    )
  )
  expect_true(all(c("trial_nr", "word_id", "word", "x_start", "x_end", "y_start", "y_end") %in% names(result)))
  expect_equal(nrow(result), 2L)
})

test_that("read_roi errors on missing required column", {
  tmp <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp))
  writeLines(c("trial,word_id,x_start,x_end,y_start", "1,1,100,200,380"), tmp)
  expect_error(read_roi(tmp))
})

test_that("read_roi errors on missing file", {
  expect_error(read_roi("/nonexistent/file.csv"))
})
