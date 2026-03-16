test_that("read_samples_csv returns a tibble with required columns", {
  csv_file <- system.file("extdata", "example_samples.csv", package = "fixated")
  result <- read_samples_csv(csv_file)

  expect_s3_class(result, "tbl_df")
  expect_true(all(c("time", "x", "y") %in% names(result)))
})

test_that("read_samples_csv time column is integer", {
  csv_file <- system.file("extdata", "example_samples.csv", package = "fixated")
  result <- read_samples_csv(csv_file)

  expect_type(result$time, "integer")
})

test_that("read_samples_csv respects eyes filter", {
  csv_file <- system.file("extdata", "example_samples.csv", package = "fixated")
  result <- read_samples_csv(csv_file, eyes = "L")

  expect_true(all(result$eye == "L"))
})

test_that("read_samples_csv supports col_map renaming", {
  # Create a temporary CSV with different column names
  tmp <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp))
  writeLines(
    c("t,gx,gy,p", "100,300,400,1200", "110,301,401,1205"),
    tmp
  )
  result <- read_samples_csv(
    tmp,
    col_map = c(time = "t", x = "gx", y = "gy", pupil = "p",
                eye = NA, trial = NA, participant = NA)
  )
  expect_true(all(c("time", "x", "y", "pupil") %in% names(result)))
  expect_equal(result$time, c(100L, 110L))
})

test_that("read_samples_csv errors on missing required column", {
  tmp <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp))
  writeLines(c("time,y", "100,400"), tmp)
  expect_error(read_samples_csv(tmp))
})

test_that("read_samples_csv errors on missing file", {
  expect_error(read_samples_csv("/nonexistent/file.csv"))
})
