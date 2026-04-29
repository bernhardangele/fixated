test_that("get_roi_from_ocr errors when tesseract is unavailable", {
  skip_if(requireNamespace("tesseract", quietly = TRUE),
          "tesseract is installed; cannot test missing-package error")
  expect_error(
    get_roi_from_ocr("some_image.png"),
    "tesseract"
  )
})

test_that("get_roi_from_ocr errors on non-existent file", {
  skip_if_not_installed("tesseract")
  expect_error(get_roi_from_ocr("/nonexistent/path/image.png"))
})

# Helper: create a minimal PNG with some text using base R graphics
.make_test_png <- function(path, text = "Hello world") {
  grDevices::png(path, width = 400, height = 80, bg = "white")
  graphics::par(mar = c(0, 0, 0, 0))
  graphics::plot.new()
  graphics::text(0.5, 0.5, text, cex = 2, family = "sans")
  grDevices::dev.off()
  invisible(path)
}

test_that("get_roi_from_ocr returns word-level tibble with correct columns", {
  skip_if_not_installed("tesseract")
  skip_if_not_installed("grDevices")

  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp))
  .make_test_png(tmp)

  result <- get_roi_from_ocr(tmp, trial_nr = 3L)

  expect_s3_class(result, "tbl_df")
  expect_named(result, c("trial_nr", "word_id", "word",
                          "x_start", "x_end", "y_start", "y_end"),
               ignore.order = FALSE)
  expect_type(result$trial_nr, "integer")
  expect_type(result$word_id,  "integer")
  expect_type(result$word,     "character")
  expect_type(result$x_start,  "double")
  expect_type(result$x_end,    "double")
  expect_type(result$y_start,  "double")
  expect_type(result$y_end,    "double")

  # All rows must carry the supplied trial_nr
  if (nrow(result) > 0L) {
    expect_true(all(result$trial_nr == 3L))
    expect_equal(result$word_id, seq_len(nrow(result)))
  }
})

test_that("get_roi_from_ocr returns character-level tibble with correct columns", {
  skip_if_not_installed("tesseract")
  skip_if_not_installed("grDevices")

  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp))
  .make_test_png(tmp)

  result <- get_roi_from_ocr(tmp, trial_nr = 1L, level = "character")

  expect_s3_class(result, "tbl_df")
  expect_named(result, c("trial_nr", "word_id", "char_id", "char",
                          "x_start", "x_end", "y_start", "y_end"),
               ignore.order = FALSE)
  expect_type(result$trial_nr, "integer")
  expect_type(result$word_id,  "integer")
  expect_type(result$char_id,  "integer")
  expect_type(result$char,     "character")

  if (nrow(result) > 0L) {
    expect_true(all(result$trial_nr == 1L))
    # char_id should be 1-based within each word
    for (wid in unique(result$word_id)) {
      chars_in_word <- result[result$word_id == wid, ]
      expect_equal(chars_in_word$char_id, seq_len(nrow(chars_in_word)))
    }
    # x_end of each char >= x_start
    expect_true(all(result$x_end >= result$x_start))
  }
})

test_that("get_roi_from_ocr empty result has correct schema (word)", {
  skip_if_not_installed("tesseract")
  skip_if_not_installed("grDevices")

  # A blank white image should yield no detections
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp))
  grDevices::png(tmp, width = 100, height = 50, bg = "white")
  graphics::plot.new()
  grDevices::dev.off()

  result <- get_roi_from_ocr(tmp, trial_nr = 1L, min_confidence = 101)

  expect_s3_class(result, "tbl_df")
  expect_named(result, c("trial_nr", "word_id", "word",
                          "x_start", "x_end", "y_start", "y_end"))
  expect_equal(nrow(result), 0L)
})

test_that("get_roi_from_ocr empty result has correct schema (character)", {
  skip_if_not_installed("tesseract")
  skip_if_not_installed("grDevices")

  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp))
  grDevices::png(tmp, width = 100, height = 50, bg = "white")
  graphics::plot.new()
  grDevices::dev.off()

  result <- get_roi_from_ocr(tmp, trial_nr = 1L, level = "character",
                              min_confidence = 101)

  expect_s3_class(result, "tbl_df")
  expect_named(result, c("trial_nr", "word_id", "char_id", "char",
                          "x_start", "x_end", "y_start", "y_end"))
  expect_equal(nrow(result), 0L)
})

test_that("get_roi_from_ocr accepts a pre-built engine", {
  skip_if_not_installed("tesseract")
  skip_if_not_installed("grDevices")

  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp))
  .make_test_png(tmp)

  eng    <- tesseract::tesseract("eng")
  result <- get_roi_from_ocr(tmp, trial_nr = 2L, engine = eng)

  expect_s3_class(result, "tbl_df")
  if (nrow(result) > 0L) {
    expect_true(all(result$trial_nr == 2L))
  }
})

test_that("get_roi_from_ocr trial_nr is coerced to integer", {
  skip_if_not_installed("tesseract")
  skip_if_not_installed("grDevices")

  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp))
  .make_test_png(tmp)

  result <- get_roi_from_ocr(tmp, trial_nr = 5)  # numeric, not integer
  if (nrow(result) > 0L) {
    expect_type(result$trial_nr, "integer")
    expect_true(all(result$trial_nr == 5L))
  }
})
