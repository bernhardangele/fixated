# Tests for read_eyelogic()

eyelogic_file <- system.file("extdata", "example_eyelogic.csv", package = "fixated")

test_that("read_eyelogic returns a named list with expected elements", {
  result <- read_eyelogic(eyelogic_file)
  expect_type(result, "list")
  expect_true(all(c("samples", "word_boundaries", "trial_db") %in% names(result)))
})

test_that("read_eyelogic trial_db has one row per trial", {
  result <- read_eyelogic(eyelogic_file)
  expect_equal(nrow(result$trial_db), 2L)
})

test_that("read_eyelogic trial_nr is 1-based to match word_boundaries TRIAL numbers", {
  result <- read_eyelogic(eyelogic_file)
  expect_equal(sort(result$trial_db$trial_nr), c(1L, 2L))
})

test_that("read_eyelogic extracts correct sentence_nr from TRIALID messages", {
  result <- read_eyelogic(eyelogic_file)
  expect_equal(result$trial_db$sentence_nr, c(182L, 193L))
})

test_that("read_eyelogic word_boundaries trial_nr matches trial_db trial_nr", {
  result <- read_eyelogic(eyelogic_file)
  wb_trials <- sort(unique(result$word_boundaries$trial_nr))
  db_trials <- sort(unique(result$trial_db$trial_nr))
  expect_equal(wb_trials, db_trials)
})

test_that("read_eyelogic word_boundaries sentence_nr matches trial_db sentence_nr", {
  result <- read_eyelogic(eyelogic_file)
  wb <- result$word_boundaries
  tdb <- result$trial_db
  # For each trial, sentence_nr in wb and tdb must agree and must be unique
  for (tr in tdb$trial_nr) {
    wb_sn  <- unique(wb$sentence_nr[wb$trial_nr == tr])
    tdb_sn <- tdb$sentence_nr[tdb$trial_nr == tr]
    expect_length(wb_sn, 1L)
    expect_equal(wb_sn, tdb_sn,
                 info = sprintf("trial_nr %d: wb sentence_nr=%s, tdb sentence_nr=%s",
                                tr, wb_sn, tdb_sn))
  }
})

test_that("merge_opensesame_csv warns on duplicate sentence_number in CSV", {
  # Create a tiny CSV with a duplicate sentence_number
  tmp_csv <- tempfile(fileext = ".csv")
  writeLines(
    c('sentence_number,label',
      '182,high',
      '182,low',   # duplicate
      '193,high'),
    tmp_csv
  )
  on.exit(unlink(tmp_csv))
  expect_warning(
    read_eyelogic(eyelogic_file, opensesame_csv_path = tmp_csv),
    regexp = "duplicate sentence_number"
  )
})
