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

test_that("read_eyelogic character_boundaries is a tibble when character data present", {
  result <- read_eyelogic(eyelogic_file)
  expect_false(is.null(result$character_boundaries))
  expect_s3_class(result$character_boundaries, "tbl_df")
})

test_that("read_eyelogic character_boundaries has expected columns", {
  result <- read_eyelogic(eyelogic_file)
  cb <- result$character_boundaries
  expect_true(all(c("trial_nr", "sentence_nr", "word_id", "char_id", "char",
                    "x_start", "x_end", "y_start", "y_end") %in% names(cb)))
})

test_that("read_eyelogic character_boundaries trial_nr matches trial_db trial_nr", {
  result <- read_eyelogic(eyelogic_file)
  cb_trials  <- sort(unique(result$character_boundaries$trial_nr))
  db_trials  <- sort(unique(result$trial_db$trial_nr))
  expect_equal(cb_trials, db_trials)
})

test_that("read_eyelogic character_boundaries char_id is sequential per word", {
  result <- read_eyelogic(eyelogic_file)
  cb <- result$character_boundaries
  for (tr in unique(cb$trial_nr)) {
    trial_cb <- cb[cb$trial_nr == tr, ]
    for (wid in unique(trial_cb$word_id)) {
      chars <- trial_cb[trial_cb$word_id == wid, ]
      expect_equal(chars$char_id, seq_len(nrow(chars)),
                   info = sprintf("trial %d word %d char_id not sequential", tr, wid))
    }
  }
})

test_that("read_eyelogic character_boundaries x_end matches last character of word", {
  result <- read_eyelogic(eyelogic_file)
  cb <- result$character_boundaries
  wb <- result$word_boundaries
  # The x_end of the last character of each word should equal the word's x_end
  for (tr in unique(cb$trial_nr)) {
    trial_cb <- cb[cb$trial_nr == tr, ]
    trial_wb <- wb[wb$trial_nr == tr, ]
    for (wid in unique(trial_cb$word_id)) {
      last_char_xend <- max(trial_cb$x_end[trial_cb$word_id == wid])
      word_xend      <- trial_wb$x_end[trial_wb$word_id == wid]
      expect_equal(last_char_xend, word_xend,
                   info = sprintf("trial %d word %d last-char x_end != word x_end", tr, wid))
    }
  }
})

test_that("read_eyelogic character_boundaries is NULL when no character messages", {
  # Create a temp file without CHARACTER lines
  tmp <- tempfile(fileext = ".csv")
  lines <- readLines(eyelogic_file)
  writeLines(lines[!grepl("CHARACTER", lines)], tmp)
  on.exit(unlink(tmp))
  result <- read_eyelogic(tmp)
  expect_null(result$character_boundaries)
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
