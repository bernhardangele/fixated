## Tests for read_eyelogic()

eyelogic_file <- system.file("extdata", "example_eyelogic.csv", package = "fixated")

# ---------------------------------------------------------------------------
# Basic structure
# ---------------------------------------------------------------------------

test_that("read_eyelogic returns a named list with expected elements", {
  skip_if_not(file.exists(eyelogic_file), "example_eyelogic.csv not found")
  result <- read_eyelogic(eyelogic_file)
  expect_type(result, "list")
  expect_true(all(c("samples", "events", "word_boundaries", "trial_db",
                    "calibration") %in% names(result)))
  expect_null(result$events)
  expect_null(result$calibration)
})

test_that("read_eyelogic samples tibble has expected columns", {
  skip_if_not(file.exists(eyelogic_file), "example_eyelogic.csv not found")
  result <- read_eyelogic(eyelogic_file)
  expect_true(all(c("time", "x", "y", "pupil", "eye") %in% names(result$samples)))
  expect_true(nrow(result$samples) > 0L)
})

# ---------------------------------------------------------------------------
# trial_db: 1-based trial_nr (fix for off-by-one with word_boundaries)
# ---------------------------------------------------------------------------

test_that("read_eyelogic trial_db uses 1-based trial_nr matching word_boundaries", {
  skip_if_not(file.exists(eyelogic_file), "example_eyelogic.csv not found")
  result <- read_eyelogic(eyelogic_file)
  tdb <- result$trial_db
  wb  <- result$word_boundaries

  # trial_db should start at 1, not 0
  expect_equal(min(tdb$trial_nr), 1L)

  # word_boundaries trial_nr range should match trial_db trial_nr range
  expect_equal(sort(unique(wb$trial_nr)), sort(unique(tdb$trial_nr)))
})

test_that("read_eyelogic trial_db sentence_nr matches word_boundaries sentence_nr for same trial_nr", {
  skip_if_not(file.exists(eyelogic_file), "example_eyelogic.csv not found")
  result <- read_eyelogic(eyelogic_file)
  tdb <- result$trial_db
  wb  <- result$word_boundaries

  # For each trial_nr present in both, the sentence_nr must agree
  common_trials <- intersect(tdb$trial_nr, wb$trial_nr)
  expect_true(length(common_trials) > 0L)

  for (tr in common_trials) {
    db_sn <- tdb$sentence_nr[tdb$trial_nr == tr]
    wb_sn <- unique(wb$sentence_nr[wb$trial_nr == tr])
    expect_equal(db_sn, wb_sn,
                 info = paste("trial_nr", tr, ": sentence_nr mismatch"))
  }
})

test_that("read_eyelogic trial_db has correct sentence_nrs from TRIALID", {
  skip_if_not(file.exists(eyelogic_file), "example_eyelogic.csv not found")
  result <- read_eyelogic(eyelogic_file)
  tdb <- result$trial_db
  expect_true(5L  %in% tdb$sentence_nr)
  expect_true(10L %in% tdb$sentence_nr)
})

# ---------------------------------------------------------------------------
# word_boundaries
# ---------------------------------------------------------------------------

test_that("read_eyelogic word_boundaries has expected columns", {
  skip_if_not(file.exists(eyelogic_file), "example_eyelogic.csv not found")
  result <- read_eyelogic(eyelogic_file)
  wb <- result$word_boundaries
  expect_false(is.null(wb))
  expect_true(all(c("trial_nr", "sentence_nr", "word_id", "word",
                    "x_start", "x_end", "y_start", "y_end") %in% names(wb)))
})

test_that("read_eyelogic word_boundaries x_start and y_start are non-NA", {
  skip_if_not(file.exists(eyelogic_file), "example_eyelogic.csv not found")
  result <- read_eyelogic(eyelogic_file)
  wb <- result$word_boundaries
  expect_false(any(is.na(wb$x_start)))
  expect_false(any(is.na(wb$y_start)))
  expect_false(any(is.na(wb$y_end)))
})

# ---------------------------------------------------------------------------
# samples get 1-based trial_nr consistent with trial_db
# ---------------------------------------------------------------------------

test_that("read_eyelogic samples trial_nr is consistent with trial_db trial_nr", {
  skip_if_not(file.exists(eyelogic_file), "example_eyelogic.csv not found")
  result <- read_eyelogic(eyelogic_file)
  spl <- result$samples
  tdb <- result$trial_db

  if ("trial_nr" %in% names(spl)) {
    sample_trials <- sort(unique(na.omit(spl$trial_nr)))
    db_trials     <- sort(unique(tdb$trial_nr))
    # Every non-NA trial_nr in samples must exist in trial_db
    expect_true(all(sample_trials %in% db_trials))
    # All trial_nrs must be >= 1 (1-based)
    expect_true(all(sample_trials >= 1L))
  }
})
