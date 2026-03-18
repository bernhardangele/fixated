library(testthat)
library(fixated)

test_that("read_asc adds sentence_nr to all tables", {
  asc_file <- system.file("extdata", "sub_1_example.asc", package = "fixated")
  skip_if(asc_file == "")
  
  res <- read_asc(asc_file)
  
  # Check trial_db
  expect_true("sentence_nr" %in% names(res$trial_db))
  expect_false("item_nr" %in% names(res$trial_db))
  
  # Check samples
  expect_true("sentence_nr" %in% names(res$samples))
  # Check if sentence_nr is NA when trial_nr is NA
  if (any(is.na(res$samples$trial_nr))) {
    na_trials <- res$samples[is.na(res$samples$trial_nr), ]
    expect_true(all(is.na(na_trials$sentence_nr)))
  }
  
  # Check events
  expect_true("sentence_nr" %in% names(res$events))
  if (any(is.na(res$events$trial_nr))) {
    na_trials_ev <- res$events[is.na(res$events$trial_nr), ]
    expect_true(all(is.na(na_trials_ev$sentence_nr)))
  }
  
  # Check word_boundaries
  expect_true("sentence_nr" %in% names(res$word_boundaries))
  expect_equal(names(res$word_boundaries)[1:2], c("trial_nr", "sentence_nr"))
  
  # Verify mapping
  trial_nr_0_sent <- res$trial_db$sentence_nr[res$trial_db$trial_nr == 0]
  if (length(trial_nr_0_sent) > 0) {
    sample_sent <- unique(res$samples$sentence_nr[res$samples$trial_nr == 0 & !is.na(res$samples$trial_nr)])
    expect_equal(sample_sent, trial_nr_0_sent)
    
    event_sent <- unique(res$events$sentence_nr[res$events$trial_nr == 0 & !is.na(res$events$trial_nr)])
    expect_equal(event_sent, trial_nr_0_sent)
  }
})
