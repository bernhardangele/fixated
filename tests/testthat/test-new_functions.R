## Tests for new functions added from scripts_to_incorporate

# ---------------------------------------------------------------------------
# read_asc() – binocular and word-boundary enhancements
# ---------------------------------------------------------------------------

test_that("read_asc returns four-element list for OpenSesame binocular file", {
  asc_file <- system.file("extdata", "sub_1_example.asc", package = "fixated")
  skip_if_not(file.exists(asc_file), "sub_1_example.asc not found")
  result <- read_asc(asc_file)
  expect_type(result, "list")
  expect_true(all(c("samples", "events", "word_boundaries", "calibration") %in%
                    names(result)))
})

test_that("read_asc binocular file produces L and R rows in samples", {
  asc_file <- system.file("extdata", "sub_1_example.asc", package = "fixated")
  skip_if_not(file.exists(asc_file), "sub_1_example.asc not found")
  result <- read_asc(asc_file)
  expect_true("L" %in% result$samples$eye)
  expect_true("R" %in% result$samples$eye)
})

test_that("read_asc eye filter works on binocular file", {
  asc_file <- system.file("extdata", "sub_1_example.asc", package = "fixated")
  skip_if_not(file.exists(asc_file), "sub_1_example.asc not found")
  result_r <- read_asc(asc_file, eyes = "R")
  expect_true(all(result_r$samples$eye == "R"))
  result_l <- read_asc(asc_file, eyes = "L")
  expect_true(all(result_l$samples$eye == "L"))
})

test_that("read_asc parses word_boundaries from binocular file", {
  asc_file <- system.file("extdata", "sub_1_example.asc", package = "fixated")
  skip_if_not(file.exists(asc_file), "sub_1_example.asc not found")
  result <- read_asc(asc_file)
  wb <- result$word_boundaries
  expect_true(!is.null(wb))
  expect_true(all(c("trial_nr", "sentence_nr", "word_id", "word",
                    "x_end") %in% names(wb)))
  expect_true(nrow(wb) > 0L)
  expect_equal(wb$word_id[[1L]], 1L)
})

test_that("read_asc parses calibration from binocular file", {
  asc_file <- system.file("extdata", "sub_1_example.asc", package = "fixated")
  skip_if_not(file.exists(asc_file), "sub_1_example.asc not found")
  result <- read_asc(asc_file)
  cal <- result$calibration
  expect_true(!is.null(cal))
  expect_true(all(c("eye", "quality", "avg_error", "max_error",
                    "offset_deg") %in% names(cal)))
  expect_true(nrow(cal) == 2L)  # LEFT and RIGHT
  expect_true(all(cal$eye %in% c("LEFT", "RIGHT")))
})

test_that("read_asc returns NULL word_boundaries for monocular example", {
  asc_file <- system.file("extdata", "example.asc", package = "fixated")
  skip_if_not(file.exists(asc_file), "example.asc not found")
  result <- read_asc(asc_file)
  expect_null(result$word_boundaries)
})

test_that("read_asc backward compatible: samples and events still present", {
  asc_file <- system.file("extdata", "example.asc", package = "fixated")
  skip_if_not(file.exists(asc_file), "example.asc not found")
  result <- read_asc(asc_file)
  expect_true("samples" %in% names(result))
  expect_true("events"  %in% names(result))
  expect_true(nrow(result$samples) > 0L)
})

# ---------------------------------------------------------------------------
# blink_overlap_or_near()
# ---------------------------------------------------------------------------

test_that("blink_overlap_or_near detects overlap", {
  expect_true(blink_overlap_or_near(100, 200, 150, 250))
})

test_that("blink_overlap_or_near detects proximity within threshold", {
  # Fixation ends at 200, blink starts at 250 → gap = 50 ms
  expect_true(blink_overlap_or_near(100, 200, 250, 350, t_diff_threshold = 100))
})

test_that("blink_overlap_or_near returns FALSE for distant blink", {
  expect_false(blink_overlap_or_near(100, 200, 400, 500, t_diff_threshold = 100))
})

test_that("blink_overlap_or_near works vectorised", {
  result <- blink_overlap_or_near(100, 200,
                                  blink_start = c(150, 400),
                                  blink_end   = c(250, 500),
                                  t_diff_threshold = 50)
  expect_equal(result, c(TRUE, FALSE))
})

# ---------------------------------------------------------------------------
# mark_fixations_close_to_blinks()
# ---------------------------------------------------------------------------

test_that("mark_fixations_close_to_blinks adds near_blink column", {
  fixations <- dplyr::tibble(
    t_start = c(100L, 500L, 900L),
    t_end   = c(300L, 700L, 1100L)
  )
  blinks <- dplyr::tibble(t_start = 280L, t_end = 350L)
  result <- mark_fixations_close_to_blinks(fixations, blinks)
  expect_true("near_blink" %in% names(result))
  expect_equal(nrow(result), 3L)
})

test_that("mark_fixations_close_to_blinks correctly flags near fixations", {
  fixations <- dplyr::tibble(
    t_start = c(100L, 500L, 900L),
    t_end   = c(300L, 700L, 1100L)
  )
  blinks <- dplyr::tibble(t_start = 280L, t_end = 350L)
  result <- mark_fixations_close_to_blinks(fixations, blinks,
                                           t_diff_threshold = 100)
  expect_true(result$near_blink[[1L]])   # fixation 1 ends at 300, blink at 280
  expect_false(result$near_blink[[2L]])  # fixation 2 starts at 500 (> 100 ms gap)
  expect_false(result$near_blink[[3L]])
})

# ---------------------------------------------------------------------------
# categorize_fixation()
# ---------------------------------------------------------------------------

test_that("categorize_fixation labels simple left-to-right reading", {
  result <- categorize_fixation(c(1L, 2L, 3L, 4L))
  expect_equal(result, c("first pass", "first pass", "first pass", "first pass"))
})

test_that("categorize_fixation labels refixation", {
  result <- categorize_fixation(c(1L, 2L, 2L, 3L))
  expect_equal(result[[3L]], "first pass refixation")
})

test_that("categorize_fixation labels regression as other fixation", {
  result <- categorize_fixation(c(1L, 2L, 3L, 2L))
  expect_equal(result[[4L]], "other fixation")
})

test_that("categorize_fixation output has same length as input", {
  seq_vec <- c(1L, 1L, 2L, 3L, 2L, 4L)
  expect_equal(length(categorize_fixation(seq_vec)), length(seq_vec))
})

# ---------------------------------------------------------------------------
# get_word_info_from_msg()
# ---------------------------------------------------------------------------

test_that("get_word_info_from_msg returns expected columns", {
  msgs <- c(
    "MSG\t100 TRIAL 0 ITEM 5 WORD 1 The RIGHT_BOUNDARY 180",
    "MSG\t100 TRIAL 0 ITEM 5 WORD 2 quick RIGHT_BOUNDARY 310"
  )
  result <- get_word_info_from_msg(msgs)
  expect_true(all(c("trial_nr", "sentence_nr", "word_id", "word",
                    "x_end") %in% names(result)))
  expect_equal(nrow(result), 2L)
})

test_that("get_word_info_from_msg parses word text and boundary correctly", {
  msgs <- c("MSG\t100 TRIAL 0 ITEM 5 WORD 1 The RIGHT_BOUNDARY 180")
  result <- get_word_info_from_msg(msgs)
  expect_equal(result$word[[1L]], "The")
  expect_equal(result$x_end[[1L]], 180L)
})

test_that("get_word_info_from_msg returns empty tibble for non-matching input", {
  result <- get_word_info_from_msg(c("MSG\t100 var condition high"))
  expect_equal(nrow(result), 0L)
})

# ---------------------------------------------------------------------------
# get_trial_info_from_msg()
# ---------------------------------------------------------------------------

test_that("get_trial_info_from_msg extracts value from single match", {
  msgs <- c("MSG\t1000 var subject_nr 42", "MSG\t1001 var condition high")
  result <- get_trial_info_from_msg(msgs, "MSG\\t\\d+ var subject_nr (\\d+)")
  expect_equal(result, "42")
})

test_that("get_trial_info_from_msg returns NA with warning for no match", {
  msgs <- c("MSG\t1000 var condition high")
  expect_warning(
    result <- get_trial_info_from_msg(msgs, "MSG\\t\\d+ var subject_nr (\\d+)"),
    "does not match"
  )
  expect_true(is.na(result))
})

test_that("get_trial_info_from_msg errors on multiple matches when not allowed", {
  msgs <- c("MSG\t1000 var x 1", "MSG\t1001 var x 2")
  expect_error(
    get_trial_info_from_msg(msgs, "MSG\\t\\d+ var x (\\d+)"),
    "multiple lines"
  )
})

# ---------------------------------------------------------------------------
# calculate_fixation_time_measures()
# ---------------------------------------------------------------------------

make_simple_fixations <- function() {
  dplyr::tibble(
    word_id    = c(1L, 2L, 3L, 2L),
    duration   = c(200L, 150L, 180L, 120L),
    too_short  = FALSE,
    too_long   = FALSE,
    merged     = FALSE,
    near_blink = FALSE
  )
}

make_simple_words <- function() {
  dplyr::tibble(
    word_id               = 1:3,
    word                  = c("The", "quick", "fox"),
    x_end                 = c(200L, 360L, 500L)
  )
}

test_that("calculate_fixation_time_measures returns expected columns", {
  result <- calculate_fixation_time_measures(
    make_simple_fixations(), make_simple_words()
  )
  expected <- c("word_id", "word", "ffd", "gd", "sfd", "gopast", "tvt")
  expect_true(all(expected %in% names(result)))
})

test_that("calculate_fixation_time_measures FFD is first-pass first duration", {
  result <- calculate_fixation_time_measures(
    make_simple_fixations(), make_simple_words()
  )
  w2 <- result[result$word_id == 2L, ]
  expect_equal(w2$ffd, 150L)
})

test_that("calculate_fixation_time_measures TVT sums all durations on word", {
  result <- calculate_fixation_time_measures(
    make_simple_fixations(), make_simple_words()
  )
  w2 <- result[result$word_id == 2L, ]
  # Word 2 fixated at duration 150 (first pass) + 120 (regression) = 270
  expect_equal(w2$tvt, 270L)
})

test_that("calculate_fixation_time_measures has one row per word", {
  result <- calculate_fixation_time_measures(
    make_simple_fixations(), make_simple_words()
  )
  expect_equal(nrow(result), 3L)
})

test_that("calculate_fixation_time_measures SFD equals FFD when single fixation", {
  fixations <- dplyr::tibble(
    word_id    = c(1L, 2L, 3L),
    duration   = c(200L, 150L, 180L),
    too_short  = FALSE,
    too_long   = FALSE,
    merged     = FALSE,
    near_blink = FALSE
  )
  result <- calculate_fixation_time_measures(fixations, make_simple_words())
  w1 <- result[result$word_id == 1L, ]
  expect_equal(w1$sfd, w1$ffd)
})

# ---------------------------------------------------------------------------
# merge_fixations()
# ---------------------------------------------------------------------------

test_that("merge_fixations returns data frame with required columns", {
  fixations <- dplyr::tibble(
    t_start  = c(100L, 400L, 450L, 600L),
    t_end    = c(370L, 440L, 580L, 800L),
    duration = c(270L,  40L, 130L, 200L),
    x        = c(300, 310,  320, 600),
    y        = c(540, 541,  540, 540)
  )
  result <- merge_fixations(fixations, max_duration = 80, max_distance = 20,
                            rate = 1000, add_eyelink_correction = FALSE)
  expect_true("merged" %in% names(result))
  expect_true(all(c("t_start", "t_end", "duration") %in% names(result)))
})

test_that("merge_fixations merges short fixation with nearby previous", {
  fixations <- dplyr::tibble(
    t_start  = c(100L, 400L, 420L),
    t_end    = c(370L, 415L, 600L),
    duration = c(270L,  15L, 180L),
    x        = c(300, 305, 600),
    y        = c(540, 541, 540)
  )
  result <- merge_fixations(fixations, max_duration = 80, max_distance = 20,
                            rate = 1000, add_eyelink_correction = FALSE)
  expect_true(any(result$merged))
})

test_that("merge_fixations errors on missing required columns", {
  bad_df <- dplyr::tibble(t_start = 1:3, t_end = 4:6)
  expect_error(merge_fixations(bad_df), "missing required columns")
})

# ---------------------------------------------------------------------------
# assign_word_info()
# ---------------------------------------------------------------------------

test_that("assign_word_info adds word_id and word columns", {
  fixations <- dplyr::tibble(x = c(160, 300), y = c(540, 540))
  wb <- dplyr::tibble(
    word_id               = 1:3,
    word                  = c("The", "quick", "fox"),
    x_end                 = c(200L, 360L, 500L),
    sentence_nr           = 1L,
    trial_nr              = 1L
  )
  result <- assign_word_info(fixations, wb)
  expect_true(all(c("word_id", "word") %in% names(result)))
  expect_equal(result$word_id[[1L]], 1L)
  expect_equal(result$word[[1L]], "The")
})

test_that("assign_word_info assigns -1 for pre-sentence fixations", {
  fixations <- dplyr::tibble(x = 50, y = 540)
  wb <- dplyr::tibble(
    word_id               = 1:2,
    word                  = c("The", "fox"),
    x_end                 = c(200L, 360L),
    sentence_nr           = 1L,
    trial_nr              = 1L
  )
  result <- assign_word_info(fixations, wb, sentence_left_x_boundary = 125)
  expect_equal(result$word_id[[1L]], -1L)
})

test_that("assign_word_info assigns -99 for post-sentence fixations", {
  fixations <- dplyr::tibble(x = 2000, y = 540)
  wb <- dplyr::tibble(
    word_id               = 1:2,
    word                  = c("The", "fox"),
    x_end                 = c(200L, 360L),
    sentence_nr           = 1L,
    trial_nr              = 1L
  )
  result <- assign_word_info(fixations, wb)
  expect_equal(result$word_id[[1L]], -99L)
})
