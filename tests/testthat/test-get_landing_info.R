# Tests for get_landing_info()

# ---- helper data -----------------------------------------------------------

make_fixations <- function() {
  dplyr::tibble(
    trial_nr   = c(1L, 1L, 1L, 1L, 2L),
    start_time = c(100L, 300L, 500L, 700L, 100L),
    avg_x      = c(150, 600, 150, 600, 150),
    eye        = c("R", "R", "R", "R", "R")
  )
}

make_word_boundaries <- function() {
  dplyr::tibble(
    trial_nr = c(1L, 1L, 2L, 2L),
    word_id  = c(1L, 2L, 1L, 2L),
    word     = c("Hello", "world", "Hello", "world"),
    x_start  = c(100, 500, 100, 500),
    x_end    = c(300, 800, 300, 800)
  )
}

# ---- word_id assignment ----------------------------------------------------

test_that("get_landing_info assigns correct word_id by avg_x", {
  fix <- make_fixations()
  wb  <- make_word_boundaries()
  result <- get_landing_info(fix, wb)
  expect_equal(result$word_id[result$trial_nr == 1L & result$avg_x == 150],
               c(1L, 1L))
  expect_equal(result$word_id[result$trial_nr == 1L & result$avg_x == 600],
               c(2L, 2L))
})

test_that("get_landing_info returns NA word_id when avg_x outside all boundaries", {
  fix <- dplyr::tibble(trial_nr = 1L, start_time = 100L, avg_x = 999)
  wb  <- dplyr::tibble(trial_nr = 1L, word_id = 1L, word = "Hi",
                       x_start = 100, x_end = 300)
  result <- get_landing_info(fix, wb)
  expect_true(is.na(result$word_id))
})

test_that("get_landing_info returns NA columns when avg_x is NA", {
  fix <- dplyr::tibble(trial_nr = 1L, start_time = 100L, avg_x = NA_real_)
  wb  <- dplyr::tibble(trial_nr = 1L, word_id = 1L, word = "Hi",
                       x_start = 100, x_end = 300)
  result <- get_landing_info(fix, wb)
  expect_true(is.na(result$word_id))
  expect_true(is.na(result$fixation_type))
  expect_true(is.na(result$word_proportion))
  expect_true(is.na(result$fixated_char))
})

test_that("get_landing_info picks smallest-width word when boundaries overlap", {
  # Two words that both contain avg_x=200: word 1 is wider (100-400),
  # word 2 is narrower (150-250). Word 2 should win.
  fix <- dplyr::tibble(trial_nr = 1L, start_time = 100L, avg_x = 200)
  wb  <- dplyr::tibble(
    trial_nr = c(1L, 1L),
    word_id  = c(1L, 2L),
    word     = c("wide", "narrow"),
    x_start  = c(100, 150),
    x_end    = c(400, 250)
  )
  result <- get_landing_info(fix, wb)
  expect_equal(result$word_id, 2L)
})

test_that("get_landing_info tie-breaks on smallest x_start", {
  # Two words with equal width both containing avg_x=200: word with
  # smaller x_start wins.
  fix <- dplyr::tibble(trial_nr = 1L, start_time = 100L, avg_x = 200)
  wb  <- dplyr::tibble(
    trial_nr = c(1L, 1L),
    word_id  = c(2L, 1L),     # shuffled order to test sort robustness
    word     = c("b", "a"),
    x_start  = c(180, 150),
    x_end    = c(280, 250)    # both width = 100
  )
  result <- get_landing_info(fix, wb)
  expect_equal(result$word_id, 1L)  # x_start 150 wins
})

# ---- fixation_type ---------------------------------------------------------

test_that("get_landing_info marks first fixation on word as 'first'", {
  fix <- make_fixations()
  wb  <- make_word_boundaries()
  result <- get_landing_info(fix, wb)
  # First fixation on word 1 in trial 1 (earliest start_time = 100)
  first_on_w1 <- result[result$trial_nr == 1L & result$word_id == 1L &
                           result$start_time == 100L, ]
  expect_equal(first_on_w1$fixation_type, "first")
})

test_that("get_landing_info marks repeated fixation on same word as 'refixation'", {
  fix <- make_fixations()
  wb  <- make_word_boundaries()
  result <- get_landing_info(fix, wb)
  # Second fixation on word 1 in trial 1 (start_time = 500)
  refx_on_w1 <- result[result$trial_nr == 1L & result$word_id == 1L &
                          result$start_time == 500L, ]
  expect_equal(refx_on_w1$fixation_type, "refixation")
})

test_that("get_landing_info fixation_type resets across trials", {
  fix <- make_fixations()
  wb  <- make_word_boundaries()
  result <- get_landing_info(fix, wb)
  # Trial 2 has one fixation on word 1: should be "first", not "refixation"
  trial2 <- result[result$trial_nr == 2L, ]
  expect_equal(trial2$fixation_type, "first")
})

test_that("get_landing_info fixation_type respects eye grouping", {
  fix <- dplyr::tibble(
    trial_nr   = c(1L, 1L, 1L, 1L),
    start_time = c(100L, 300L, 100L, 300L),
    avg_x      = c(150, 150, 150, 150),
    eye        = c("R", "R", "L", "L")
  )
  wb <- dplyr::tibble(
    trial_nr = 1L, word_id = 1L, word = "Hi",
    x_start = 100, x_end = 300
  )
  result <- get_landing_info(fix, wb)
  # Both eyes: first fixation is "first", second is "refixation"
  expect_equal(result$fixation_type[result$eye == "R" & result$start_time == 100L],
               "first")
  expect_equal(result$fixation_type[result$eye == "R" & result$start_time == 300L],
               "refixation")
  expect_equal(result$fixation_type[result$eye == "L" & result$start_time == 100L],
               "first")
  expect_equal(result$fixation_type[result$eye == "L" & result$start_time == 300L],
               "refixation")
})

test_that("get_landing_info sets fixation_type to NA when word_id is NA", {
  fix <- dplyr::tibble(trial_nr = 1L, start_time = 100L, avg_x = 999)
  wb  <- dplyr::tibble(trial_nr = 1L, word_id = 1L, word = "Hi",
                       x_start = 100, x_end = 300)
  result <- get_landing_info(fix, wb)
  expect_true(is.na(result$fixation_type))
})

# ---- word_proportion -------------------------------------------------------

test_that("get_landing_info computes correct word_proportion", {
  # Word spans 100-1100 (width 1000), avg_x = 200 → proportion = 0.10
  fix <- dplyr::tibble(trial_nr = 1L, start_time = 100L, avg_x = 200)
  wb  <- dplyr::tibble(trial_nr = 1L, word_id = 1L, word = "Hello",
                       x_start = 100, x_end = 1100)
  result <- get_landing_info(fix, wb)
  expect_equal(result$word_proportion, 0.10)
})

test_that("get_landing_info clamps word_proportion to [0, 1]", {
  # avg_x slightly outside due to numeric drift should clamp
  fix <- dplyr::tibble(trial_nr = 1L, start_time = 100L,
                       avg_x = 300 + 1e-10)
  wb  <- dplyr::tibble(trial_nr = 1L, word_id = 1L, word = "Hi",
                       x_start = 100, x_end = 300)
  result <- get_landing_info(fix, wb)
  expect_lte(result$word_proportion, 1)
  expect_gte(result$word_proportion, 0)
})

test_that("get_landing_info sets word_proportion to NA for zero-width boundary", {
  fix <- dplyr::tibble(trial_nr = 1L, start_time = 100L, avg_x = 200)
  wb  <- dplyr::tibble(trial_nr = 1L, word_id = 1L, word = "X",
                       x_start = 200, x_end = 200)
  result <- get_landing_info(fix, wb)
  expect_true(is.na(result$word_proportion))
})

test_that("get_landing_info sets word_proportion to NA when avg_x outside boundary", {
  fix <- dplyr::tibble(trial_nr = 1L, start_time = 100L, avg_x = 999)
  wb  <- dplyr::tibble(trial_nr = 1L, word_id = 1L, word = "Hi",
                       x_start = 100, x_end = 300)
  result <- get_landing_info(fix, wb)
  expect_true(is.na(result$word_proportion))
})

# ---- fixated_char ----------------------------------------------------------

test_that("get_landing_info computes correct fixated_char", {
  # 5-letter word, proportion 0.10 → char = floor(0.10 * 5) + 1 = 1
  fix <- dplyr::tibble(trial_nr = 1L, start_time = 100L, avg_x = 200)
  wb  <- dplyr::tibble(trial_nr = 1L, word_id = 1L, word = "Hello",
                       x_start = 100, x_end = 1100)
  result <- get_landing_info(fix, wb)
  expect_equal(result$fixated_char, 1L)
})

test_that("get_landing_info maps proportion 0.9 to last char for 5-letter word", {
  # proportion = 0.9, N = 5 → floor(0.9 * 5) + 1 = floor(4.5) + 1 = 5
  fix <- dplyr::tibble(trial_nr = 1L, start_time = 100L, avg_x = 1000)
  wb  <- dplyr::tibble(trial_nr = 1L, word_id = 1L, word = "Hello",
                       x_start = 100, x_end = 1100)
  result <- get_landing_info(fix, wb)
  expect_equal(result$fixated_char, 5L)
})

test_that("get_landing_info fixated_char is NA when word_proportion is NA", {
  fix <- dplyr::tibble(trial_nr = 1L, start_time = 100L, avg_x = 999)
  wb  <- dplyr::tibble(trial_nr = 1L, word_id = 1L, word = "Hi",
                       x_start = 100, x_end = 300)
  result <- get_landing_info(fix, wb)
  expect_true(is.na(result$fixated_char))
})

test_that("get_landing_info fixated_char is NA when word column absent", {
  fix <- dplyr::tibble(trial_nr = 1L, start_time = 100L, avg_x = 200)
  wb  <- dplyr::tibble(trial_nr = 1L, word_id = 1L,
                       x_start = 100, x_end = 300)
  result <- get_landing_info(fix, wb)
  expect_true(is.na(result$fixated_char))
})

# ---- join-key behavior -----------------------------------------------------

test_that("get_landing_info joins on sentence_nr when trial_nr absent", {
  fix <- dplyr::tibble(sentence_nr = 1L, start_time = 100L, avg_x = 200)
  wb  <- dplyr::tibble(sentence_nr = 1L, word_id = 1L, word = "Hi",
                       x_start = 100, x_end = 300)
  result <- get_landing_info(fix, wb)
  expect_equal(result$word_id, 1L)
})

test_that("get_landing_info warns and cross-joins when no shared key", {
  fix <- dplyr::tibble(start_time = 100L, avg_x = 200)
  wb  <- dplyr::tibble(word_id = 1L, word = "Hi",
                       x_start = 100, x_end = 300)
  expect_warning(
    result <- get_landing_info(fix, wb),
    "cross join"
  )
  expect_equal(result$word_id, 1L)
})

test_that("get_landing_info word_boundaries matching is per trial", {
  # avg_x 200 is in word 1 for trial 1 but word 2 for trial 2
  fix <- dplyr::tibble(trial_nr = c(1L, 2L), start_time = c(100L, 100L),
                       avg_x = c(200, 200))
  wb  <- dplyr::tibble(
    trial_nr = c(1L, 1L, 2L, 2L),
    word_id  = c(1L, 2L, 1L, 2L),
    word     = c("a", "b", "a", "b"),
    x_start  = c(100, 400, 50, 150),
    x_end    = c(350, 700, 140, 300)
  )
  result <- get_landing_info(fix, wb)
  # Trial 1: avg_x 200 in word 1 (100-350); Trial 2: avg_x 200 in word 2 (150-300)
  expect_equal(result$word_id[result$trial_nr == 1L], 1L)
  expect_equal(result$word_id[result$trial_nr == 2L], 2L)
})

# ---- output structure ------------------------------------------------------

test_that("get_landing_info returns a data frame with correct added columns", {
  fix    <- make_fixations()
  wb     <- make_word_boundaries()
  result <- get_landing_info(fix, wb)
  expect_true(is.data.frame(result))
  for (col in c("word_id", "fixation_type", "word_proportion", "fixated_char")) {
    expect_true(col %in% names(result),
                label = paste("column", col, "present"))
  }
  expect_equal(nrow(result), nrow(fix))
})

test_that("get_landing_info preserves original columns", {
  fix    <- make_fixations()
  wb     <- make_word_boundaries()
  result <- get_landing_info(fix, wb)
  for (col in names(fix)) {
    expect_true(col %in% names(result),
                label = paste("original column", col, "preserved"))
  }
})

test_that("get_landing_info preserves original row order", {
  fix <- dplyr::tibble(
    trial_nr   = c(1L, 1L, 1L),
    start_time = c(500L, 100L, 300L),   # out of time order
    avg_x      = c(150, 600, 150)
  )
  wb <- make_word_boundaries()[1:2, ]
  result <- get_landing_info(fix, wb)
  # Rows should match original start_time order, not sorted order
  expect_equal(result$start_time, c(500L, 100L, 300L))
})

# ---- error handling --------------------------------------------------------

test_that("get_landing_info errors when avg_x column missing from fixations", {
  fix <- dplyr::tibble(trial_nr = 1L, start_time = 100L)
  wb  <- dplyr::tibble(trial_nr = 1L, word_id = 1L, x_start = 100, x_end = 300)
  expect_error(get_landing_info(fix, wb), "avg_x")
})

test_that("get_landing_info errors when word_boundaries missing required column", {
  fix <- dplyr::tibble(trial_nr = 1L, avg_x = 200)
  wb  <- dplyr::tibble(trial_nr = 1L, word_id = 1L, x_start = 100)  # no x_end
  expect_error(get_landing_info(fix, wb), "x_end")
})

# ---- character_boundaries option -------------------------------------------

make_char_boundaries <- function() {
  # "Hello" spans x 100-300 with unequal character widths
  # H: 100-130, e: 130-160, l: 160-200, l: 200-240, o: 240-300
  dplyr::tibble(
    trial_nr = rep(1L, 5L),
    word_id  = rep(1L, 5L),
    char_id  = 1:5,
    char     = c("H", "e", "l", "l", "o"),
    x_start  = c(100, 130, 160, 200, 240),
    x_end    = c(130, 160, 200, 240, 300)
  )
}

test_that("get_landing_info uses character_boundaries when provided", {
  # Midpoint of 'l' at index 3 is about 180 (x_start=160, x_end=200)
  fix <- dplyr::tibble(trial_nr = 1L, start_time = 100L, avg_x = 180)
  wb  <- dplyr::tibble(trial_nr = 1L, word_id = 1L, word = "Hello",
                        x_start = 100, x_end = 300)
  cb  <- make_char_boundaries()
  result <- get_landing_info(fix, wb, character_boundaries = cb)
  # With character boundaries, avg_x=180 falls in char 3 (x_start=160, x_end=200)
  expect_equal(result$fixated_char, 3L)
  # Monospace would give floor(0.40 * 5) + 1 = 3 too, so use a position where they differ
})

test_that("get_landing_info character_boundaries overrides monospace", {
  # avg_x = 250 → in char 'o' (char_id=5, x_start=240, x_end=300)
  # Monospace: word_proportion = (250-100)/200 = 0.75, floor(0.75*5)+1 = 4
  # Data-driven: char 5 (x_start=240 <= 250 <= x_end=300)
  fix <- dplyr::tibble(trial_nr = 1L, start_time = 100L, avg_x = 250)
  wb  <- dplyr::tibble(trial_nr = 1L, word_id = 1L, word = "Hello",
                        x_start = 100, x_end = 300)
  cb  <- make_char_boundaries()

  monospace_result <- get_landing_info(fix, wb)
  cb_result        <- get_landing_info(fix, wb, character_boundaries = cb)

  expect_equal(monospace_result$fixated_char, 4L)   # monospace estimate
  expect_equal(cb_result$fixated_char, 5L)           # data-driven estimate
})

test_that("get_landing_info falls back to monospace when avg_x not in any char box", {
  # avg_x = 305 is slightly beyond the last character's x_end (300)
  # but still within the word boundary (x_end = 310)
  fix <- dplyr::tibble(trial_nr = 1L, start_time = 100L, avg_x = 305)
  wb  <- dplyr::tibble(trial_nr = 1L, word_id = 1L, word = "Hello",
                        x_start = 100, x_end = 310)
  # Characters only cover 100-300, so 305 falls in a gap
  cb  <- make_char_boundaries()
  result <- get_landing_info(fix, wb, character_boundaries = cb)
  # Falls back to monospace: word_proportion = (305-100)/210 ≈ 0.976
  # floor(0.976 * 5) + 1 = 5
  expect_equal(result$fixated_char, 5L)
})

test_that("get_landing_info character_boundaries uses trial_nr join key", {
  # Two trials with the same word region but different char widths
  fix <- dplyr::tibble(
    trial_nr   = c(1L, 2L),
    start_time = c(100L, 100L),
    avg_x      = c(250, 250)
  )
  wb <- dplyr::tibble(
    trial_nr = c(1L, 2L),
    word_id  = c(1L, 1L),
    word     = c("Hello", "Hello"),
    x_start  = c(100, 100),
    x_end    = c(300, 300)
  )
  # Trial 1: avg_x=250 → char 5 (x_start=240, x_end=300)
  # Trial 2: avg_x=250 → char 4 (x_start=240, x_end=270) in different char layout
  cb <- dplyr::bind_rows(
    make_char_boundaries(),
    dplyr::tibble(
      trial_nr = rep(2L, 5L),
      word_id  = rep(1L, 5L),
      char_id  = 1:5,
      char     = c("H", "e", "l", "l", "o"),
      x_start  = c(100, 140, 180, 220, 270),
      x_end    = c(140, 180, 220, 270, 300)
    )
  )
  result <- get_landing_info(fix, wb, character_boundaries = cb)
  expect_equal(result$fixated_char[result$trial_nr == 1L], 5L)
  expect_equal(result$fixated_char[result$trial_nr == 2L], 4L)
})

test_that("get_landing_info errors when character_boundaries missing required columns", {
  fix <- dplyr::tibble(trial_nr = 1L, avg_x = 200)
  wb  <- dplyr::tibble(trial_nr = 1L, word_id = 1L, x_start = 100, x_end = 300)
  bad_cb <- dplyr::tibble(word_id = 1L, char_id = 1L, x_start = 100)  # no x_end
  expect_error(get_landing_info(fix, wb, character_boundaries = bad_cb), "x_end")
})
