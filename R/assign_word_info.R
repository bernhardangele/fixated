#' Assign Word Information to Fixations
#'
#' Adds word-level information (word number and word text) to each fixation in
#' a data frame of fixations based on word right-boundary positions.  A
#' fixation is assigned to the word whose right boundary is the first one to
#' the right of the fixation's x-coordinate.
#'
#' @param fixations A data frame of fixations containing at least a column
#'   named `x` (horizontal gaze position in pixels).
#' @param word_boundaries A data frame of word boundary information with
#'   columns `word_nr` (integer), `word` (character), and
#'   `word_right_x_boundary` (numeric, pixels).  Typically the output of
#'   [get_word_info_from_msg()] or a row from the `word_boundaries` element
#'   returned by [read_asc()].
#' @param sentence_left_x_boundary Numeric scalar.  X-coordinate (pixels)
#'   of the left edge of the sentence area.  Fixations to the left of this
#'   value receive `word_nr = -1`.  Defaults to `125`.
#'
#' @return A copy of `fixations` with two new columns added:
#'   \describe{
#'     \item{`word_nr`}{Integer word number, or `-1` for fixations before
#'       the sentence start, or `-99` for fixations past the last word.}
#'     \item{`word`}{Character word text, or `NA` for out-of-sentence
#'       fixations.}
#'   }
#'   The columns `sentence_nr` and `trial_nr` are also set from the first row
#'   of `word_boundaries` when those columns are present there.
#'
#' @importFrom dplyr mutate
#'
#' @export
#'
#' @examples
#' fixations <- dplyr::tibble(x = c(50, 160, 300, 2000), y = c(540, 540, 540, 540))
#' word_boundaries <- dplyr::tibble(
#'   word_nr              = 1:3,
#'   word                 = c("The", "quick", "fox"),
#'   word_right_x_boundary = c(200, 360, 500),
#'   sentence_nr          = 1L,
#'   trial_nr             = 1L
#' )
#' assign_word_info(fixations, word_boundaries)
assign_word_info <- function(fixations,
                             word_boundaries,
                             sentence_left_x_boundary = 125) {
  stopifnot(is.data.frame(fixations), is.data.frame(word_boundaries))
  stopifnot("x" %in% names(fixations))
  stopifnot(all(c("word_nr", "word", "word_right_x_boundary") %in%
                  names(word_boundaries)))

  fx         <- fixations$x
  boundaries <- sort(word_boundaries$word_right_x_boundary)
  n_words    <- length(boundaries)

  # Vectorised assignment using findInterval:
  # findInterval(x, v) returns k so that v[k] <= x < v[k+1].
  # Adding 1 gives the index of the first word whose right boundary exceeds x,
  # which is exactly the word the fixation lands on.
  raw_idx <- findInterval(fx, boundaries) + 1L

  # Classify each fixation
  before_sentence <- !is.na(fx) & fx < sentence_left_x_boundary
  after_sentence  <- !is.na(fx) & !before_sentence & raw_idx > n_words

  word_nr_vec <- integer(nrow(fixations))
  word_vec    <- character(nrow(fixations))

  word_nr_vec[is.na(fx)]         <- NA_integer_
  word_vec[is.na(fx)]            <- NA_character_
  word_nr_vec[before_sentence]   <- -1L
  word_vec[before_sentence]      <- NA_character_
  word_nr_vec[after_sentence]    <- -99L
  word_vec[after_sentence]       <- NA_character_

  valid <- !is.na(fx) & !before_sentence & !after_sentence
  if (any(valid)) {
    word_nr_vec[valid] <- word_boundaries$word_nr[raw_idx[valid]]
    word_vec[valid]    <- word_boundaries$word[raw_idx[valid]]
  }

  out <- fixations
  out$word_nr <- word_nr_vec
  out$word    <- word_vec

  if ("sentence_nr" %in% names(word_boundaries)) {
    out$sentence_nr <- word_boundaries$sentence_nr[[1L]]
  }
  if ("trial_nr" %in% names(word_boundaries)) {
    out$trial_nr <- word_boundaries$trial_nr[[1L]]
  }

  out
}
