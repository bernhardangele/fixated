#' Categorize Fixations as First-Pass or Other
#'
#' Classifies each element of a word-number sequence into one of three
#' categories used in reading-research eye-movement analysis:
#'
#' * **`"first pass"`** – The first time the eye lands on a word, before
#'   moving to any later word.
#' * **`"first pass refixation"`** – A repeated fixation on the same word
#'   immediately following a first-pass fixation on that word (no intervening
#'   fixation on a different word).
#' * **`"other fixation"`** – Any fixation that is not a first-pass fixation
#'   or first-pass refixation (e.g., a regression or a second-pass fixation).
#'
#' @param sequence Integer (or numeric) vector of word numbers, one entry per
#'   fixation in temporal order.
#'
#' @return A character vector the same length as `sequence` with values
#'   `"first pass"`, `"first pass refixation"`, or `"other fixation"`.
#'
#' @export
#'
#' @examples
#' # Simple left-to-right reading
#' categorize_fixation(c(1, 2, 3, 4))
#'
#' # Regression back to word 2 after word 4
#' categorize_fixation(c(1, 2, 3, 4, 2))
#'
#' # Refixation on word 3
#' categorize_fixation(c(1, 2, 3, 3, 4))
categorize_fixation <- function(sequence) {
  categories <- character(length(sequence))

  # Track which words can still receive a first-pass fixation
  eligible <- unique(sequence)

  for (i in seq_along(sequence)) {
    element <- sequence[[i]]

    if (element %in% eligible) {
      categories[[i]] <- "first pass"
      # All words from the earliest up to the current word are no longer
      # eligible for first-pass fixations
      eligible <- setdiff(eligible, min(sequence):element)
    } else if (
      i > 1L &&
      sequence[[i]] == sequence[[i - 1L]] &&
      categories[[i - 1L]] %in% c("first pass", "first pass refixation")
    ) {
      categories[[i]] <- "first pass refixation"
    } else {
      categories[[i]] <- "other fixation"
    }
  }

  categories
}
