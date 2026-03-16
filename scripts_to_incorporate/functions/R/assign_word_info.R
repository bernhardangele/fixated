#' Assign Word Information to Fixations
#'
#' This function adds information about the fixated word to each fixation in a data frame of fixations based on word boundaries. It determines the word number and word text that was fixated during each fixation.
#'
#' @param fixations A data frame containing fixation data, usually from the output of \code{read_asc_file}
#' @param word_boundaries A data frame containing word boundary information, usually from the output of \code{read_asc_file}.
#' @param sentence_left_x_boundary X-coordinate boundary indicating the left edge of the sentence on the screen.
#'
#' @return A modified data frame of fixations with added columns for word number and word text for each fixation.
#'
#' @import dplyr
#'
#' @export
assign_word_info <-
  function(fixations,
           word_boundaries,
           sentence_left_x_boundary = 125) {
    # initialize new columns with 0, except for sentence nr and trial nr, which are the same for all fixations (this is all one trial)
    fixations_with_words <-
      fixations %>% mutate(
        word_nr = NA,
        word = NA,
        sentence_nr = word_boundaries$sentence_nr[1],
        trial_nr = word_boundaries$trial_nr[1]
      )
    for (i in 1:nrow(fixations)) {
      fixation_x <- fixations$x[i]
      word_right_boundaries <- word_boundaries$word_right_x_boundary
      
      if (fixation_x < sentence_left_x_boundary) {
        fixations_with_words$word_nr[i] <- -1
        fixations_with_words$word[i] <- NA
      } else if (fixation_x >= max(word_right_boundaries)) {
        fixations_with_words$word_nr[i] <- -99
        fixations_with_words$word[i] <- NA
      } else {
        word_idx <- which(fixation_x < word_right_boundaries)[1]
        fixations_with_words$word_nr[i] <-
          word_boundaries$word_nr[word_idx]
        fixations_with_words$word[i] <-
          word_boundaries$word[word_idx]
      }
    }
    return(fixations_with_words)
  }

