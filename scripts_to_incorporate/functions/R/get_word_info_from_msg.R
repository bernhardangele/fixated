#' Get Word Information from Trial Message
#'
#' This function extracts word information from a trial message containing lines related to words. It searches for lines containing the "WORD" pattern and parses relevant information from them to create a data frame with word-related details.
#'
#' @param trial_msg A character vector containing lines of trial messages.
#'
#' @return A data frame with word information including trial number, sentence number, word number, word text, and word right boundary.
#'
#' @import dplyr
#' @import readr
#'
#' @export
get_word_info_from_msg <-  function(trial_msg) {
  # Function code here
}

get_word_info_from_msg <-  function(trial_msg) {
  word_lines <- grep(x = trial_msg, pattern = "WORD", value = TRUE)
  words <- read_table(word_lines, col_names = c("MSG", "timestamp", "TRIAL", "trial_nr", "ITEM", "sentence_nr", "WORD", "word_nr", "word", "RIGHT_BOUNDARY", "word_right_x_boundary")) %>%
    dplyr::select(trial_nr, sentence_nr, word_nr, word, word_right_x_boundary)
  return(words)
}
