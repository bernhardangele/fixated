#' Get Trial Information from Messages
#'
#' This function extracts information from a character vector containing multiple lines of text based on a provided regex pattern with exactly one capture group. It is designed to retrieve relevant information from messages in an ASC file.
#'
#' @param text_vector A character vector with multiple lines of text.
#' @param regex_pattern A regex pattern with exactly one capture group for matching lines in the text.
#' @param multiple_allowed Logical indicating whether multiple matches are allowed. If TRUE, the function returns the first match and displays a warning; if FALSE, an error is thrown for multiple matches.
#'
#' @return The matched content of the capture group or NA if no match is found.
#'
#' @examples
#' # Create a character vector of messages
#' messages <- c("!MODE RECORD", "Trial: 42", "Subject: John Doe")
#'
#' # Extract trial information using regex
#' trial_number <- get_trial_info_from_msg(messages, "Trial: (\\d+)")
#'
#' @export

get_trial_info_from_msg <- function(text_vector, regex_pattern, multiple_allowed = FALSE) {
  matches <- str_match(text_vector, regex_pattern)
  
  if (sum(!is.na(matches[,1])) == 0) {
    warning("Pattern '", regex_pattern, "' does not match any lines.")
    return(NA)
  } else if (sum(!is.na(matches[,1])) == 1) {
    return(matches[!is.na(matches[,1]), 2])
  } else {
    if(multiple_allowed){
      warning("Pattern matches multiple lines: ", paste(matches[!is.na(matches[,1]), 1], collapse = " "), ". Returning first match.")
      return(matches[!is.na(matches[,1]), 2][1])
    } else {
      stop("Pattern matches multiple lines: ", paste(matches[!is.na(matches[,1]), 1], collapse = " "))
    }
  }
}
