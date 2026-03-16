#' Extract Trial Information from ASC Message Lines
#'
#' Searches a character vector of MSG lines for a regular expression pattern
#' with exactly one capture group and returns the captured content.  Designed
#' for extracting variable values written into EyeLink ASC files by
#' OpenSesame or similar stimulus-presentation software.
#'
#' @param text_vector A character vector of MSG lines (one line per element).
#' @param regex_pattern A regular expression with **exactly one capture
#'   group** `(…)` that matches the value to be extracted.
#' @param multiple_allowed Logical.  If `FALSE` (default), throws an error
#'   when the pattern matches more than one line.  If `TRUE`, returns the
#'   first match and emits a warning.
#'
#' @return The content of the capture group as a character scalar, or `NA`
#'   (with a warning) when no line matches.
#'
#' @importFrom stringr str_match
#'
#' @export
#'
#' @examples
#' msgs <- c(
#'   "MSG\t1000 var subject_nr 42",
#'   "MSG\t1001 var condition high",
#'   "MSG\t1002 DISPLAY ON"
#' )
#' get_trial_info_from_msg(msgs, "MSG\\t\\d+ var subject_nr (\\d+)")
#' get_trial_info_from_msg(msgs, "MSG\\t(\\d+) DISPLAY ON")
get_trial_info_from_msg <- function(text_vector,
                                    regex_pattern,
                                    multiple_allowed = FALSE) {
  matches <- stringr::str_match(text_vector, regex_pattern)
  n_matched <- sum(!is.na(matches[, 1L]))

  if (n_matched == 0L) {
    warning("Pattern '", regex_pattern, "' does not match any lines.")
    return(NA_character_)
  }

  if (n_matched == 1L) {
    return(matches[!is.na(matches[, 1L]), 2L])
  }

  # Multiple matches
  if (multiple_allowed) {
    warning(
      "Pattern matches multiple lines: ",
      paste(matches[!is.na(matches[, 1L]), 1L], collapse = " "),
      ". Returning first match."
    )
    return(matches[!is.na(matches[, 1L]), 2L][[1L]])
  }

  stop(
    "Pattern matches multiple lines: ",
    paste(matches[!is.na(matches[, 1L]), 1L], collapse = " ")
  )
}
