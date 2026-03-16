#' Extract Word Boundary Information from ASC Message Lines
#'
#' Parses lines containing `TRIAL … ITEM … WORD …` messages written by
#' OpenSesame into the EyeLink ASC file and returns a tidy data frame of
#' word boundary positions.
#'
#' The expected message format is:
#' ```
#' MSG  <time>  TRIAL <trial_nr>  ITEM <sentence_nr>  WORD <word_nr>  <word>  RIGHT_BOUNDARY  <x>
#' ```
#'
#' @param trial_msg A character vector of MSG lines from a single trial
#'   (one line per element).
#'
#' @return A [tibble][tibble::tibble] with columns `trial_nr` (integer),
#'   `sentence_nr` (integer), `word_nr` (integer), `word` (character), and
#'   `word_right_x_boundary` (integer pixel coordinate of the word's right
#'   edge).  Returns an empty tibble when no matching lines are found.
#'
#' @importFrom dplyr tibble
#' @importFrom stringr str_match str_detect
#'
#' @export
#'
#' @examples
#' msgs <- c(
#'   "MSG\t100 TRIAL 0 ITEM 5 WORD 1 The RIGHT_BOUNDARY 180",
#'   "MSG\t100 TRIAL 0 ITEM 5 WORD 2 quick RIGHT_BOUNDARY 310",
#'   "MSG\t100 TRIAL 0 ITEM 5 WORD 3 fox RIGHT_BOUNDARY 420"
#' )
#' get_word_info_from_msg(msgs)
get_word_info_from_msg <- function(trial_msg) {
  pattern <- paste0(
    "MSG\\t\\d+\\s+TRIAL\\s+(\\d+)\\s+ITEM\\s+(\\d+)\\s+",
    "WORD\\s+(\\d+)\\s+(\\S+)\\s+RIGHT_BOUNDARY\\s+(\\d+)"
  )
  word_lines <- trial_msg[stringr::str_detect(trial_msg, pattern)]

  if (length(word_lines) == 0L) {
    return(dplyr::tibble(
      trial_nr              = integer(0),
      sentence_nr           = integer(0),
      word_nr               = integer(0),
      word                  = character(0),
      word_right_x_boundary = integer(0)
    ))
  }

  m <- stringr::str_match(word_lines, pattern)
  dplyr::tibble(
    trial_nr              = as.integer(m[, 2L]),
    sentence_nr           = as.integer(m[, 3L]),
    word_nr               = as.integer(m[, 4L]),
    word                  = m[, 5L],
    word_right_x_boundary = as.integer(m[, 6L])
  )
}
