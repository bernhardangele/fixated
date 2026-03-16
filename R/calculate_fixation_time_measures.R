#' Calculate Fixation Time Measures for a Single Trial
#'
#' Computes standard reading-research eye-movement measures (FFD, GD, SFD,
#' go-past time, TVT) for each word in a trial from a cleaned fixation data
#' frame that contains word assignments.
#'
#' This function is designed to work with the output of [process_eyelink_trial()]
#' or [assign_word_info()].  The fixation data frame must contain quality flags
#' (`too_short`, `too_long`, `merged`, `near_blink`) that are used to filter
#' fixations before computing the measures.
#'
#' @param fixations A data frame of fixations for a single trial.  Must
#'   contain columns `word_nr`, `duration`, `too_short`, `too_long`,
#'   `merged`, `near_blink`.
#' @param words A data frame of word information for the trial (e.g., from
#'   [get_word_info_from_msg()]).  Must contain columns `word_nr`, `word`,
#'   and `word_right_x_boundary`.
#'
#' @return A data frame with one row per word and the following columns:
#'   \describe{
#'     \item{`word_nr`}{Word number.}
#'     \item{`word`}{Word text.}
#'     \item{`ffd`}{First Fixation Duration (ms) – duration of the first
#'       fixation on the word during the first pass.}
#'     \item{`gd`}{Gaze Duration (ms) – sum of all first-pass fixation
#'       durations on the word.}
#'     \item{`sfd`}{Single Fixation Duration (ms) – equal to FFD when the
#'       word received exactly one fixation during the first pass, otherwise
#'       `NA`.}
#'     \item{`gopast`}{Go-Past Time (ms) – total time from first entering
#'       the word until the gaze exits to the right, including regressions.
#'       Also known as regression-path duration.}
#'     \item{`tvt`}{Total Viewing Time (ms) – sum of all fixation durations
#'       on the word across the entire trial.}
#'   }
#'
#' @importFrom dplyr filter mutate group_by summarize left_join select
#'
#' @export
#'
#' @examples
#' # Minimal example with three words and five fixations
#' fixations <- dplyr::tibble(
#'   word_nr    = c(1L, 2L, 3L, 2L, 3L),
#'   duration   = c(200L, 150L, 180L, 120L, 90L),
#'   too_short  = FALSE,
#'   too_long   = FALSE,
#'   merged     = FALSE,
#'   near_blink = FALSE
#' )
#' words <- dplyr::tibble(
#'   word_nr               = 1:3,
#'   word                  = c("The", "quick", "fox"),
#'   word_right_x_boundary = c(200L, 360L, 500L)
#' )
#' calculate_fixation_time_measures(fixations, words)
calculate_fixation_time_measures <- function(fixations, words) {
  stopifnot(is.data.frame(fixations), is.data.frame(words))
  required_fix <- c("word_nr", "duration", "too_short", "too_long",
                    "merged", "near_blink")
  missing_fix <- setdiff(required_fix, names(fixations))
  if (length(missing_fix) > 0L) {
    stop("fixations is missing required columns: ",
         paste(missing_fix, collapse = ", "))
  }

  # Apply exclusion criteria: remove fixations flagged as too short, too long,
  # merged into another, or near a blink (< 100 ms)
  fixations_clean <- fixations |>
    dplyr::filter(
      !.data$too_short,
      !.data$too_long,
      !.data$merged,
      !(.data$duration < 100L & .data$near_blink)
    ) |>
    dplyr::mutate(
      word_max   = cummax(.data$word_nr),
      first_pass = categorize_fixation(.data$word_nr)
    )

  # First-pass measures (FFD, GD, SFD)
  first_pass_measures <- fixations_clean |>
    dplyr::filter(.data$first_pass != "other fixation") |>
    dplyr::group_by(.data$word_nr) |>
    dplyr::summarize(
      ffd = .data$duration[[1L]],
      gd  = sum(.data$duration),
      sfd = ifelse(dplyr::n() == 1L, .data$duration[[1L]], NA_integer_),
      .groups = "drop"
    )

  # Go-past time (accumulated duration up to rightward exit)
  gopast_measures <- fixations_clean |>
    dplyr::group_by(.data$word_max) |>
    dplyr::summarize(gopast = sum(.data$duration), .groups = "drop")

  # Total viewing time (all passes)
  tvt_measures <- fixations_clean |>
    dplyr::group_by(.data$word_nr) |>
    dplyr::summarize(tvt = sum(.data$duration), .groups = "drop")

  words |>
    dplyr::left_join(first_pass_measures, by = "word_nr") |>
    dplyr::left_join(gopast_measures,     by = c("word_nr" = "word_max")) |>
    dplyr::left_join(tvt_measures,        by = "word_nr") |>
    dplyr::select(-"word_right_x_boundary")
}
