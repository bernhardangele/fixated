#' Process a Single Trial Using EyeLink Fixation Events
#'
#' Processes the EyeLink-detected fixation events for one trial of a reading
#' experiment.  The function filters fixations to the display-on interval,
#' marks fixations near blinks, flags short/long fixations, merges short
#' fixations with adjacent ones, and finally assigns word information from
#' the trial's word-boundary data.
#'
#' @param eyelink_fixations A data frame of EyeLink fixation events for the
#'   trial (typically from the `trial_fixations` list returned by
#'   [read_asc()]).  Must contain columns `eye`, `t_start`, `t_end`,
#'   `duration`, `x`, `y`.
#' @param eyelink_blinks A data frame of EyeLink blink events for the trial,
#'   or `NA` when no blinks were recorded.  When a data frame, must contain
#'   columns `eye`, `t_start`, `t_end`.
#' @param trial_words A data frame of word-boundary information for the
#'   trial (e.g., from [get_word_info_from_msg()]).
#' @param trial_db_for_trial A single-row data frame (one trial from
#'   `trial_db`) containing at least `timestamp_display_on`,
#'   `timestamp_display_off`, `recording_sampling_rate`, `subject_nr`,
#'   and `sentence_number`.
#' @param use_eye Character scalar.  Which eye to process (`"L"` or `"R"`).
#'   Defaults to `"R"`.
#' @param min_duration_for_elimination Numeric scalar.  Fixations longer than
#'   this value (ms) are flagged as `too_long`.  Defaults to `800`.
#' @param max_duration_for_elimination_or_merge Numeric scalar.  Fixations
#'   shorter than this value (ms) are flagged as `too_short` (candidates for
#'   elimination or merging).  Defaults to `80`.
#' @param max_distance_for_merge Numeric scalar.  Maximum distance (pixels)
#'   between two fixation centres for the short one to be merged with its
#'   neighbour.  Defaults to `12`.
#' @param sentence_start_x Numeric scalar.  Left x boundary of the sentence
#'   area (pixels).  Defaults to `125`.
#'
#' @return A data frame of cleaned fixations with word information added (see
#'   [assign_word_info()]), or `NULL` when no fixations remain after
#'   processing.
#'
#' @importFrom dplyr filter mutate
#'
#' @export
#'
#' @seealso [assign_word_info()], [merge_fixations()],
#'   [mark_fixations_close_to_blinks()], [calculate_fixation_time_measures()]
process_eyelink_trial <- function(eyelink_fixations,
                                  eyelink_blinks,
                                  trial_words,
                                  trial_db_for_trial,
                                  use_eye                            = "R",
                                  min_duration_for_elimination       = 800,
                                  max_duration_for_elimination_or_merge = 80,
                                  max_distance_for_merge             = 12,
                                  sentence_start_x                   = 125) {
  stopifnot(is.data.frame(eyelink_fixations))
  stopifnot(is.data.frame(trial_words))

  display_on  <- trial_db_for_trial$timestamp_display_on
  display_off <- trial_db_for_trial$timestamp_display_off

  fixations <- eyelink_fixations |>
    dplyr::filter(
      .data$eye == use_eye,
      .data$t_start > display_on,
      .data$t_end   < display_off
    )

  fixations$near_blink <- FALSE

  if (dplyr::is_tibble(eyelink_blinks) || is.data.frame(eyelink_blinks)) {
    blinks <- eyelink_blinks |>
      dplyr::filter(
        .data$eye == use_eye,
        .data$t_start > display_on,
        .data$t_end   < display_off
      )
    if (nrow(blinks) > 0L) {
      fixations <- mark_fixations_close_to_blinks(
        fixations, blinks, t_diff_threshold = 50
      )
    }
  }

  fixations <- fixations |>
    dplyr::mutate(
      too_short = .data$duration < max_duration_for_elimination_or_merge,
      too_long  = .data$duration > min_duration_for_elimination
    )

  merged_fixations <- merge_fixations(
    fixations,
    max_duration = max_duration_for_elimination_or_merge,
    max_distance = max_distance_for_merge,
    rate         = as.numeric(trial_db_for_trial$recording_sampling_rate)
  )

  if (nrow(merged_fixations) > 0L) {
    assign_word_info(merged_fixations, trial_words, sentence_start_x)
  } else {
    warning(
      "No fixations found for ", use_eye, " eye, Subject ",
      trial_db_for_trial$subject_nr, " Trial ",
      rownames(trial_db_for_trial), " Sentence ",
      trial_db_for_trial$sentence_number, ". Returning NULL."
    )
    NULL
  }
}
