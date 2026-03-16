#' Calculate EyeLink-Based Eye-Movement Measures for a Subject
#'
#' Iterates over all experimental trials for a subject, processes each trial's
#' EyeLink fixation events using [process_eyelink_trial()], and computes
#' word-level eye-movement measures with [calculate_fixation_time_measures()].
#'
#' Only trials where `trial_type == "experimental"` **and**
#' `has_display_off == TRUE` are processed; all other trials return `NULL`
#' and are silently skipped.
#'
#' @param subject A list with the structure returned by the
#'   `read_asc_file()` function (from the original analysis scripts).  The
#'   list must contain:
#'   \describe{
#'     \item{`filename`}{Character string – subject identifier.}
#'     \item{`trial_db`}{Data frame with one row per trial, including columns
#'       `trial_type`, `has_display_off`, `subject_nr`,
#'       `recording_sampling_rate`, `target_word_nr`, `response`, `cond`,
#'       `sentence_number`.}
#'     \item{`trial_fixations`}{List of per-trial fixation data frames.}
#'     \item{`trial_blinks`}{List of per-trial blink data frames (or `NA`).}
#'     \item{`trial_words`}{List of per-trial word-boundary data frames.}
#'   }
#' @param use_eye Character scalar.  Which eye to use (`"L"` or `"R"`).
#'   Defaults to `"R"`.
#'
#' @return A data frame (one row per word per trial) with columns `word_nr`,
#'   `word`, `ffd`, `gd`, `sfd`, `gopast`, `tvt`, `subject`,
#'   `target_word_nr`, `acceptability`, `rate`, and `cond`.
#'
#' @importFrom dplyr bind_rows mutate
#'
#' @export
#'
#' @seealso [process_eyelink_trial()], [calculate_fixation_time_measures()]
calculate_subject_eyelink_measures <- function(subject, use_eye = "R") {
  trial_db <- subject$trial_db
  fixations_by_word <- vector("list", nrow(trial_db))

  for (i in seq_len(nrow(trial_db))) {
    row <- trial_db[i, ]
    if (isTRUE(row$trial_type == "experimental") &&
        isTRUE(row$has_display_off)) {
      fixations_for_measures <- process_eyelink_trial(
        eyelink_fixations  = subject$trial_fixations[[i]],
        eyelink_blinks     = subject$trial_blinks[[i]],
        trial_words        = subject$trial_words[[i]],
        trial_db_for_trial = row,
        use_eye            = use_eye
      )
      if (!is.null(fixations_for_measures)) {
        measures <- calculate_fixation_time_measures(
          fixations_for_measures,
          subject$trial_words[[i]]
        )
        measures$subject        <- row$subject_nr
        measures$target_word_nr <- as.numeric(row$target_word_nr) + 1L
        measures$acceptability  <- row$response
        measures$rate           <- row$recording_sampling_rate
        measures$cond           <- row$cond
        fixations_by_word[[i]]  <- measures
      }
    }
  }

  dplyr::bind_rows(fixations_by_word)
}

#' Process All Experimental Trials for a Subject (EyeLink Fixations)
#'
#' Applies [process_eyelink_trial()] to every experimental trial in a subject
#' data list and returns a list of per-trial cleaned fixation data frames.
#' Trials that are not experimental or that lack a display-off marker return
#' `NULL`.
#'
#' @param subject A subject data list (see [calculate_subject_eyelink_measures()]
#'   for the required structure).
#' @param use_eye Character scalar.  Which eye to process.  Defaults to `"R"`.
#'
#' @return A list (one element per trial) of cleaned fixation data frames with
#'   word information, or `NULL` for skipped trials.
#'
#' @importFrom dplyr bind_rows
#'
#' @export
#'
#' @seealso [process_eyelink_trial()], [calculate_subject_eyelink_measures()]
process_subject_eyelink_trials <- function(subject, use_eye = "R") {
  trial_db <- subject$trial_db
  eyelink_fixations_trials <- vector("list", nrow(trial_db))

  for (i in seq_len(nrow(trial_db))) {
    row <- trial_db[i, ]
    if (isTRUE(row$trial_type == "experimental") &&
        isTRUE(row$has_display_off)) {
      eyelink_fixations_trials[[i]] <- process_eyelink_trial(
        eyelink_fixations  = subject$trial_fixations[[i]],
        eyelink_blinks     = subject$trial_blinks[[i]],
        trial_words        = subject$trial_words[[i]],
        trial_db_for_trial = row,
        use_eye            = use_eye
      )
    }
  }

  eyelink_fixations_trials
}
