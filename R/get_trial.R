#' @title Get samples and events for a specific trial
#'
#' @description
#' Convenience function that filters the `samples` and `events` tables
#' returned by \code{\link{read_asc}} to a single trial.  The tables must
#' contain a `trial_nr` column, which is added automatically when
#' \code{\link{read_asc}} detects trial structure in the file.
#'
#' @param asc_result A named list as returned by \code{\link{read_asc}},
#'   with elements `samples` and `events` that contain a `trial_nr` column.
#' @param trial_nr Integer scalar.  The trial number to extract (as stored
#'   in the `trial_nr` column and in `asc_result$trial_db$trial_nr`).
#'
#' @return A named list with two elements:
#'   \describe{
#'     \item{`samples`}{Tibble of samples for the specified trial.}
#'     \item{`events`}{Tibble of events for the specified trial.}
#'   }
#'
#' @importFrom dplyr filter
#' @importFrom rlang .data
#'
#' @export
#'
#' @examples
#' asc_file <- system.file("extdata", "sub_1_example.asc", package = "fixated")
#' if (file.exists(asc_file)) {
#'   result  <- read_asc(asc_file)
#'   trial0  <- get_trial(result, 0L)
#'   head(trial0$samples)
#'   head(trial0$events)
#' }
get_trial <- function(asc_result, trial_nr) {
  stopifnot(is.list(asc_result))
  if (!all(c("samples", "events") %in% names(asc_result))) {
    stop("asc_result must have 'samples' and 'events' elements")
  }
  if (!"trial_nr" %in% names(asc_result$samples)) {
    stop("asc_result$samples does not have a 'trial_nr' column; ",
         "re-read the file with read_asc() using a supported eye_tracker")
  }
  if (!"trial_nr" %in% names(asc_result$events)) {
    stop("asc_result$events does not have a 'trial_nr' column; ",
         "re-read the file with read_asc() using a supported eye_tracker")
  }
  tnr <- as.integer(trial_nr)
  list(
    samples = dplyr::filter(asc_result$samples, .data$trial_nr == tnr),
    events  = dplyr::filter(asc_result$events,  .data$trial_nr == tnr)
  )
}
