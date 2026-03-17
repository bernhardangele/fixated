#' @title Extract EyeLink-detected fixations from an ASC events table
#'
#' @description
#' The EyeLink host computer runs its own fixation-detection algorithm and
#' writes the results as `EFIX` lines to the ASC file.  \code{\link{read_asc}}
#' parses these into the `events` tibble with `type == "FIXATION"`.
#'
#' This function extracts those rows and returns them in the same column
#' format produced by \code{\link{detect_fixations}}, making it easy to
#' substitute EyeLink fixations for algorithm-detected ones in downstream
#' analysis steps.
#'
#' @param events A [tibble][tibble::tibble] as returned in the `events`
#'   element of \code{\link{read_asc}}, with columns `type`, `eye`,
#'   `start_time`, `end_time`, `duration`, `avg_x`, `avg_y`.
#'
#' @return A [tibble][tibble::tibble] with one row per EyeLink-detected
#'   fixation and columns `start_time`, `end_time`, `duration`, `avg_x`,
#'   `avg_y`, `avg_pupil`, `eye`.  If a `trial_nr` column is present in
#'   `events`, it is preserved in the output.  The tibble is sorted by
#'   `start_time`.
#'
#' @importFrom dplyr filter select arrange
#' @importFrom rlang .data
#'
#' @export
#'
#' @examples
#' asc_file <- system.file("extdata", "sub_1_example.asc", package = "fixated")
#' if (file.exists(asc_file)) {
#'   result    <- read_asc(asc_file)
#'   fixations <- get_eyelink_fixations(result$events)
#'   head(fixations)
#' }
get_eyelink_fixations <- function(events) {
  stopifnot(is.data.frame(events))
  required <- c("type", "eye", "start_time", "end_time", "duration",
                "avg_x", "avg_y")
  missing  <- setdiff(required, names(events))
  if (length(missing) > 0L) {
    stop("events is missing required columns: ", paste(missing, collapse = ", "))
  }

  fix <- dplyr::filter(events, .data$type == "FIXATION")

  keep_cols <- c("start_time", "end_time", "duration",
                 "avg_x", "avg_y", "avg_pupil", "eye")
  if ("trial_nr" %in% names(fix)) keep_cols <- c(keep_cols, "trial_nr")
  keep_cols <- keep_cols[keep_cols %in% names(fix)]

  fix <- dplyr::select(fix, dplyr::all_of(keep_cols))
  dplyr::arrange(fix, .data$start_time)
}
