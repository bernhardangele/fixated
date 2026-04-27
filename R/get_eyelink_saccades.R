#' @title Extract EyeLink-detected saccades from an ASC events table
#'
#' @description
#' The EyeLink host computer records saccade events as `ESACC` lines in the
#' ASC file.  \code{\link{read_asc}} parses these into the `events` tibble
#' with `type == "SACCADE"`.
#'
#' This function extracts those rows and returns them in a convenient tibble
#' that includes the **landing x position** (`x_end`) and landing y position
#' (`y_end`) of each saccade, i.e. the gaze coordinates at the end of the
#' saccade.
#'
#' @param events A [tibble][tibble::tibble] as returned in the `events`
#'   element of \code{\link{read_asc}}, with columns `type`, `eye`,
#'   `start_time`, `end_time`, `duration`, `x_start`, `y_start`, `x_end`,
#'   `y_end`.
#'
#' @return A [tibble][tibble::tibble] with one row per EyeLink-detected
#'   saccade and columns `start_time`, `end_time`, `duration`, `x_start`,
#'   `y_start`, `x_end`, `y_end`, `eye`.  The column `x_end` is the
#'   **landing x position** of the saccade (horizontal gaze coordinate at
#'   saccade offset), and `y_end` is the corresponding landing y position.
#'   If `trial_nr` or `sentence_nr` columns are present in `events`, they
#'   are preserved in the output.  The tibble is sorted by `start_time`.
#'
#' @importFrom dplyr filter select arrange
#' @importFrom rlang .data
#'
#' @export
#'
#' @examples
#' asc_file <- system.file("extdata", "sub_1_example.asc", package = "fixated")
#' if (file.exists(asc_file)) {
#'   result   <- read_asc(asc_file)
#'   saccades <- get_eyelink_saccades(result$events)
#'   head(saccades)
#' }
get_eyelink_saccades <- function(events) {
  stopifnot(is.data.frame(events))
  required <- c("type", "eye", "start_time", "end_time", "duration",
                "x_start", "y_start", "x_end", "y_end")
  missing  <- setdiff(required, names(events))
  if (length(missing) > 0L) {
    stop("events is missing required columns: ", paste(missing, collapse = ", "))
  }

  sacc <- dplyr::filter(events, .data$type == "SACCADE")

  keep_cols <- c("start_time", "end_time", "duration",
                 "x_start", "y_start", "x_end", "y_end", "eye")
  if ("trial_nr" %in% names(sacc)) keep_cols <- c(keep_cols, "trial_nr")
  if ("sentence_nr" %in% names(sacc)) keep_cols <- c(keep_cols, "sentence_nr")
  keep_cols <- keep_cols[keep_cols %in% names(sacc)]

  sacc <- dplyr::select(sacc, dplyr::all_of(keep_cols))
  dplyr::arrange(sacc, .data$start_time)
}
