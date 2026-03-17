#' @title Read raw eye-tracking samples from a CSV file
#'
#' @description
#' Reads a CSV file containing raw gaze samples exported from an eye tracker
#' and returns a standardised [tibble][tibble::tibble].  The function expects
#' at minimum columns for time, x-coordinate, and y-coordinate (column names
#' are configurable via the `col_map` argument).
#'
#' The coordinate system origin follows the EyeLink convention: **(0, 0) is
#' the top-left corner** of the display, with x increasing rightward and y
#' increasing downward.
#'
#' @param path Character scalar.  Path to the CSV file.
#' @param col_map Named character vector mapping canonical column names to the
#'   actual column names present in the CSV.  The canonical names are:
#'   * `"time"` – sample timestamp (integer, milliseconds).
#'   * `"x"` – horizontal gaze position (numeric, pixels).
#'   * `"y"` – vertical gaze position (numeric, pixels).
#'   * `"pupil"` – pupil size (numeric; optional, pass `NA` to omit).
#'   * `"eye"` – which eye (`"L"` or `"R"`; optional, pass `NA` to omit).
#'   * `"trial_nr"` – trial identifier (optional, pass `NA` to omit).
#'   * `"participant"` – participant identifier (optional, pass `NA` to omit).
#'
#'   Defaults to `c(time = "time", x = "x", y = "y", pupil = "pupil",
#'   eye = "eye", trial_nr = "trial", participant = "participant")`.
#' @param eyes Character vector.  Which eye values to retain.  Defaults to
#'   `c("L", "R")`.  Ignored if the `"eye"` mapping is `NA`.
#' @param ... Additional arguments forwarded to [readr::read_csv()].
#'
#' @return A [tibble][tibble::tibble] with columns `time` (integer), `x`
#'   (double), `y` (double), and any of `pupil`, `eye`, `trial_nr`,
#'   `participant` that were present in the source file.
#'
#' @importFrom readr read_csv col_double col_integer col_character cols
#' @importFrom dplyr select rename mutate filter tibble
#' @importFrom rlang .data
#'
#' @export
#'
#' @examples
#' csv_file <- system.file("extdata", "example_samples.csv", package = "fixated")
#' if (file.exists(csv_file)) {
#'   samples <- read_samples_csv(csv_file)
#'   head(samples)
#' }
read_samples_csv <- function(
    path,
    col_map = c(
      time        = "time",
      x           = "x",
      y           = "y",
      pupil       = "pupil",
      eye         = "eye",
      trial_nr    = "trial",
      participant = "participant"
    ),
    eyes = c("L", "R"),
    ...
) {
  stopifnot(is.character(path), length(path) == 1L, file.exists(path))

  raw <- readr::read_csv(path, show_col_types = FALSE, ...)

  # Check required columns are present
  required <- c("time", "x", "y")
  for (canon in required) {
    csv_col <- col_map[[canon]]
    if (is.na(csv_col) || !(csv_col %in% names(raw))) {
      stop(
        "Required column '", csv_col, "' (mapped from '", canon,
        "') not found in CSV. Available columns: ",
        paste(names(raw), collapse = ", ")
      )
    }
  }

  # Build renamed data frame with canonical column names
  rename_vec <- character(0)
  for (canon in names(col_map)) {
    csv_col <- col_map[[canon]]
    if (!is.na(csv_col) && csv_col %in% names(raw) && csv_col != canon) {
      rename_vec[[canon]] <- csv_col
    }
  }

  if (length(rename_vec) > 0L) {
    raw <- dplyr::rename(raw, !!!rename_vec)
  }

  # Keep only columns that map to canonical names and exist
  keep_cols <- names(col_map)[
    !is.na(col_map) &
      (col_map %in% names(raw) | names(col_map) %in% names(raw))
  ]
  # After rename, column names are the canonical names
  actual_keep <- intersect(keep_cols, names(raw))
  raw <- dplyr::select(raw, dplyr::all_of(actual_keep))

  # Coerce types
  raw <- dplyr::mutate(
    raw,
    time = as.integer(.data$time),
    x    = as.numeric(.data$x),
    y    = as.numeric(.data$y)
  )

  if ("pupil" %in% names(raw)) {
    raw <- dplyr::mutate(raw, pupil = as.numeric(.data$pupil))
  }

  # Filter by eye if the column is present
  if ("eye" %in% names(raw)) {
    raw <- dplyr::filter(raw, .data$eye %in% eyes)
  }

  raw
}
