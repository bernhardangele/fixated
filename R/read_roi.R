#' @title Read word regions of interest from a CSV file
#'
#' @description
#' Reads a CSV file that describes the rectangular regions of interest (ROIs)
#' for individual words within sentences/trials and returns a standardised
#' [tibble][tibble::tibble].
#'
#' Each row in the file represents one word.  The ROI is defined by the
#' pixel coordinates of its bounding box.  The coordinate system origin
#' follows the EyeLink convention: **(0, 0) is the top-left corner** of the
#' display, with x increasing rightward and y increasing downward.
#'
#' @param path Character scalar.  Path to the CSV file.
#' @param col_map Named character vector mapping canonical column names to the
#'   actual column names present in the CSV.  Canonical names and defaults:
#'   * `"trial"` → `"trial"` – trial/sentence identifier (**required**).
#'   * `"word_id"` → `"word_id"` – word index within trial (**required**).
#'   * `"word"` → `"word"` – word text string (optional).
#'   * `"x_start"` → `"x_start"` – left edge of ROI in pixels (**required**).
#'   * `"x_end"` → `"x_end"` – right edge of ROI in pixels (**required**).
#'   * `"y_start"` → `"y_start"` – top edge of ROI in pixels (**required**).
#'   * `"y_end"` → `"y_end"` – bottom edge of ROI in pixels (**required**).
#' @param ... Additional arguments forwarded to [readr::read_csv()].
#'
#' @return A [tibble][tibble::tibble] with columns `trial`, `word_id`,
#'   `x_start`, `x_end`, `y_start`, `y_end`, and (if present) `word`, all
#'   with canonical names.  Sorted by `trial`, then `word_id`.
#'
#' @importFrom readr read_csv
#' @importFrom dplyr mutate arrange rename select tibble
#' @importFrom rlang .data
#'
#' @export
#'
#' @examples
#' roi_file <- system.file("extdata", "example_roi.csv", package = "fixated")
#' if (file.exists(roi_file)) {
#'   roi <- read_roi(roi_file)
#'   head(roi)
#' }
read_roi <- function(
    path,
    col_map = c(
      trial   = "trial",
      word_id = "word_id",
      word    = "word",
      x_start = "x_start",
      x_end   = "x_end",
      y_start = "y_start",
      y_end   = "y_end"
    ),
    ...
) {
  stopifnot(is.character(path), length(path) == 1L, file.exists(path))

  raw <- readr::read_csv(path, show_col_types = FALSE, ...)

  required_canons <- c("trial", "word_id", "x_start", "x_end", "y_start", "y_end")
  for (canon in required_canons) {
    csv_col <- col_map[[canon]]
    if (is.na(csv_col) || !(csv_col %in% names(raw))) {
      stop(
        "Required column '", csv_col, "' (mapped from '", canon,
        "') not found in CSV. Available columns: ",
        paste(names(raw), collapse = ", ")
      )
    }
  }

  # Rename to canonical names
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

  # Keep canonical columns that are present
  canonical_cols <- names(col_map)
  keep_cols      <- intersect(canonical_cols, names(raw))
  raw            <- dplyr::select(raw, dplyr::all_of(keep_cols))

  # Coerce types
  raw <- dplyr::mutate(
    raw,
    word_id = as.integer(.data$word_id),
    x_start = as.numeric(.data$x_start),
    x_end   = as.numeric(.data$x_end),
    y_start = as.numeric(.data$y_start),
    y_end   = as.numeric(.data$y_end)
  )

  dplyr::arrange(raw, .data$trial, .data$word_id)
}
