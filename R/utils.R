# Internal utility functions for the fixated package.
# These are not exported and are called from other package files.

#' Check that a data frame has required columns
#' @param df Data frame to check.
#' @param required Character vector of required column names.
#' @param arg_name Name of the argument (for error message).
#' @noRd
.check_required_cols <- function(df, required, arg_name = "df") {
  missing_cols <- setdiff(required, names(df))
  if (length(missing_cols) > 0L) {
    stop(
      "`", arg_name, "` is missing required columns: ",
      paste(missing_cols, collapse = ", ")
    )
  }
  invisible(df)
}

#' Euclidean distance between two 2-D points
#' @param x1,y1 Numeric scalars for point 1.
#' @param x2,y2 Numeric scalars for point 2.
#' @noRd
.euclidean <- function(x1, y1, x2, y2) {
  sqrt((x2 - x1)^2 + (y2 - y1)^2)
}

#' Convert pixels to degrees of visual angle
#'
#' @param pixels Numeric vector of pixel distances.
#' @param pixels_per_cm Numeric scalar. Monitor resolution in pixels per cm.
#' @param viewing_distance_cm Numeric scalar. Viewing distance in cm.
#' @return Numeric vector of values in degrees.
#' @noRd
.pixels_to_degrees <- function(pixels, pixels_per_cm, viewing_distance_cm) {
  cm_distance <- pixels / pixels_per_cm
  atan2(cm_distance, viewing_distance_cm) * (180 / pi)
}

#' Safe as.integer that returns NA on failure (no warning)
#' @noRd
.safe_int <- function(x) suppressWarnings(as.integer(x))

#' Safe as.numeric that returns NA on failure (no warning)
#' @noRd
.safe_num <- function(x) suppressWarnings(as.numeric(x))
