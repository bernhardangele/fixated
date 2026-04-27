#' @title fixated: Analyze Eye Movement Data from Eye Trackers
#'
#' @description
#' The fixated package provides tools to read, process, and analyze eye
#' movement data recorded by eye trackers such as the SR Research EyeLink.
#'
#' ## Main features
#'
#' * Read raw gaze samples and events from ASC files (`read_asc()`) or
#'   CSV files (`read_samples_csv()`).
#' * Detect fixations from raw samples using a dispersion-threshold
#'   algorithm (`detect_fixations()`).
#' * Clean fixation sequences: merge adjacent short fixations and remove
#'   outliers (`clean_fixations()`).
#' * Extract EyeLink-detected fixations (including average x/y location)
#'   from parsed events (`get_eyelink_fixations()`).
#' * Extract EyeLink-detected saccades (including landing x/y position)
#'   from parsed events (`get_eyelink_saccades()`).
#' * Read word-level regions of interest from a CSV file (`read_roi()`).
#' * Compute standard reading-research eye-movement measures per word
#'   (`compute_eye_measures()`): first fixation duration (FFD), gaze
#'   duration (GD), go-past time (GPT), and total viewing time (TVT).
#'
#' @useDynLib fixated, .registration = TRUE
#' @importFrom Rcpp sourceCpp
#'
#' @keywords internal
"_PACKAGE"
