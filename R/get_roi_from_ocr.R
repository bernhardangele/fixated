#' @title Extract word or character regions of interest from a trial image
#'   using OCR
#'
#' @description
#' Uses the Tesseract OCR engine (via the \pkg{tesseract} package) to locate
#' word or character bounding boxes in a single trial screenshot.  The
#' returned tibble has the same column layout as the `word_boundaries` or
#' `character_boundaries` tables produced by [read_asc()] and
#' [read_eyetrack_asc()], so it can be used directly as the `roi` argument to
#' [compute_eye_measures()].
#'
#' Because the function processes a **single image**, it is designed to be
#' called in a loop or with [purrr::map()] / [purrr::imap_dfr()] to process
#' an entire folder of trial screenshots (see the Examples section).
#'
#' @param image_path Character scalar.  Path to the image file (PNG, JPEG,
#'   TIFF, or any format supported by the \pkg{tesseract} package).
#' @param trial_nr Integer scalar.  Trial number assigned to all rows in the
#'   returned tibble.  Defaults to `1L`.
#' @param level Character scalar.  Granularity of the bounding boxes.  Either
#'   `"word"` (default) or `"character"`.  At the word level the output
#'   matches the `word_boundaries` schema; at the character level it matches
#'   the `character_boundaries` schema.
#'
#'   **Note:** character-level bounding boxes are estimated by dividing each
#'   word's bounding box proportionally across its characters (equal-width
#'   assumption).  This is a reasonable approximation for most fonts used in
#'   reading experiments but will not be pixel-perfect for proportional fonts.
#' @param language Character scalar.  Tesseract language/model name passed to
#'   [tesseract::tesseract()].  Defaults to `"eng"`.  Ignored when `engine`
#'   is supplied.
#' @param engine A pre-built `tesseract` engine object created by
#'   [tesseract::tesseract()].  When provided, `language` is ignored.  Reuse
#'   a single engine object across many images (e.g., inside a `purrr::map`
#'   call) to avoid repeated initialisation overhead.
#' @param min_confidence Numeric scalar.  OCR tokens whose confidence score is
#'   strictly below this threshold are dropped.  Confidence scores from
#'   Tesseract range from 0 to 100; passing a value greater than 100 (e.g.
#'   `101`) will discard all detections.  Defaults to `0` (retain all tokens).
#'
#' @return
#' When `level = "word"`, a [tibble][tibble::tibble] with columns:
#' \describe{
#'   \item{`trial_nr`}{Integer. Trial identifier (the value of `trial_nr`).}
#'   \item{`word_id`}{Integer. Sequential word index within the trial (1-based).}
#'   \item{`word`}{Character. Word text as recognised by Tesseract.}
#'   \item{`x_start`}{Double. Left edge of the word bounding box in pixels.}
#'   \item{`x_end`}{Double. Right edge of the word bounding box in pixels.}
#'   \item{`y_start`}{Double. Top edge of the word bounding box in pixels.}
#'   \item{`y_end`}{Double. Bottom edge of the word bounding box in pixels.}
#' }
#'
#' When `level = "character"`, a [tibble][tibble::tibble] with columns:
#' \describe{
#'   \item{`trial_nr`}{Integer. Trial identifier.}
#'   \item{`word_id`}{Integer. Sequential word index within the trial.}
#'   \item{`char_id`}{Integer. Position of the character within the word (1-based).}
#'   \item{`char`}{Character. Single character as recognised by Tesseract.}
#'   \item{`x_start`}{Double. Left edge of the character bounding box in pixels.}
#'   \item{`x_end`}{Double. Right edge of the character bounding box in pixels.}
#'   \item{`y_start`}{Double. Top edge of the character bounding box in pixels.}
#'   \item{`y_end`}{Double. Bottom edge of the character bounding box in pixels.}
#' }
#'
#' Rows are sorted by `trial_nr`, `word_id` (and `char_id` when applicable).
#' An empty tibble with the appropriate columns is returned if no text is
#' detected or all detections fall below `min_confidence`.
#'
#' @details
#' ## Coordinate system
#' All coordinates follow the EyeLink convention: (0, 0) is the **top-left**
#' corner of the image, x increases rightward and y increases downward.
#' Tesseract uses the same convention, so no axis flip is applied.
#'
#' ## Engine reuse across images
#' Creating a Tesseract engine is relatively expensive.  When processing many
#' images, create the engine once and pass it to every call:
#'
#' ```r
#' library(tesseract)
#' eng <- tesseract("eng")
#'
#' image_files <- list.files("stimuli/", pattern = "\\.png$", full.names = TRUE)
#' roi_all <- purrr::imap_dfr(
#'   image_files,
#'   \(path, i) get_roi_from_ocr(path, trial_nr = i, engine = eng)
#' )
#' ```
#'
#' @seealso [read_roi()], [read_asc()], [read_eyetrack_asc()],
#'   [compute_eye_measures()]
#'
#' @importFrom dplyr tibble bind_rows
#' @importFrom rlang .data
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Single image — word-level ROIs
#' roi <- get_roi_from_ocr("trial_01.png", trial_nr = 1L)
#' head(roi)
#'
#' # Character-level ROIs
#' char_roi <- get_roi_from_ocr("trial_01.png", trial_nr = 1L, level = "character")
#' head(char_roi)
#'
#' # Process a whole folder with purrr, reusing the engine
#' image_files <- list.files("stimuli/", pattern = "\\.png$", full.names = TRUE)
#' eng <- tesseract::tesseract("eng")
#' roi_all <- purrr::imap_dfr(
#'   image_files,
#'   \(path, i) get_roi_from_ocr(path, trial_nr = i, engine = eng)
#' )
#' }
get_roi_from_ocr <- function(
    image_path,
    trial_nr       = 1L,
    level          = c("word", "character"),
    language       = "eng",
    engine         = NULL,
    min_confidence = 0
) {
  if (!requireNamespace("tesseract", quietly = TRUE)) {
    stop(
      "Package 'tesseract' is required for get_roi_from_ocr(). ",
      "Install it with: install.packages('tesseract')",
      call. = FALSE
    )
  }

  stopifnot(
    is.character(image_path), length(image_path) == 1L,
    file.exists(image_path)
  )

  trial_nr <- as.integer(trial_nr)
  stopifnot(length(trial_nr) == 1L, !is.na(trial_nr))

  level <- match.arg(level)

  stopifnot(
    is.numeric(min_confidence), length(min_confidence) == 1L,
    !is.na(min_confidence), min_confidence >= 0
  )

  if (is.null(engine)) {
    stopifnot(is.character(language), length(language) == 1L)
    engine <- tesseract::tesseract(language = language)
  }

  ocr_result <- tesseract::ocr_data(image_path, engine = engine)

  # Keep only tokens that pass confidence threshold and contain non-whitespace
  keep <- !is.na(ocr_result$confidence) &
    ocr_result$confidence >= min_confidence &
    nzchar(trimws(ocr_result$word))
  ocr_result <- ocr_result[keep, , drop = FALSE]

  # Tesseract's GetUTF8Text() appends a trailing newline to every word token;
  # strip leading/trailing whitespace so stored values are clean.
  ocr_result$word <- trimws(as.character(ocr_result$word))

  # Re-filter: trimming may have collapsed some tokens to empty strings.
  ocr_result <- ocr_result[nzchar(ocr_result$word), , drop = FALSE]

  # --- Empty result -----------------------------------------------------------
  if (nrow(ocr_result) == 0L) {
    return(.empty_roi_tibble(level))
  }

  # --- Parse bounding boxes ---------------------------------------------------
  # tesseract::ocr_data() returns bbox as "left,top,right,bottom"
  # (comma-separated integers), which already follows the EyeLink top-left
  # origin convention.
  bbox_parts <- strsplit(as.character(ocr_result$bbox), "[[:space:],]+")
  x_start <- as.numeric(vapply(bbox_parts, `[[`, character(1L), 1L))
  y_start <- as.numeric(vapply(bbox_parts, `[[`, character(1L), 2L))
  x_end   <- as.numeric(vapply(bbox_parts, `[[`, character(1L), 3L))
  y_end   <- as.numeric(vapply(bbox_parts, `[[`, character(1L), 4L))

  n_words <- nrow(ocr_result)

  # --- Word-level output ------------------------------------------------------
  if (level == "word") {
    return(dplyr::tibble(
      trial_nr = rep(trial_nr, n_words),
      word_id  = seq_len(n_words),
      word     = ocr_result$word,
      x_start  = x_start,
      x_end    = x_end,
      y_start  = y_start,
      y_end    = y_end
    ))
  }

  # --- Character-level output (proportional split of word bbox) ---------------
  char_rows <- vector("list", n_words)
  for (w in seq_len(n_words)) {
    wrd  <- ocr_result$word[[w]]
    nch  <- nchar(wrd)
    if (nch == 0L) next

    wx_s   <- x_start[[w]]
    wx_e   <- x_end[[w]]
    wy_s   <- y_start[[w]]
    wy_e   <- y_end[[w]]
    char_w <- (wx_e - wx_s) / nch
    chars  <- strsplit(wrd, "")[[1L]]

    char_rows[[w]] <- dplyr::tibble(
      trial_nr = rep(trial_nr, nch),
      word_id  = rep(as.integer(w), nch),
      char_id  = seq_len(nch),
      char     = chars,
      x_start  = wx_s + (seq_len(nch) - 1L) * char_w,
      x_end    = wx_s + seq_len(nch) * char_w,
      y_start  = rep(wy_s, nch),
      y_end    = rep(wy_e, nch)
    )
  }

  dplyr::bind_rows(char_rows)
}

# ---------------------------------------------------------------------------
# Internal helpers
# ---------------------------------------------------------------------------

#' Return a zero-row tibble with the correct schema
#' @noRd
.empty_roi_tibble <- function(level) {
  if (level == "word") {
    dplyr::tibble(
      trial_nr = integer(0),
      word_id  = integer(0),
      word     = character(0),
      x_start  = double(0),
      x_end    = double(0),
      y_start  = double(0),
      y_end    = double(0)
    )
  } else {
    dplyr::tibble(
      trial_nr = integer(0),
      word_id  = integer(0),
      char_id  = integer(0),
      char     = character(0),
      x_start  = double(0),
      x_end    = double(0),
      y_start  = double(0),
      y_end    = double(0)
    )
  }
}
