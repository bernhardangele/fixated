#' @title Read an EyeLink ASC file
#'
#' @description
#' Parses an EyeLink ASC file (converted from EDF with `edf2asc`) and returns
#' a named list with data frames for samples, events, and (when present)
#' word boundary and calibration information.
#'
#' Both **monocular** (single eye, 4 data columns) and **binocular** (both
#' eyes, 7 data columns) sample formats are supported.  Binocular files are
#' automatically detected from the `START` header line and produce two rows
#' per sample timestamp—one for each eye.
#'
#' The coordinate system origin follows the EyeLink convention: **(0, 0) is
#' the top-left corner** of the display, with x increasing rightward and y
#' increasing downward.
#'
#' @param path Character scalar.  Full path to the `.asc` file.
#' @param eyes Character vector.  Which eyes to return.  One or both of
#'   `"L"` and `"R"`.  Defaults to `c("L", "R")` (both).
#' @param sample_cols Character vector.  Additional MSG-based columns to
#'   parse into the samples table.  Currently unused; reserved for future
#'   extension.
#'
#' @return A named list with elements:
#'   \describe{
#'     \item{`samples`}{A [tibble][tibble::tibble] with columns
#'       `time` (integer, ms), `x` (double, pixels), `y` (double, pixels),
#'       `pupil` (double), `eye` (character, `"L"` or `"R"`).}
#'     \item{`events`}{A [tibble][tibble::tibble] with columns `type`
#'       (`"FIXATION"`, `"SACCADE"`, or `"BLINK"`), `eye`, `start_time`,
#'       `end_time`, `duration`, `x_start`, `y_start`, `x_end`, `y_end`,
#'       `avg_x`, `avg_y`, `avg_pupil`.}
#'     \item{`word_boundaries`}{A [tibble][tibble::tibble] with columns
#'       `trial_nr`, `item_nr`, `word_nr`, `word`, `right_boundary`
#'       (pixels), parsed from `TRIAL … ITEM … WORD … RIGHT_BOUNDARY …`
#'       messages written by OpenSesame.  `NULL` when no such messages are
#'       found.}
#'     \item{`calibration`}{A [tibble][tibble::tibble] with columns `eye`,
#'       `quality`, `avg_error`, `max_error`, `offset_deg`, `x_offset`,
#'       `y_offset`, parsed from `!CAL VALIDATION` messages.  `NULL` when
#'       no such messages are found.}
#'   }
#'
#' @importFrom dplyr tibble mutate filter bind_rows select rename arrange
#'   if_else group_by summarise
#' @importFrom stringr str_detect str_split str_trim str_extract str_remove
#'   str_starts str_squish str_match
#' @importFrom rlang .data
#'
#' @export
#'
#' @examples
#' asc_file <- system.file("extdata", "example.asc", package = "fixated")
#' if (file.exists(asc_file)) {
#'   result <- read_asc(asc_file)
#'   head(result$samples)
#'   head(result$events)
#' }
#'
#' # Binocular OpenSesame-style file with word boundaries
#' asc_bino <- system.file("extdata", "sub_1_example.asc", package = "fixated")
#' if (file.exists(asc_bino)) {
#'   result <- read_asc(asc_bino)
#'   head(result$samples)
#'   head(result$word_boundaries)
#'   head(result$calibration)
#' }
read_asc <- function(path, eyes = c("L", "R"), sample_cols = character(0)) {
  stopifnot(is.character(path), length(path) == 1L, file.exists(path))
  eyes <- match.arg(eyes, c("L", "R"), several.ok = TRUE)

  lines <- readLines(path, warn = FALSE)

  # ---- detect binocular recording format -----------------------------------
  is_binocular <- .detect_asc_binocular(lines)

  # ---- collect trial metadata from MSG lines --------------------------------
  trial_meta <- .parse_asc_messages(lines)

  # ---- parse raw samples ----------------------------------------------------
  samples <- .parse_asc_samples(lines, eyes, trial_meta, is_binocular)

  # ---- parse events (EFIX, ESACC, EBLINK) -----------------------------------
  events <- .parse_asc_events(lines, eyes, trial_meta)

  # ---- parse word boundaries (OpenSesame TRIAL/ITEM/WORD messages) ----------
  word_boundaries <- .parse_asc_word_boundaries(lines)

  # ---- parse calibration/validation info ------------------------------------
  calibration <- .parse_asc_calibration(lines)

  list(
    samples         = samples,
    events          = events,
    word_boundaries = word_boundaries,
    calibration     = calibration
  )
}

# ---------------------------------------------------------------------------
# Internal helpers for read_asc()
# ---------------------------------------------------------------------------

#' Detect whether an ASC file contains binocular recordings
#'
#' Looks for a `START` header line that lists both `LEFT` and `RIGHT`.
#' @noRd
.detect_asc_binocular <- function(lines) {
  start_idx <- which(stringr::str_starts(lines, "START"))
  for (i in start_idx) {
    if (stringr::str_detect(lines[[i]], "LEFT") &&
        stringr::str_detect(lines[[i]], "RIGHT")) {
      return(TRUE)
    }
  }
  FALSE
}

#' Parse MSG lines from an ASC file into a per-sample metadata table
#' @noRd
.parse_asc_messages <- function(lines) {
  msg_idx <- which(stringr::str_starts(lines, "MSG"))
  if (length(msg_idx) == 0L) {
    return(list())
  }

  # Build a simple mapping: time -> list of key=value pairs
  meta <- vector("list", length(msg_idx))
  for (i in seq_along(msg_idx)) {
    parts <- stringr::str_squish(lines[msg_idx[[i]]])
    parts <- stringr::str_split(parts, "\\s+")[[1]]
    # MSG <time> <content...>
    if (length(parts) < 3L) next
    time_val <- suppressWarnings(as.integer(parts[[2]]))
    if (is.na(time_val)) next
    content <- paste(parts[-(1:2)], collapse = " ")
    meta[[i]] <- list(time = time_val, content = content)
  }
  meta[!vapply(meta, is.null, logical(1))]
}

#' Parse raw sample lines from an ASC file
#'
#' Handles both monocular (4 data columns: time, x, y, pupil) and binocular
#' (7 data columns: time, xl, yl, pl, xr, yr, pr) formats.  Binocular files
#' produce two rows per timestamp, one per eye.
#' @noRd
.parse_asc_samples <- function(lines, eyes, trial_meta, is_binocular = FALSE) {
  # Sample lines start with a digit and are NOT keyword lines
  sample_pattern <- "^[0-9]"
  s_idx <- which(stringr::str_detect(lines, sample_pattern))
  if (length(s_idx) == 0L) {
    return(.empty_samples_tibble())
  }

  raw <- lines[s_idx]
  split_lines <- stringr::str_split(raw, "\\s+")
  n <- length(split_lines)

  if (is_binocular) {
    # Binocular format: time | xl | yl | pl | xr | yr | pr [| flags...]
    # Produces two rows per sample line (one for each eye)
    time_vec  <- integer(n)
    xl_vec    <- numeric(n)
    yl_vec    <- numeric(n)
    pl_vec    <- numeric(n)
    xr_vec    <- numeric(n)
    yr_vec    <- numeric(n)
    pr_vec    <- numeric(n)

    for (i in seq_len(n)) {
      cols        <- split_lines[[i]]
      time_vec[i] <- suppressWarnings(as.integer(cols[[1L]]))
      xl_vec[i]   <- if (length(cols) >= 2L) suppressWarnings(as.numeric(cols[[2L]])) else NA_real_
      yl_vec[i]   <- if (length(cols) >= 3L) suppressWarnings(as.numeric(cols[[3L]])) else NA_real_
      pl_vec[i]   <- if (length(cols) >= 4L) suppressWarnings(as.numeric(cols[[4L]])) else NA_real_
      xr_vec[i]   <- if (length(cols) >= 5L) suppressWarnings(as.numeric(cols[[5L]])) else NA_real_
      yr_vec[i]   <- if (length(cols) >= 6L) suppressWarnings(as.numeric(cols[[6L]])) else NA_real_
      pr_vec[i]   <- if (length(cols) >= 7L) suppressWarnings(as.numeric(cols[[7L]])) else NA_real_
    }

    left_rows <- dplyr::tibble(
      time  = time_vec,
      x     = xl_vec,
      y     = yl_vec,
      pupil = pl_vec,
      eye   = "L"
    )
    right_rows <- dplyr::tibble(
      time  = time_vec,
      x     = xr_vec,
      y     = yr_vec,
      pupil = pr_vec,
      eye   = "R"
    )
    out <- dplyr::bind_rows(left_rows, right_rows)
  } else {
    # Monocular format: time | x | y | pupil [| eye]
    time_vec  <- integer(n)
    x_vec     <- numeric(n)
    y_vec     <- numeric(n)
    pupil_vec <- numeric(n)
    eye_vec   <- character(n)

    for (i in seq_len(n)) {
      cols <- split_lines[[i]]
      time_vec[[i]]  <- suppressWarnings(as.integer(cols[[1L]]))
      x_val <- suppressWarnings(as.numeric(cols[[2L]]))
      y_val <- suppressWarnings(as.numeric(cols[[3L]]))
      p_val <- suppressWarnings(as.numeric(cols[[4L]]))
      x_vec[[i]]     <- if (is.na(x_val)) NA_real_ else x_val
      y_vec[[i]]     <- if (is.na(y_val)) NA_real_ else y_val
      pupil_vec[[i]] <- if (is.na(p_val)) NA_real_ else p_val
      # Optional 5th column with eye label (some EyeLink formats)
      eye_vec[[i]] <- if (length(cols) >= 5L && cols[[5L]] %in% c("L", "R")) {
        cols[[5L]]
      } else {
        "L" # monocular default
      }
    }

    out <- dplyr::tibble(
      time  = time_vec,
      x     = x_vec,
      y     = y_vec,
      pupil = pupil_vec,
      eye   = eye_vec
    )
  }

  out <- dplyr::filter(out, .data$eye %in% eyes, !is.na(.data$time))
  out <- dplyr::arrange(out, .data$time, .data$eye)
  out
}

#' Parse event lines (EFIX, ESACC, EBLINK) from an ASC file
#' @noRd
.parse_asc_events <- function(lines, eyes, trial_meta) {
  fix_idx   <- which(stringr::str_starts(lines, "EFIX"))
  sacc_idx  <- which(stringr::str_starts(lines, "ESACC"))
  blink_idx <- which(stringr::str_starts(lines, "EBLINK"))

  fix_rows   <- .parse_efix_lines(lines[fix_idx])
  sacc_rows  <- .parse_esacc_lines(lines[sacc_idx])
  blink_rows <- .parse_eblink_lines(lines[blink_idx])

  out <- dplyr::bind_rows(fix_rows, sacc_rows, blink_rows)
  if (nrow(out) == 0L) return(.empty_events_tibble())

  out <- dplyr::filter(out, .data$eye %in% eyes)
  out <- dplyr::arrange(out, .data$start_time)
  out
}

#' Parse EFIX lines
#' Format: EFIX <eye> <start> <end> <duration> <avg_x> <avg_y> <avg_pupil>
#' @noRd
.parse_efix_lines <- function(lines) {
  if (length(lines) == 0L) return(.empty_events_tibble())
  split_lines <- stringr::str_split(stringr::str_squish(lines), "\\s+")
  rows <- lapply(split_lines, function(cols) {
    if (length(cols) < 8L) return(NULL)
    list(
      type       = "FIXATION",
      eye        = cols[[2]],
      start_time = suppressWarnings(as.integer(cols[[3]])),
      end_time   = suppressWarnings(as.integer(cols[[4]])),
      duration   = suppressWarnings(as.integer(cols[[5]])),
      x_start    = NA_real_,
      y_start    = NA_real_,
      x_end      = NA_real_,
      y_end      = NA_real_,
      avg_x      = suppressWarnings(as.numeric(cols[[6]])),
      avg_y      = suppressWarnings(as.numeric(cols[[7]])),
      avg_pupil  = suppressWarnings(as.numeric(cols[[8]]))
    )
  })
  rows <- rows[!vapply(rows, is.null, logical(1))]
  if (length(rows) == 0L) return(.empty_events_tibble())
  dplyr::bind_rows(rows)
}

#' Parse ESACC lines
#' Format: ESACC <eye> <start> <end> <duration> <x1> <y1> <x2> <y2> <ampl> <pvel>
#' @noRd
.parse_esacc_lines <- function(lines) {
  if (length(lines) == 0L) return(.empty_events_tibble())
  split_lines <- stringr::str_split(stringr::str_squish(lines), "\\s+")
  rows <- lapply(split_lines, function(cols) {
    if (length(cols) < 10L) return(NULL)
    list(
      type       = "SACCADE",
      eye        = cols[[2]],
      start_time = suppressWarnings(as.integer(cols[[3]])),
      end_time   = suppressWarnings(as.integer(cols[[4]])),
      duration   = suppressWarnings(as.integer(cols[[5]])),
      x_start    = suppressWarnings(as.numeric(cols[[6]])),
      y_start    = suppressWarnings(as.numeric(cols[[7]])),
      x_end      = suppressWarnings(as.numeric(cols[[8]])),
      y_end      = suppressWarnings(as.numeric(cols[[9]])),
      avg_x      = NA_real_,
      avg_y      = NA_real_,
      avg_pupil  = NA_real_
    )
  })
  rows <- rows[!vapply(rows, is.null, logical(1))]
  if (length(rows) == 0L) return(.empty_events_tibble())
  dplyr::bind_rows(rows)
}

#' Parse EBLINK lines
#' Format: EBLINK <eye> <start> <end> <duration>
#' @noRd
.parse_eblink_lines <- function(lines) {
  if (length(lines) == 0L) return(.empty_events_tibble())
  split_lines <- stringr::str_split(stringr::str_squish(lines), "\\s+")
  rows <- lapply(split_lines, function(cols) {
    if (length(cols) < 5L) return(NULL)
    list(
      type       = "BLINK",
      eye        = cols[[2]],
      start_time = suppressWarnings(as.integer(cols[[3]])),
      end_time   = suppressWarnings(as.integer(cols[[4]])),
      duration   = suppressWarnings(as.integer(cols[[5]])),
      x_start    = NA_real_,
      y_start    = NA_real_,
      x_end      = NA_real_,
      y_end      = NA_real_,
      avg_x      = NA_real_,
      avg_y      = NA_real_,
      avg_pupil  = NA_real_
    )
  })
  rows <- rows[!vapply(rows, is.null, logical(1))]
  if (length(rows) == 0L) return(.empty_events_tibble())
  dplyr::bind_rows(rows)
}

#' Parse word boundary messages written by OpenSesame
#'
#' Expects lines of the form:
#'   `MSG\t<time> TRIAL <trial> ITEM <item> WORD <word_nr> <word> RIGHT_BOUNDARY <x>`
#'
#' @return A tibble with columns `trial_nr`, `item_nr`, `word_nr`, `word`,
#'   `right_boundary`, or `NULL` when no such messages are present.
#' @noRd
.parse_asc_word_boundaries <- function(lines) {
  pattern <- "^MSG\\t\\d+\\s+TRIAL\\s+(\\d+)\\s+ITEM\\s+(\\d+)\\s+WORD\\s+(\\d+)\\s+(\\S+)\\s+RIGHT_BOUNDARY\\s+(\\d+)"
  wb_idx <- which(stringr::str_detect(lines, pattern))
  if (length(wb_idx) == 0L) return(NULL)

  m <- stringr::str_match(lines[wb_idx], pattern)
  dplyr::tibble(
    trial_nr        = as.integer(m[, 2L]),
    item_nr         = as.integer(m[, 3L]),
    word_nr         = as.integer(m[, 4L]),
    word            = m[, 5L],
    right_boundary  = as.integer(m[, 6L])
  )
}

#' Parse calibration/validation summary messages
#'
#' Expects lines of the form:
#'   `MSG\t<time> !CAL VALIDATION HV9 LR LEFT/RIGHT <quality> ERROR <avg> avg. <max> max OFFSET <deg> deg. <x>,<y> pix.`
#'
#' @return A tibble with columns `eye`, `quality`, `avg_error`, `max_error`,
#'   `offset_deg`, `x_offset`, `y_offset`, or `NULL` when no such messages
#'   are present.
#' @noRd
.parse_asc_calibration <- function(lines) {
  pattern <- paste0(
    "MSG\\t\\d+\\s+!CAL\\s+VALIDATION\\s+\\S+\\s+\\S+\\s+",
    "(LEFT|RIGHT)\\s+(\\w+)\\s+ERROR\\s+([\\d.]+)\\s+avg\\.\\s+",
    "([\\d.]+)\\s+max\\s+OFFSET\\s+([\\d.]+)\\s+deg\\.\\s+",
    "([-\\d.]+),([-\\d.]+)\\s+pix\\."
  )
  cal_idx <- which(stringr::str_detect(lines, pattern))
  if (length(cal_idx) == 0L) return(NULL)

  m <- stringr::str_match(lines[cal_idx], pattern)
  dplyr::tibble(
    eye        = m[, 2L],
    quality    = m[, 3L],
    avg_error  = as.numeric(m[, 4L]),
    max_error  = as.numeric(m[, 5L]),
    offset_deg = as.numeric(m[, 6L]),
    x_offset   = as.numeric(m[, 7L]),
    y_offset   = as.numeric(m[, 8L])
  )
}

#' Empty samples tibble prototype
#' @noRd
.empty_samples_tibble <- function() {
  dplyr::tibble(
    time  = integer(0),
    x     = numeric(0),
    y     = numeric(0),
    pupil = numeric(0),
    eye   = character(0)
  )
}

#' Empty events tibble prototype
#' @noRd
.empty_events_tibble <- function() {
  dplyr::tibble(
    type       = character(0),
    eye        = character(0),
    start_time = integer(0),
    end_time   = integer(0),
    duration   = integer(0),
    x_start    = numeric(0),
    y_start    = numeric(0),
    x_end      = numeric(0),
    y_end      = numeric(0),
    avg_x      = numeric(0),
    avg_y      = numeric(0),
    avg_pupil  = numeric(0)
  )
}
