#' @title Read an EyeLink ASC file
#'
#' @description
#' Parses an EyeLink ASC file (converted from EDF with `edf2asc`) and returns
#' a named list with data frames for samples, events, and (when present)
#' word boundary, calibration, and trial information.
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
#' @param eye_tracker Character scalar.  Shortcut for common eye-tracker
#'   setups that presets `trial_start_pattern` and `trial_end_pattern`.
#'   Currently supported values are `"eyelink_opensesame"` (default) and
#'   `"custom"`.  For `"eyelink_opensesame"`, the trial start is detected
#'   from `TRIAL … ITEM … WORD 1` ROI messages written by OpenSesame, and
#'   the trial end from `END` lines written by the EyeLink host.  Set to
#'   `"custom"` and supply `trial_start_pattern`/`trial_end_pattern` for
#'   other setups.
#' @param trial_start_pattern Character scalar or `NULL`.  A regular
#'   expression that matches the **first line** of each trial.  When
#'   non-`NULL`, overrides the default for `eye_tracker`.  Pass `NULL` (the
#'   default) to use the pattern implied by `eye_tracker`.
#' @param trial_end_pattern Character scalar or `NULL`.  A regular expression
#'   that matches the **last line** (or a line within) each trial.  When
#'   non-`NULL`, overrides the default for `eye_tracker`.
#' @param parse_vars Logical scalar.  When `TRUE` (default) and
#'   `eye_tracker = "eyelink_opensesame"`, OpenSesame variable messages of
#'   the form `MSG … var KEY VALUE` with a single-word value are parsed and
#'   added as columns to `trial_db`.  Set to `FALSE` to skip this step.
#'
#' @return A named list with elements:
#'   \describe{
#'     \item{`samples`}{A [tibble][tibble::tibble] with columns
#'       `time` (integer, ms), `x` (double, pixels), `y` (double, pixels),
#'       `pupil` (double), `eye` (character, `"L"` or `"R"`).  When trial
#'       information is found a `trial_nr` column is also included.}
#'     \item{`events`}{A [tibble][tibble::tibble] with columns `type`
#'       (`"FIXATION"`, `"SACCADE"`, or `"BLINK"`), `eye`, `start_time`,
#'       `end_time`, `duration`, `x_start`, `y_start`, `x_end`, `y_end`,
#'       `avg_x`, `avg_y`, `avg_pupil`.  When trial information is found a
#'       `trial_nr` column is also included.}
#'     \item{`word_boundaries`}{A [tibble][tibble::tibble] with columns
#'       `trial_nr`, `item_nr`, `word_nr`, `word`, `right_boundary`
#'       (pixels), parsed from `TRIAL … ITEM … WORD … RIGHT_BOUNDARY …`
#'       messages written by OpenSesame.  `NULL` when no such messages are
#'       found.}
#'     \item{`calibration`}{A [tibble][tibble::tibble] with columns `eye`,
#'       `quality`, `avg_error`, `max_error`, `offset_deg`, `x_offset`,
#'       `y_offset`, parsed from `!CAL VALIDATION` messages.  `NULL` when
#'       no such messages are found.}
#'     \item{`trial_db`}{A [tibble][tibble::tibble] with one row per trial
#'       and columns `trial_nr`, `item_nr`, `t_trial_start`,
#'       `t_recording_start`, `t_gaze_target_on`, `t_gaze_target_off`,
#'       `t_display_on`, `t_display_off`, `t_trial_end`, `has_display_off`.
#'       When `parse_vars = TRUE` and `eye_tracker = "eyelink_opensesame"`,
#'       additional columns are appended for each OpenSesame variable whose
#'       value is a simple scalar (no spaces).  `NULL` when no trial
#'       structure is detected.}
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
#' # Binocular OpenSesame-style file with word boundaries and trial structure
#' asc_bino <- system.file("extdata", "sub_1_example.asc", package = "fixated")
#' if (file.exists(asc_bino)) {
#'   result <- read_asc(asc_bino)
#'   head(result$samples)
#'   head(result$word_boundaries)
#'   head(result$calibration)
#'   print(result$trial_db)
#'   # Get samples and events for trial 0
#'   trial0 <- get_trial(result, 0L)
#' }
read_asc <- function(path,
                     eyes                 = c("L", "R"),
                     sample_cols          = character(0),
                     eye_tracker          = "eyelink_opensesame",
                     trial_start_pattern  = NULL,
                     trial_end_pattern    = NULL,
                     parse_vars           = TRUE) {
  stopifnot(is.character(path), length(path) == 1L, file.exists(path))
  eyes <- match.arg(eyes, c("L", "R"), several.ok = TRUE)
  eye_tracker <- match.arg(eye_tracker, c("eyelink_opensesame", "custom"))

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

  # ---- resolve trial start/end patterns ------------------------------------
  patterns <- .resolve_trial_patterns(eye_tracker,
                                      trial_start_pattern,
                                      trial_end_pattern)

  # ---- parse trial structure -----------------------------------------------
  trial_db <- NULL
  if (!is.null(patterns$start)) {
    trial_db <- .parse_asc_trial_db(lines,
                                     patterns$start,
                                     patterns$end,
                                     eye_tracker,
                                     parse_vars)
    if (!is.null(trial_db) && nrow(trial_db) > 0L) {
      samples <- .assign_trial_nr(samples, trial_db, time_col = "time")
      events  <- .assign_trial_nr(events,  trial_db, time_col = "start_time")
    }
  }

  list(
    samples         = samples,
    events          = events,
    word_boundaries = word_boundaries,
    calibration     = calibration,
    trial_db        = trial_db
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

#' Extract a numeric value from a whitespace-split column vector
#'
#' Returns the numeric value of `cols[[idx]]` if `cols` is long enough,
#' otherwise returns `NA_real_`.  Suppresses coercion warnings so that
#' EyeLink's missing-data placeholder `"."` silently becomes `NA`.
#' @noRd
.asc_col_num <- function(cols, idx) {
  if (length(cols) >= idx) suppressWarnings(as.numeric(cols[[idx]])) else NA_real_
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
      xl_vec[i]   <- .asc_col_num(cols, 2L)
      yl_vec[i]   <- .asc_col_num(cols, 3L)
      pl_vec[i]   <- .asc_col_num(cols, 4L)
      xr_vec[i]   <- .asc_col_num(cols, 5L)
      yr_vec[i]   <- .asc_col_num(cols, 6L)
      pr_vec[i]   <- .asc_col_num(cols, 7L)
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

# ---------------------------------------------------------------------------
# Trial structure helpers
# ---------------------------------------------------------------------------

#' Resolve trial start/end patterns from eye_tracker shortcut or user input
#' @noRd
.resolve_trial_patterns <- function(eye_tracker,
                                     user_start = NULL,
                                     user_end   = NULL) {
  default_end <- "^END\\b"

  if (!is.null(user_start)) {
    return(list(
      start = user_start,
      end   = if (!is.null(user_end)) user_end else default_end
    ))
  }

  if (eye_tracker == "eyelink_opensesame") {
    return(list(
      start = "^MSG\\t\\d+\\s+TRIAL\\s+\\d+\\s+ITEM\\s+\\d+\\s+WORD\\s+1\\b",
      end   = default_end
    ))
  }

  # "custom" with no user_start → no trial parsing
  list(start = NULL, end = NULL)
}

#' Parse trial metadata from ASC file lines into a trial_db tibble
#'
#' @param lines Character vector of all lines.
#' @param start_pattern Regex matching the first line of each trial.
#' @param end_pattern Regex matching the last line of each trial (`END`).
#' @param eye_tracker Character scalar identifying the eye-tracker type.
#' @param parse_vars Logical; parse OpenSesame `var` messages?
#' @return Tibble with one row per trial, or `NULL` if no trials found.
#' @noRd
.parse_asc_trial_db <- function(lines,
                                 start_pattern,
                                 end_pattern,
                                 eye_tracker = "eyelink_opensesame",
                                 parse_vars  = TRUE) {
  start_idx <- which(stringr::str_detect(lines, start_pattern))
  if (length(start_idx) == 0L) return(NULL)

  n_trials   <- length(start_idx)
  n_lines    <- length(lines)
  # Upper line boundary for each trial (exclusive): first line of next trial
  next_start <- c(start_idx[-1L], n_lines + 1L)

  # --- extract trial_nr, item_nr, t_trial_start from WORD 1 messages --------
  wb_pat    <- "^MSG\\t(\\d+)\\s+TRIAL\\s+(\\d+)\\s+ITEM\\s+(\\d+)\\s+WORD\\s+1\\b"
  m_start   <- stringr::str_match(lines[start_idx], wb_pat)
  t_starts  <- suppressWarnings(as.integer(m_start[, 2L]))
  trial_nrs <- suppressWarnings(as.integer(m_start[, 3L]))
  item_nrs  <- suppressWarnings(as.integer(m_start[, 4L]))

  # If the pattern did not produce trial_nr/item_nr (custom tracker), use
  # sequential indices and try to pull a timestamp from generic MSG lines.
  if (all(is.na(trial_nrs))) {
    m_gen     <- stringr::str_match(lines[start_idx], "^MSG\\t(\\d+)")
    t_starts  <- suppressWarnings(as.integer(m_gen[, 2L]))
    trial_nrs <- seq_len(n_trials)
    item_nrs  <- rep(NA_integer_, n_trials)
  }

  # --- find END timestamps --------------------------------------------------
  t_ends <- rep(NA_integer_, n_trials)
  if (!is.null(end_pattern) && nchar(end_pattern) > 0L) {
    end_idx <- which(stringr::str_detect(lines, end_pattern))
    if (length(end_idx) > 0L) {
      for (i in seq_len(n_trials)) {
        cands <- end_idx[end_idx >= start_idx[i] & end_idx < next_start[i]]
        if (length(cands) > 0L) {
          m_e <- stringr::str_match(lines[cands[1L]], "^END\\s+(\\d+)")
          if (!is.na(m_e[2L])) t_ends[i] <- as.integer(m_e[2L])
        }
      }
    }
  }

  # --- single-pass scan for per-trial event timestamps ----------------------
  # Helper: given a set of line indices and their extracted timestamps,
  # return the first timestamp per trial.
  first_per_trial <- function(idx, timestamps) {
    if (length(idx) == 0L) return(rep(NA_integer_, n_trials))
    ti     <- findInterval(idx, start_idx)  # 0 if before first trial, 1-based trial index otherwise
    valid  <- ti >= 1L & ti <= n_trials & idx < next_start[pmax(ti, 1L)]
    idx    <- idx[valid];  ti <- ti[valid];  timestamps <- timestamps[valid]
    if (length(idx) == 0L) return(rep(NA_integer_, n_trials))
    # Keep only first occurrence per trial (lines are in ascending order)
    dup    <- duplicated(ti)
    result <- rep(NA_integer_, n_trials)
    result[ti[!dup]] <- timestamps[!dup]
    result
  }

  # Helper: scan for MSG lines matching a pattern; extract timestamp col 2
  scan_msg <- function(pattern) {
    idx <- which(stringr::str_detect(lines, pattern))
    if (length(idx) == 0L) {
      return(list(idx = integer(0), ts = integer(0)))
    }
    m  <- stringr::str_match(lines[idx], "^MSG\\t(\\d+)")
    ts <- suppressWarnings(as.integer(m[, 2L]))
    list(idx = idx, ts = ts)
  }

  # START line (not a MSG line)
  start_rec_idx <- which(stringr::str_starts(lines, "START"))
  start_rec_m   <- stringr::str_match(lines[start_rec_idx], "^START\\s+(\\d+)")
  t_rec_start   <- first_per_trial(start_rec_idx,
                                    suppressWarnings(as.integer(start_rec_m[, 2L])))

  gaze_on  <- scan_msg("^MSG\\t\\d+\\s+GAZE TARGET ON")
  gaze_off <- scan_msg("^MSG\\t\\d+\\s+GAZE TARGET OFF")
  disp_on  <- scan_msg("^MSG\\t\\d+\\s+DISPLAY ON")
  disp_off <- scan_msg("^MSG\\t\\d+\\s+DISPLAY OFF")

  t_gaze_on  <- first_per_trial(gaze_on$idx,  gaze_on$ts)
  t_gaze_off <- first_per_trial(gaze_off$idx, gaze_off$ts)
  t_disp_on  <- first_per_trial(disp_on$idx,  disp_on$ts)
  t_disp_off <- first_per_trial(disp_off$idx, disp_off$ts)

  # --- assemble trial_db ----------------------------------------------------
  trial_db <- dplyr::tibble(
    trial_nr          = trial_nrs,
    item_nr           = item_nrs,
    t_trial_start     = t_starts,
    t_recording_start = t_rec_start,
    t_gaze_target_on  = t_gaze_on,
    t_gaze_target_off = t_gaze_off,
    t_display_on      = t_disp_on,
    t_display_off     = t_disp_off,
    t_trial_end       = t_ends,
    has_display_off   = !is.na(t_disp_off)
  )

  # --- optionally add OpenSesame var columns --------------------------------
  if (isTRUE(parse_vars) && eye_tracker == "eyelink_opensesame") {
    trial_db <- .add_opensesame_vars(lines, start_idx, next_start, trial_db)
  }

  trial_db
}

#' Assign `trial_nr` column to a data frame based on trial time boundaries
#'
#' @param df Data frame (samples or events).
#' @param trial_db Tibble produced by `.parse_asc_trial_db()`.
#' @param time_col Name of the timestamp column in `df`.
#' @return `df` with a `trial_nr` integer column appended.
#' @noRd
.assign_trial_nr <- function(df, trial_db, time_col = "time") {
  if (is.null(df) || nrow(df) == 0L ||
      is.null(trial_db) || nrow(trial_db) == 0L) {
    df$trial_nr <- NA_integer_
    return(df)
  }
  if (!time_col %in% names(df)) {
    df$trial_nr <- NA_integer_
    return(df)
  }

  times   <- df[[time_col]]
  starts  <- trial_db$t_trial_start
  ends    <- trial_db$t_trial_end
  t_nrs   <- trial_db$trial_nr
  n       <- nrow(trial_db)

  # findInterval: for each time, idx gives the 1-based trial index where
  # starts[idx] <= time < starts[idx+1]; 0 means before the first trial.
  idx     <- findInterval(times, starts)
  valid   <- idx >= 1L & idx <= n

  # Check upper bound against t_trial_end (NA end = no upper limit)
  t_end_v <- ifelse(valid, ends[pmax(idx, 1L)], NA_integer_)
  upper_ok <- is.na(t_end_v) | times <= t_end_v

  df$trial_nr <- as.integer(ifelse(valid & upper_ok,
                                    t_nrs[pmax(idx, 1L)],
                                    NA_integer_))
  df
}

#' Parse OpenSesame `var KEY VALUE` messages and append as columns to trial_db
#'
#' Only "simple" variables (single-word value, no spaces) are included.
#'
#' @param lines Character vector of all ASC lines.
#' @param start_idx Integer vector of line indices where each trial starts.
#' @param next_start Integer vector of exclusive upper line bounds per trial.
#' @param trial_db Tibble to augment.
#' @return `trial_db` with additional character columns for each var found.
#' @noRd
.add_opensesame_vars <- function(lines, start_idx, next_start, trial_db) {
  # Match lines of the form: MSG\t<time> var <key> <single_word_value>
  var_pattern <- "^MSG\\t\\d+\\s+var\\s+(\\w+)\\s+(\\S+)$"
  var_idx     <- which(stringr::str_detect(lines, var_pattern))
  if (length(var_idx) == 0L) return(trial_db)

  m          <- stringr::str_match(lines[var_idx], var_pattern)
  var_names  <- m[, 2L]
  var_values <- m[, 3L]

  n_trials <- nrow(trial_db)
  # Assign each var line to a trial
  ti    <- findInterval(var_idx, start_idx)
  valid <- ti >= 1L & ti <= n_trials & var_idx < next_start[pmax(ti, 1L)]

  if (!any(valid)) return(trial_db)

  ti         <- ti[valid]
  var_names  <- var_names[valid]
  var_values <- var_values[valid]

  unique_vars <- unique(var_names)
  # Pre-build a matrix of NA strings, then fill in values
  vars_wide <- matrix(NA_character_,
                       nrow     = n_trials,
                       ncol     = length(unique_vars),
                       dimnames = list(NULL, unique_vars))

  for (vname in unique_vars) {
    is_v     <- var_names == vname
    v_trials <- ti[is_v]
    v_vals   <- var_values[is_v]
    # Keep last occurrence per trial (lines are in ascending order)
    dup              <- duplicated(v_trials, fromLast = TRUE)
    vars_wide[v_trials[!dup], vname] <- v_vals[!dup]
  }

  for (vname in unique_vars) {
    trial_db[[vname]] <- vars_wide[, vname]
  }
  trial_db
}
