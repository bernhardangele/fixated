#' @title Read a UMass EyeTrack ASC file
#'
#' @description
#' Parses an EyeLink ASC file recorded by UMass EyeTrack 6 and returns a named
#' list with data frames for fixations, word-level regions of interest (ROIs),
#' and trial metadata.
#'
#' EyeTrack ASC files differ from OpenSesame-recorded EyeLink ASC files in the
#' following ways:
#'
#' * **Trial structure** is delimited by `MSG … TRIALID <id>` at the start
#'   and `MSG … TRIAL_RESULT` at the end.
#' * **TRIALID** encodes the condition and item numbers in the format
#'   `[prefix][cond]I[item]D[n]` (e.g., `P1I3D0`).
#' * **Display onset** is recorded as `MSG … SYNCTIME`, which immediately
#'   follows `MSG … DISPLAY ON`.
#' * **Word ROIs** are specified at the character level via
#'   `MSG … REGION CHAR <idx> <line> <char> <x_l> <y_t> <x_r> <y_b>` messages.
#'   Adjacent non-space characters are merged into word-level ROIs.
#' * Only the **right eye** (`EFIX R`) fixation events are used.
#'
#' @param path Character scalar.  Full path to the EyeTrack `.asc` file.
#' @param trial_pattern Character scalar.  A regular expression used to filter
#'   TRIALID labels.  Only trials whose ID matches this pattern (via
#'   [grepl()]) are included.  Defaults to `"^[A-Za-z]"` (any ID starting
#'   with a letter, which is the typical EyeTrack convention).
#'
#' @return A named list with three elements:
#'   \describe{
#'     \item{`fixations`}{A [tibble][tibble::tibble] with one row per fixation
#'       and columns `trial_nr` (sequential integer), `start_time`,
#'       `end_time`, `duration` (all integer, ms), `avg_x`, `avg_y` (double,
#'       pixels).  This tibble can be passed directly to
#'       [compute_eye_measures()].}
#'     \item{`word_boundaries`}{A [tibble][tibble::tibble] with one row per
#'       trial × word and columns `trial_nr`, `word_id`, `word`, `x_start`,
#'       `x_end`, `y_start`, `y_end`.  This tibble can be passed directly to
#'       [compute_eye_measures()] as the `roi` argument.}
#'     \item{`trial_db`}{A [tibble][tibble::tibble] with one row per trial and
#'       columns `trial_nr`, `trial_id`, `prefix`, `cond`, `item`, `seq`,
#'       `t_trial_start`, `t_display_on`, `t_display_off`, `t_trial_end`.
#'       `t_display_on` is the EyeLink timestamp of the `SYNCTIME` message
#'       (equivalent to `t_display_on` in [compute_eye_measures()]'s
#'       `trial_db`).  This tibble can be passed directly to
#'       [compute_eye_measures()] as the `trial_db` argument.}
#'   }
#'
#' @seealso [compute_eye_measures()], [read_asc()]
#'
#' @importFrom dplyr tibble bind_rows as_tibble
#'
#' @export
#'
#' @examples
#' asc_file <- system.file("extdata", "eyetrack_example.asc",
#'                         package = "fixated")
#' if (file.exists(asc_file)) {
#'   result <- read_eyetrack_asc(asc_file)
#'   print(result$trial_db)
#'   print(result$word_boundaries)
#'   print(result$fixations)
#' }
read_eyetrack_asc <- function(path, trial_pattern = "^[A-Za-z]") {
  stopifnot(is.character(path), length(path) == 1L, file.exists(path))
  stopifnot(is.character(trial_pattern), length(trial_pattern) == 1L)

  lines <- readLines(path, warn = FALSE)

  fix_rows   <- list()
  roi_rows   <- list()
  trial_rows <- list()

  in_trial    <- FALSE
  seq_n       <- 0L
  cond        <- NA_integer_
  item        <- NA_integer_
  tid         <- NA_character_
  prefix      <- NA_character_
  t_start     <- NA_real_
  t_sync      <- NA_real_
  t_disp_off  <- NA_real_
  chars_list  <- list()
  efix_list   <- list()

  for (line in lines) {
    # --- Trial start: TRIALID message ---------------------------------------
    if (grepl("MSG.*TRIALID\\s+(\\S+)", line)) {
      m_tid <- regmatches(line, regexpr("TRIALID\\s+(\\S+)", line))
      if (length(m_tid) > 0L) {
        trial_id_str <- sub("TRIALID\\s+", "", m_tid)
        if (grepl(trial_pattern, trial_id_str)) {
          # Parse [prefix][cond][letter][item]
          m <- regmatches(trial_id_str,
                          regexpr("^([A-Za-z]+)(\\d+)[A-Za-z](\\d+)",
                                  trial_id_str))
          if (length(m) > 0L) {
            nums <- regmatches(m, gregexpr("\\d+", m))[[1L]]
            pfx  <- regmatches(m, gregexpr("[A-Za-z]+", m))[[1L]][1L]
            if (length(nums) >= 2L) {
              # Extract EyeLink timestamp from MSG line
              ts_m <- regmatches(line, regexpr("^MSG\\t(\\d+)", line))
              ts   <- if (length(ts_m) > 0L)
                        as.numeric(sub("^MSG\\t", "", ts_m)) else NA_real_
              seq_n      <- seq_n + 1L
              cond       <- as.integer(nums[[1L]])
              item       <- as.integer(nums[[2L]])
              tid        <- trial_id_str
              prefix     <- pfx
              t_start    <- ts
              t_sync     <- NA_real_
              t_disp_off <- NA_real_
              chars_list <- list()
              efix_list  <- list()
              in_trial   <- TRUE
            }
          }
        } else {
          in_trial <- FALSE
        }
      }
      next
    }

    if (!in_trial) next

    # --- Character region message -------------------------------------------
    if (grepl("REGION CHAR", line)) {
      parts <- strsplit(trimws(line), "\\s+")[[1L]]
      np    <- length(parts)
      if (np >= 10L) {
        idx  <- as.integer(parts[[5L]])
        ch   <- if (np >= 11L) parts[[7L]] else " "
        x_l  <- as.numeric(parts[[np - 3L]])
        y_t  <- as.numeric(parts[[np - 2L]])
        x_r  <- as.numeric(parts[[np - 1L]])
        y_b  <- as.numeric(parts[[np]])
        chars_list[[length(chars_list) + 1L]] <- list(
          idx = idx, char = ch,
          x_left = x_l, y_top = y_t, x_right = x_r, y_bottom = y_b
        )
      }
      next
    }

    # --- Display onset: SYNCTIME message ------------------------------------
    if (grepl("SYNCTIME", line)) {
      ts_m <- regmatches(line, regexpr("^MSG\\t(\\d+)", line))
      t_sync <- if (length(ts_m) > 0L)
                  as.numeric(sub("^MSG\\t", "", ts_m)) else NA_real_
      next
    }

    # --- Display off --------------------------------------------------------
    if (grepl("DISPLAY OFF", line)) {
      ts_m <- regmatches(line, regexpr("^MSG\\t(\\d+)", line))
      t_disp_off <- if (length(ts_m) > 0L)
                      as.numeric(sub("^MSG\\t", "", ts_m)) else NA_real_
      next
    }

    # --- Fixation event (right eye) -----------------------------------------
    if (grepl("^EFIX\\s+R\\s", line)) {
      parts <- strsplit(trimws(line), "\\s+")[[1L]]
      if (length(parts) >= 7L) {
        efix_list[[length(efix_list) + 1L]] <- list(
          t_start  = as.numeric(parts[[3L]]),
          t_end    = as.numeric(parts[[4L]]),
          duration = as.numeric(parts[[5L]]),
          x        = as.numeric(parts[[6L]]),
          y        = as.numeric(parts[[7L]])
        )
      }
      next
    }

    # --- Trial end: TRIAL_RESULT message ------------------------------------
    if (grepl("TRIAL_RESULT", line)) {
      if (in_trial && length(chars_list) > 0L) {
        ts_m  <- regmatches(line, regexpr("^MSG\\t(\\d+)", line))
        t_end <- if (length(ts_m) > 0L)
                   as.numeric(sub("^MSG\\t", "", ts_m)) else NA_real_

        # Build word ROIs from character regions
        chars_df <- do.call(rbind, lapply(chars_list, as.data.frame,
                                          stringsAsFactors = FALSE))
        chars_df <- chars_df[order(chars_df$idx), , drop = FALSE]
        word_rois <- .build_word_rois_from_chars(chars_df)

        if (nrow(word_rois) > 0L) {
          word_rois$trial_nr <- seq_n
          roi_rows[[length(roi_rows) + 1L]] <- word_rois
        }

        # Collect fixation rows for this trial
        if (length(efix_list) > 0L) {
          for (fi in seq_along(efix_list)) {
            ef <- efix_list[[fi]]
            fix_rows[[length(fix_rows) + 1L]] <- list(
              trial_nr   = seq_n,
              start_time = as.integer(round(ef$t_start)),
              end_time   = as.integer(round(ef$t_end)),
              duration   = as.integer(round(ef$duration)),
              avg_x      = ef$x,
              avg_y      = ef$y
            )
          }
        }

        trial_rows[[length(trial_rows) + 1L]] <- list(
          trial_nr     = seq_n,
          trial_id     = tid,
          prefix       = prefix,
          cond         = cond,
          item         = item,
          seq          = seq_n,
          t_trial_start  = as.integer(round(t_start)),
          t_display_on   = if (!is.na(t_sync))     as.integer(round(t_sync))     else NA_integer_,
          t_display_off  = if (!is.na(t_disp_off)) as.integer(round(t_disp_off)) else NA_integer_,
          t_trial_end    = as.integer(round(t_end))
        )
      }
      in_trial <- FALSE
      next
    }
  }

  # Assemble output tibbles
  fixations_tbl <- if (length(fix_rows) > 0L) {
    dplyr::bind_rows(lapply(fix_rows, dplyr::as_tibble))
  } else {
    dplyr::tibble(
      trial_nr   = integer(0),
      start_time = integer(0),
      end_time   = integer(0),
      duration   = integer(0),
      avg_x      = double(0),
      avg_y      = double(0)
    )
  }

  word_boundaries_tbl <- if (length(roi_rows) > 0L) {
    dplyr::bind_rows(lapply(roi_rows, function(r) {
      dplyr::as_tibble(r[, c("trial_nr", "word_id", "word",
                              "x_start", "x_end", "y_start", "y_end")])
    }))
  } else {
    dplyr::tibble(
      trial_nr = integer(0),
      word_id  = integer(0),
      word     = character(0),
      x_start  = double(0),
      x_end    = double(0),
      y_start  = double(0),
      y_end    = double(0)
    )
  }

  trial_db_tbl <- if (length(trial_rows) > 0L) {
    dplyr::bind_rows(lapply(trial_rows, dplyr::as_tibble))
  } else {
    dplyr::tibble(
      trial_nr      = integer(0),
      trial_id      = character(0),
      prefix        = character(0),
      cond          = integer(0),
      item          = integer(0),
      seq           = integer(0),
      t_trial_start = integer(0),
      t_display_on  = integer(0),
      t_display_off = integer(0),
      t_trial_end   = integer(0)
    )
  }

  list(
    fixations       = fixations_tbl,
    word_boundaries = word_boundaries_tbl,
    trial_db        = trial_db_tbl
  )
}

# ---------------------------------------------------------------------------
# Internal helpers
# ---------------------------------------------------------------------------

#' Build word-level ROIs from a character-level data frame
#'
#' Adjacent non-space characters are merged into a single word ROI.  The
#' trailing space after a word (if present) is included in that word's ROI,
#' matching the eyedry/analyzeeyedry convention.
#'
#' @param chars_df Data frame with columns: idx, char, x_left, y_top,
#'   x_right, y_bottom.  Must be sorted by idx.
#' @return Data frame with columns: word_id (integer), word (character),
#'   x_start, x_end, y_start, y_end.
#' @noRd
.build_word_rois_from_chars <- function(chars_df) {
  n <- nrow(chars_df)
  if (n == 0L) return(data.frame())

  # Identify the row-index of the first character of each word
  word_start_rows <- integer(0)
  for (ci in seq_len(n)) {
    if (ci == 1L) {
      word_start_rows <- c(word_start_rows, ci)
    } else if (chars_df$char[[ci - 1L]] == " " &&
               chars_df$char[[ci]]     != " ") {
      word_start_rows <- c(word_start_rows, ci)
    }
  }

  nw <- length(word_start_rows)
  if (nw == 0L) return(data.frame())

  word_end_rows <- c(word_start_rows[-1L] - 1L, n)

  rois <- vector("list", nw)
  for (w in seq_len(nw)) {
    rows    <- word_start_rows[[w]]:word_end_rows[[w]]
    x_s     <- chars_df$x_left[[word_start_rows[[w]]]]
    x_e     <- chars_df$x_right[[word_end_rows[[w]]]]
    y_s     <- min(chars_df$y_top[rows])
    y_e     <- max(chars_df$y_bottom[rows])
    word_str <- paste(chars_df$char[rows], collapse = "")
    rois[[w]] <- data.frame(
      word_id = as.integer(w),
      word    = word_str,
      x_start = x_s,
      x_end   = x_e,
      y_start = y_s,
      y_end   = y_e,
      stringsAsFactors = FALSE
    )
  }
  do.call(rbind, rois)
}
