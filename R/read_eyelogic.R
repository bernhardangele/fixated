#' @title Read an EyeLogic CSV file
#'
#' @description
#' Parses an EyeLogic CSV file (exported by the EyeLogic eye tracker) and
#' returns a named list with data frames for samples, and (when present)
#' word boundary and trial information.
#'
#' EyeLogic files do not contain pre-detected events (fixations, saccades, blinks).
#' Fixation detection must be performed in a separate step using [detect_fixations()].
#'
#' The coordinate system origin follows the EyeLink convention: **(0, 0) is
#' the top-left corner** of the display, with x increasing rightward and y
#' increasing downward.
#'
#' Timestamps are returned in milliseconds, relative to the first `start_trial`
#' message in the file. Raw microsecond timestamps are preserved in a separate
#' `time_micro` column (in samples) and `_micro` suffixed columns (in trial_db).
#'
#' @param path Character scalar.  Full path to the `.csv` file.
#' @param eyes Character vector.  Which eyes to return.  One or both of
#'   `"L"` and `"R"`.  Defaults to `c("L", "R")` (both).
#' @param parse_vars Logical scalar.  When `TRUE` (default),
#'   OpenSesame variable messages of the form `MSG ... var KEY VALUE` with a
#'   single-word value are parsed and added as columns to `trial_db`.
#'   Set to `FALSE` to skip this step.
#' @param opensesame_csv_path Character scalar or `NULL`.  Path to an OpenSesame
#'   CSV file containing trial-level variables (e.g., sentence text, condition).
#'   When provided, all columns from this file are merged into `trial_db` by
#'   matching on `sentence_number` (in the CSV) with `sentence_nr` (in `trial_db`).
#'   Variable names are prefixed with `os_` to avoid conflicts.
#'   Defaults to `NULL` (no external variables loaded).
#'
#' @return A named list with elements:
#'   \describe{
#'     \item{`samples`}{A [tibble][tibble::tibble] with columns
#'       `time` (numeric, ms relative to first trial_start), `time_micro` (numeric,
#'       raw microseconds), `x` (double, pixels), `y` (double, pixels),
#'       `pupil` (double), `eye` (character, `"L"` or `"R"`).  When trial
#'       information is found `trial_nr` and `sentence_nr` columns are also
#'       included.}
#'     \item{`events`}{Always `NULL` for EyeLogic files.}
#'     \item{`word_boundaries`}{A [tibble][tibble::tibble] with columns
#'       `trial_nr`, `sentence_nr`, `word_id`, `word`, `x_start`, `x_end`,
#'       `y_start`, `y_end`, parsed from `TRIAL ... ITEM ... WORD ...`
#'       messages written by OpenSesame.  `NULL` when no such messages are
#'       found.}
#'     \item{`character_boundaries`}{Always `NULL` for EyeLogic files.}
#'     \item{`calibration`}{A [tibble][tibble::tibble] with columns
#'       parsed from calibration messages.  `NULL` when no such messages are
#'       found.}
#'     \item{`trial_db`}{A [tibble][tibble::tibble] with one row per trial
#'       and columns `trial_nr`, `sentence_nr`, `t_trial_start` (ms),
#'       `t_display_on` (ms), `t_display_off` (ms), `t_trial_end` (ms),
#'       `has_display_off`, plus `_micro` suffixed columns with raw timestamps.
#'       When `parse_vars = TRUE`, additional columns are appended for each
#'       OpenSesame variable whose value is a simple scalar (no spaces).
#'       When `opensesame_csv_path` is provided, all columns from that file
#'       are included with an `os_` prefix.
#'       `NULL` when no trial structure is detected.}
#'   }
#'
#' @importFrom dplyr tibble mutate filter bind_rows select rename arrange
#'   if_else group_by summarise as_tibble left_join
#' @importFrom stringr str_detect str_split str_trim str_extract str_remove
#'   str_starts str_match
#' @importFrom rlang .data
#' @importFrom readr read_delim cols col_character col_double col_integer
#'
#' @export
#'
#' @examples
#' eyelogic_file <- system.file("extdata", "example_eyelogic.csv", package = "fixated")
#' if (file.exists(eyelogic_file)) {
#'   result <- read_eyelogic(eyelogic_file)
#'   head(result$samples)
#'   head(result$word_boundaries)
#'   print(result$trial_db)
#' }
#'
#' # Load with external OpenSesame variables
#' result <- read_eyelogic(
#'   eyelogic_file,
#'   opensesame_csv_path = "path/to/subject_opensesame.csv"
#' )
read_eyelogic <- function(path,
                          eyes                 = c("L", "R"),
                          parse_vars           = TRUE,
                          opensesame_csv_path  = NULL) {
  stopifnot(is.character(path), length(path) == 1L, file.exists(path))
  eyes <- match.arg(eyes, c("L", "R"), several.ok = TRUE)

  # ---- 1. Read the CSV file --------------------------------------------------
  # EyeLogic files use semicolon as separator
  # First check if the file has the Sep=; header
  first_line <- readLines(path, n = 1L)
  if (length(first_line) == 0L) {
    stop("File is empty: ", path)
  }
  
  # Determine separator
  sep <- ";"
  
  # Read the file
  # Suppress warnings about column count mismatches (MSG lines have fewer columns than DAT lines)
  raw <- suppressWarnings(readr::read_delim(
    path,
    delim = sep,
    col_types = readr::cols(
      .default = readr::col_character()
    ),
    skip = if (grepl("^Sep=", first_line)) 1L else 0L,
    show_col_types = FALSE
  ))
  
  # ---- 2. Separate MSG and DAT lines -----------------------------------------
  if (!"TYPE" %in% names(raw)) {
    stop("File does not have a TYPE column. Is this a valid EyeLogic file?")
  }
  
  msg_lines <- raw[raw$TYPE == "MSG", ]
  dat_lines <- raw[raw$TYPE == "DAT", ]
  
  # ---- 3. Find the reference time (first start_trial message) ----------------
  # All timestamps will be relative to this point
  ref_time_micro <- .find_first_trial_start(msg_lines)
  if (is.na(ref_time_micro)) {
    warning("No start_trial messages found. Using first sample timestamp as reference.")
    ref_time_micro <- if (nrow(dat_lines) > 0) {
      as.numeric(dat_lines$timestampMicroSec[1])
    } else {
      0
    }
  }
  
  # ---- 4. Parse DAT lines into samples ----------------------------------------
  samples <- .parse_eyelogic_dat(dat_lines, eyes, ref_time_micro)
  
  # ---- 5. Parse MSG lines for metadata ----------------------------------------
  word_boundaries <- .parse_eyelogic_word_boundaries(msg_lines)
  calibration     <- .parse_eyelogic_calibration(msg_lines, ref_time_micro)
  
  # ---- 6. Parse trial structure -----------------------------------------------
  trial_db <- .parse_eyelogic_trial_db(msg_lines, parse_vars, ref_time_micro)
  
  # Assign trial information to samples
  if (!is.null(trial_db) && nrow(trial_db) > 0L) {
    samples <- .assign_trial_nr(samples, trial_db, time_col = "time")
    
    # Join sentence_nr based on trial_nr
    lookup <- trial_db[, c("trial_nr", "sentence_nr")]
    samples <- dplyr::left_join(samples, lookup, by = "trial_nr")
  }
  
  # ---- 6b. Load OpenSesame variables from external CSV -----------------------
  if (!is.null(opensesame_csv_path) && !is.null(trial_db)) {
    trial_db <- .merge_opensesame_csv(trial_db, opensesame_csv_path)
  }
  
  # ---- 7. Post-process word boundaries with trial_db metadata ----------------
  if (!is.null(word_boundaries) && !is.null(trial_db)) {
    word_boundaries <- .process_word_boundaries(word_boundaries, trial_db)
  }
  
  # ---- 8. Return the list ----------------------------------------------------
  list(
    samples              = samples,
    events               = NULL,
    word_boundaries      = word_boundaries,
    character_boundaries = NULL,
    calibration          = calibration,
    trial_db             = trial_db
  )
}

# ---------------------------------------------------------------------------
# Internal helpers for read_eyelogic()
# ---------------------------------------------------------------------------

#' Find the timestamp (in microseconds) of the first start_trial message
#'
#' @param msg_lines A tibble with MSG lines from the EyeLogic file
#' @return Numeric scalar with the microsecond timestamp, or NA if not found
#' @noRd
.find_first_trial_start <- function(msg_lines) {
  if (nrow(msg_lines) == 0L) return(NA_real_)
  
  msg_content <- msg_lines$index
  start_pattern <- "^\\s*start_trial\\s+(\\d+)"
  start_idx <- which(stringr::str_detect(msg_content, start_pattern))
  
  if (length(start_idx) == 0L) return(NA_real_)
  
  as.numeric(msg_lines$timestampMicroSec[start_idx[1L]])
}

#' Parse DAT lines from EyeLogic file into a samples tibble
#'
#' @param dat_lines A tibble with DAT lines from the EyeLogic file
#' @param eyes Character vector of eyes to include
#' @param ref_time_micro Numeric scalar. Reference time in microseconds.
#'   Timestamps in `time` column will be relative to this value (in ms).
#' @return A tibble with columns time, time_micro, x, y, pupil, eye
#' @noRd
.parse_eyelogic_dat <- function(dat_lines, eyes, ref_time_micro) {
  if (nrow(dat_lines) == 0L) {
    return(dplyr::tibble(
      time       = double(0),
      time_micro = double(0),
      x          = double(0),
      y          = double(0),
      pupil      = double(0),
      eye        = character(0)
    ))
  }
  
  # Extract columns - EyeLogic format has:
  # TYPE;timestampMicroSec;index;porFilteredX;porFilteredY;porLeftX;porLeftY;pupilRadiusLeft;porRightX;porRightY;pupilRadiusRight
  
  # Convert to numeric (they're read as character)
  timestamp_micro <- as.numeric(dat_lines$timestampMicroSec)
  
  # Convert to ms relative to reference time
  timestamp <- (timestamp_micro - ref_time_micro) / 1000
  
  # Filtered position (used for main gaze coordinates)
  por_filtered_x <- as.numeric(dat_lines$porFilteredX)
  por_filtered_y <- as.numeric(dat_lines$porFilteredY)
  
  # Left eye data
  por_left_x <- as.numeric(dat_lines$porLeftX)
  por_left_y <- as.numeric(dat_lines$porLeftY)
  pupil_left <- as.numeric(dat_lines$pupilRadiusLeft)
  
  # Right eye data
  por_right_x <- as.numeric(dat_lines$porRightX)
  por_right_y <- as.numeric(dat_lines$porRightY)
  pupil_right <- as.numeric(dat_lines$pupilRadiusRight)
  
  # Define missing value threshold
  missing_val <- -1.7976931348623157e+308
  
  # Create separate rows for left and right eyes
  samples_list <- list()
  
  # Left eye samples
  if ("L" %in% eyes) {
    left_valid <- !is.na(por_left_x) & por_left_x != missing_val & 
                  !is.na(por_left_y) & por_left_y != missing_val
    
    if (any(left_valid)) {
      left_samples <- dplyr::tibble(
        time       = timestamp[left_valid],
        time_micro = timestamp_micro[left_valid],
        x          = por_left_x[left_valid],
        y          = por_left_y[left_valid],
        pupil      = pupil_left[left_valid],
        eye        = "L"
      )
      samples_list <- c(samples_list, list(left_samples))
    }
  }
  
  # Right eye samples
  if ("R" %in% eyes) {
    right_valid <- !is.na(por_right_x) & por_right_x != missing_val & 
                   !is.na(por_right_y) & por_right_y != missing_val
    
    if (any(right_valid)) {
      right_samples <- dplyr::tibble(
        time       = timestamp[right_valid],
        time_micro = timestamp_micro[right_valid],
        x          = por_right_x[right_valid],
        y          = por_right_y[right_valid],
        pupil      = pupil_right[right_valid],
        eye        = "R"
      )
      samples_list <- c(samples_list, list(right_samples))
    }
  }
  
  if (length(samples_list) == 0L) {
    return(dplyr::tibble(
      time       = double(0),
      time_micro = double(0),
      x          = double(0),
      y          = double(0),
      pupil      = double(0),
      eye        = character(0)
    ))
  }
  
  # Combine and sort
  samples <- dplyr::bind_rows(samples_list)
  samples <- dplyr::arrange(samples, .data$time, .data$eye)
  
  samples
}

#' Parse word boundary messages from EyeLogic file
#'
#' Expects lines of the form:
#'   `MSG;timestamp;TRIAL <trial> ITEM <item> WORD <word_nr> <word> RIGHT_BOUNDARY <x>`
#'
#' @param msg_lines A tibble with MSG lines from the EyeLogic file
#' @return A tibble with columns trial_nr, sentence_nr, word_id, word, x_end, or NULL
#' @noRd
.parse_eyelogic_word_boundaries <- function(msg_lines) {
  if (nrow(msg_lines) == 0L) return(NULL)
  
  # Extract the message content (third column is "index" for MSG lines)
  # For MSG lines, the "index" column contains the message content
  msg_content <- msg_lines$index
  
  pattern <- "^\\s*TRIAL\\s+(\\d+)\\s+ITEM\\s+(\\d+)\\s+WORD\\s+(\\d+)\\s+(\\S+)\\s+RIGHT_BOUNDARY\\s+(\\d+)"
  wb_idx <- which(stringr::str_detect(msg_content, pattern))
  if (length(wb_idx) == 0L) return(NULL)
  
  m <- stringr::str_match(msg_content[wb_idx], pattern)
  dplyr::tibble(
    trial_nr    = as.integer(m[, 2L]),
    sentence_nr = as.integer(m[, 3L]),
    word_id     = as.integer(m[, 4L]),
    word        = m[, 5L],
    x_end       = as.integer(m[, 6L])
  )
}

#' Parse calibration messages from EyeLogic file
#'
#' @param msg_lines A tibble with MSG lines from the EyeLogic file
#' @param ref_time_micro Numeric scalar. Reference time in microseconds.
#' @return A tibble with calibration information or NULL
#' @noRd
.parse_eyelogic_calibration <- function(msg_lines, ref_time_micro) {
  if (nrow(msg_lines) == 0L) return(NULL)
  
  # Extract the message content (third column is "index" for MSG lines)
  msg_content <- msg_lines$index
  
  # Look for calibration accuracy messages
  pattern <- "^\\s*accuracy \\(in pixels\\) = X=([\\d.]+), Y=([\\d.]+)"
  cal_idx <- which(stringr::str_detect(msg_content, pattern))
  if (length(cal_idx) == 0L) return(NULL)
  
  m <- stringr::str_match(msg_content[cal_idx], pattern)
  
  # Get timestamp for calibration
  timestamp_micro <- as.numeric(msg_lines$timestampMicroSec[cal_idx[1L]])
  
  dplyr::tibble(
    eye        = "L",  # EyeLogic calibration is binocular combined
    quality    = "CALIBRATION",
    avg_error  = (as.numeric(m[, 2L]) + as.numeric(m[, 3L])) / 2,
    max_error  = pmax(as.numeric(m[, 2L]), as.numeric(m[, 3L])),
    offset_deg = NA_real_,
    x_offset   = as.numeric(m[, 2L]),
    y_offset   = as.numeric(m[, 3L])
  )
}

#' Parse trial structure from EyeLogic file
#'
#' Uses start_trial and stop_trial messages to define trial boundaries.
#'
#' @param msg_lines A tibble with MSG lines from the EyeLogic file
#' @param parse_vars Logical; parse OpenSesame var messages?
#' @param ref_time_micro Numeric scalar. Reference time in microseconds.
#' @return Tibble with one row per trial, or NULL if no trials found
#' @noRd
.parse_eyelogic_trial_db <- function(msg_lines, parse_vars = TRUE, ref_time_micro = 0) {
  if (nrow(msg_lines) == 0L) return(NULL)
  
  # Extract timestamps in microseconds
  timestamp_micro <- as.numeric(msg_lines$timestampMicroSec)
  
  # Convert to ms relative to reference time
  timestamp <- (timestamp_micro - ref_time_micro) / 1000
  
  msg_content <- msg_lines$index
  
  # Find start_trial messages
  start_pattern <- "^\\s*start_trial\\s+(\\d+)"
  start_idx <- which(stringr::str_detect(msg_content, start_pattern))
  
  if (length(start_idx) == 0L) return(NULL)
  
  # Extract trial numbers from start_trial messages
  m_start <- stringr::str_match(msg_content[start_idx], start_pattern)
  trial_nrs <- as.integer(m_start[, 2L])
  
  # Find stop_trial messages
  stop_pattern <- "^\\s*stop_trial"
  stop_idx <- which(stringr::str_detect(msg_content, stop_pattern))
  
  # Find DISPLAY ON and DISPLAY OFF messages
  display_on_pattern <- "^\\s*DISPLAY ON"
  display_off_pattern <- "^\\s*DISPLAY OFF"
  
  display_on_idx <- which(stringr::str_detect(msg_content, display_on_pattern))
  display_off_idx <- which(stringr::str_detect(msg_content, display_off_pattern))
  
  # Find TRIALID messages for sentence_nr
  trialid_pattern <- "^\\s*TRIALID\\s+(\\S+)"
  trialid_idx <- which(stringr::str_detect(msg_content, trialid_pattern))
  
  # Build trial_db
  n_trials <- length(start_idx)
  
  # For each trial, find timestamps (ms and micro)
  t_trial_start <- timestamp[start_idx]
  t_trial_start_micro <- timestamp_micro[start_idx]
  
  # Find DISPLAY ON and DISPLAY OFF for each trial
  t_display_on <- rep(NA_real_, n_trials)
  t_display_off <- rep(NA_real_, n_trials)
  t_display_on_micro <- rep(NA_real_, n_trials)
  t_display_off_micro <- rep(NA_real_, n_trials)
  
  # Find sentence_nr from TRIALID messages
  sentence_nrs <- rep(NA_integer_, n_trials)
  
  # Upper bound for each trial
  next_start <- c(start_idx[-1L], nrow(msg_lines) + 1L)
  
  for (i in seq_len(n_trials)) {
    # Find DISPLAY ON within this trial
    display_on_in_trial <- display_on_idx[display_on_idx >= start_idx[i] & 
                                          display_on_idx < next_start[i]]
    if (length(display_on_in_trial) > 0L) {
      t_display_on[i] <- timestamp[display_on_in_trial[1L]]
      t_display_on_micro[i] <- timestamp_micro[display_on_in_trial[1L]]
    }
    
    # Find DISPLAY OFF within this trial
    display_off_in_trial <- display_off_idx[display_off_idx >= start_idx[i] & 
                                            display_off_idx < next_start[i]]
    if (length(display_off_in_trial) > 0L) {
      t_display_off[i] <- timestamp[display_off_in_trial[length(display_off_in_trial)]]
      t_display_off_micro[i] <- timestamp_micro[display_off_in_trial[length(display_off_in_trial)]]
    }
    
    # Find TRIALID within this trial
    trialid_in_trial <- trialid_idx[trialid_idx >= start_idx[i] & 
                                    trialid_idx < next_start[i]]
    if (length(trialid_in_trial) > 0L) {
      m_trialid <- stringr::str_match(msg_content[trialid_in_trial[1L]], trialid_pattern)
      # Extract trial number from TRIALID (e.g., "I303C_high" -> 303)
      trial_id_str <- m_trialid[, 2L]
      # Try to extract numeric part
      num_match <- stringr::str_extract(trial_id_str, "\\d+")
      if (!is.na(num_match)) {
        sentence_nrs[i] <- as.integer(num_match)
      }
    }
  }
  
  # Find trial end (stop_trial timestamp for each trial)
  t_trial_end <- rep(NA_real_, n_trials)
  t_trial_end_micro <- rep(NA_real_, n_trials)
  for (i in seq_len(n_trials)) {
    stop_in_trial <- stop_idx[stop_idx >= start_idx[i] & 
                              stop_idx < next_start[i]]
    if (length(stop_in_trial) > 0L) {
      t_trial_end[i] <- timestamp[stop_in_trial[1L]]
      t_trial_end_micro[i] <- timestamp_micro[stop_in_trial[1L]]
    }
  }
  
  # Create trial_db with both ms and micro columns
  trial_db <- dplyr::tibble(
    trial_nr               = trial_nrs,
    sentence_nr            = sentence_nrs,
    t_trial_start          = t_trial_start,
    t_trial_start_micro    = t_trial_start_micro,
    t_display_on           = t_display_on,
    t_display_on_micro     = t_display_on_micro,
    t_display_off          = t_display_off,
    t_display_off_micro    = t_display_off_micro,
    t_trial_end            = t_trial_end,
    t_trial_end_micro      = t_trial_end_micro,
    has_display_off        = !is.na(t_display_off)
  )
  
  # Optionally add OpenSesame variables
  if (isTRUE(parse_vars)) {
    trial_db <- .add_opensesame_vars_eyelogic(msg_lines, start_idx, next_start, trial_db)
  }
  
  trial_db
}

#' Parse OpenSesame var messages from EyeLogic file
#'
#' @param msg_lines A tibble with MSG lines from the EyeLogic file
#' @param start_idx Integer vector of line indices where each trial starts
#' @param next_start Integer vector of exclusive upper line bounds per trial
#' @param trial_db Tibble to augment
#' @return trial_db with additional columns for OpenSesame variables
#' @noRd
.add_opensesame_vars_eyelogic <- function(msg_lines, start_idx, next_start, trial_db) {
  # Extract message content
  msg_content <- msg_lines$index
  
  # Match lines of the form: MSG;timestamp;var <key> <single_word_value>
  var_pattern <- "^\\s*var\\s+(\\w+)\\s+(\\S+)$"
  var_idx <- which(stringr::str_detect(msg_content, var_pattern))
  if (length(var_idx) == 0L) return(trial_db)
  
  m          <- stringr::str_match(msg_content[var_idx], var_pattern)
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
  prefixed_vars <- paste0("os_", unique_vars)
  
  # Pre-build a matrix of NA strings, then fill in values
  vars_wide <- matrix(NA_character_,
                       nrow     = n_trials,
                       ncol     = length(unique_vars),
                       dimnames = list(NULL, prefixed_vars))
  
  for (i in seq_along(unique_vars)) {
    vname    <- unique_vars[i]
    p_vname  <- prefixed_vars[i]
    is_v     <- var_names == vname
    v_trials <- ti[is_v]
    v_vals   <- var_values[is_v]
    # Keep last occurrence per trial (lines are in ascending order)
    dup              <- duplicated(v_trials, fromLast = TRUE)
    vars_wide[v_trials[!dup], p_vname] <- v_vals[!dup]
  }
  
  for (p_vname in prefixed_vars) {
    trial_db[[p_vname]] <- vars_wide[, p_vname]
  }
  trial_db
}

#' Process word boundaries with trial metadata
#'
#' @param word_boundaries Word boundaries tibble
#' @param trial_db Trial database tibble
#' @return Processed word boundaries with x_start, y_start, y_end
#' @noRd
.process_word_boundaries <- function(word_boundaries, trial_db) {
  wb_processed <- list()
  unique_trials <- unique(word_boundaries$trial_nr)
  
  # Track whether we've already warned about missing columns
  warned_start_x <- FALSE
  warned_height <- FALSE
  
  for (tr in unique_trials) {
    trial_wb <- word_boundaries[word_boundaries$trial_nr == tr, ]
    trial_info <- trial_db[trial_db$trial_nr == tr, ]
    
    if (nrow(trial_info) == 0L) {
      wb_processed[[as.character(tr)]] <- trial_wb
      next
    }
    
    # Get x_start from trial_db or default
    s_start_x <- if ("os_sentence_start_x" %in% names(trial_info)) {
      trial_info$os_sentence_start_x[[1L]]
    } else if ("sentence_start_x" %in% names(trial_info)) {
      trial_info$sentence_start_x[[1L]]
    } else {
      if (!warned_start_x) {
        warning("Column 'os_sentence_start_x' not found in trial_db. Defaulting to 125 for all trials.")
        warned_start_x <- TRUE
      }
      125
    }
    
    # Get height from trial_db or default
    s_height <- if ("os_height" %in% names(trial_info)) {
      as.numeric(trial_info$os_height[[1L]])
    } else if ("height" %in% names(trial_info)) {
      as.numeric(trial_info$height[[1L]])
    } else {
      if (!warned_height) {
        warning("Column 'os_height' not found in trial_db. Defaulting to 1080 for all trials.")
        warned_height <- TRUE
      }
      1080
    }
    
    # Compute x_start, y_start, y_end
    trial_wb <- trial_wb[order(trial_wb$word_id), ]
    n_w <- nrow(trial_wb)
    x_starts <- numeric(n_w)
    x_starts[1L] <- as.numeric(s_start_x)
    if (n_w > 1L) {
      x_starts[2:n_w] <- as.numeric(trial_wb$x_end[1:(n_w - 1L)])
    }
    
    trial_wb$x_start <- x_starts
    trial_wb$y_start <- 0
    trial_wb$y_end   <- s_height
    
    wb_processed[[as.character(tr)]] <- trial_wb
  }
  
  word_boundaries <- dplyr::bind_rows(wb_processed)
  
  # Final column ordering to match read_roi
  desired_cols <- c("trial_nr", "sentence_nr", "word_id", "word", "x_start", "x_end", "y_start", "y_end")
  existing_cols <- names(word_boundaries)
  col_order <- c(intersect(desired_cols, existing_cols), setdiff(existing_cols, desired_cols))
  word_boundaries <- word_boundaries[, col_order]
  
  word_boundaries
}

#' Merge OpenSesame variables from external CSV into trial_db
#'
#' Reads an OpenSesame CSV file and merges all columns into trial_db by
#' matching on sentence_number (CSV) with sentence_nr (trial_db).
#' Variable names are prefixed with 'os_' to avoid conflicts.
#'
#' @param trial_db Trial database tibble with sentence_nr column
#' @param opensesame_csv_path Path to OpenSesame CSV file
#' @return trial_db with additional os_* columns from the OpenSesame CSV
#' @noRd
.merge_opensesame_csv <- function(trial_db, opensesame_csv_path) {
  if (!file.exists(opensesame_csv_path)) {
    warning("OpenSesame CSV file not found: ", opensesame_csv_path)
    return(trial_db)
  }
  
  # Read the OpenSesame CSV
  # Suppress warnings about column type issues
  os_data <- tryCatch({
    suppressWarnings(readr::read_csv(opensesame_csv_path, show_col_types = FALSE))
  }, error = function(e) {
    warning("Failed to read OpenSesame CSV: ", e$message)
    return(NULL)
  })
  
  if (is.null(os_data) || nrow(os_data) == 0L) {
    return(trial_db)
  }
  
  # Check for sentence_number column in the CSV
  if (!"sentence_number" %in% names(os_data)) {
    # Try alternative column names
    if ("sentence_nr" %in% names(os_data)) {
      os_data$sentence_number <- os_data$sentence_nr
    } else {
      warning("OpenSesame CSV does not have a 'sentence_number' or 'sentence_nr' column. Cannot merge.")
      return(trial_db)
    }
  }
  
  # Prefix all columns (except the join key) with 'os_'
  # Keep sentence_number for joining, but also rename it to avoid confusion
  cols_to_prefix <- setdiff(names(os_data), "sentence_number")
  prefixed_data <- os_data
  
  for (col in cols_to_prefix) {
    new_name <- paste0("os_", col)
    # Avoid double-prefixing if already prefixed
    if (!startsWith(col, "os_")) {
      names(prefixed_data)[names(prefixed_data) == col] <- new_name
    }
  }
  
  # Select only columns that exist in trial_db (to avoid duplicate join keys)
  # and the prefixed columns
  prefixed_data <- prefixed_data[, c("sentence_number", 
                                      setdiff(names(prefixed_data), c("sentence_number", "sentence_nr"))), 
                                  drop = FALSE]
  
  # Join with trial_db by sentence_nr (trial_db) = sentence_number (CSV)
  trial_db <- dplyr::left_join(
    trial_db,
    prefixed_data,
    by = c("sentence_nr" = "sentence_number")
  )
  
  trial_db
}