#' @title Compute word-level eye-movement measures
#'
#' @description
#' Assigns fixations to words based on regions of interest (ROIs) and
#' computes standard reading-research eye-movement measures for each word in
#' each trial.
#'
#' ## Measures computed
#'
#' | Measure | Abbreviation | Definition |
#' |---------|:---:|--------|
#' | First Fixation Duration | FFD | Duration of the *first* fixation that lands on the word during the first pass (a single fixation that lands on the word and is not preceded by a fixation on a later word). |
#' | Gaze Duration | GD | Sum of all fixation durations during the first pass through the word (from first entry until the gaze exits the word for the first time). |
#' | Go-Past Time | GPT | Time from when the eye first enters the word until it first exits to the *right* of the word (includes regressive fixations to earlier words). Also called *regression-path duration*. |
#' | Total Viewing Time | TVT | Sum of all fixation durations on the word across the entire trial (all passes). |
#'
#' Fixations not falling within any ROI are assigned `word_id = NA`.
#'
#' @param fixations A data frame of fixations with (at minimum) columns
#'   `start_time`, `end_time`, `duration`, `avg_x`, and `avg_y`.  A `trial`
#'   column is required for multi-trial data.  An `eye` column, if present,
#'   is passed through to the output.
#' @param roi A data frame of word ROIs as returned by `read_roi()`.  Must
#'   contain columns `trial`, `word_id`, `x_start`, `x_end`, `y_start`,
#'   `y_end`.
#' @param trial_col Character scalar.  Name of the trial identifier column in
#'   both `fixations` and `roi`.  Defaults to `"trial_nr"`.
#' @param eye_col Character scalar or `NULL`.  Name of the eye column in
#'   `fixations`, if any.  Measures are computed separately per eye when
#'   provided.  Defaults to `"eye"`.
#' @param include_word_col Logical.  If `TRUE` and the `roi` data frame
#'   contains a `word` column, attach it to the output.  Defaults to `TRUE`.
#' @param trial_db A data frame (e.g. the `trial_db` element returned by
#'   [read_asc()]) with columns named by `trial_col`, `t_display_on`, and
#'   `t_display_off`.  When supplied, only fixations whose `start_time` is
#'   **strictly after** `t_display_on` **and** whose `end_time` is **strictly
#'   before** `t_display_off` are retained for each trial.  `NA` values in
#'   `t_display_on` or `t_display_off` disable the respective bound for that
#'   trial.  Defaults to `NULL` (no filtering).
#'
#' @return A [tibble][tibble::tibble] with one row per trial × word
#'   combination that received at least one fixation.  Columns:
#'   \describe{
#'     \item{`trial`}{Trial identifier.}
#'     \item{`sentence_nr`}{Sentence/item identifier (if available in `roi`).}
#'     \item{`word_id`}{Integer word index.}
#'     \item{`word`}{Word string (if available and `include_word_col = TRUE`).}
#'     \item{`ffd`}{First fixation duration (ms); `NA` if not applicable.}
#'     \item{`gd`}{Gaze duration (ms); `NA` if the word was not fixated in
#'       the first pass.}
#'     \item{`gpt`}{Go-past time (ms); `NA` if the word was never fixated.}
#'     \item{`tvt`}{Total viewing time (ms).}
#'     \item{`n_fixations`}{Total number of fixations on the word.}
#'   }
#'   Rows for words that received zero fixations are not included.  If
#'   `eye_col` is present, the `eye` column is also included and measures are
#'   computed per eye.
#'
#' @importFrom dplyr tibble mutate filter arrange bind_rows select left_join
#'   group_by summarise first last pull row_number lead
#' @importFrom rlang .data
#'
#' @export
#'
#' @examples
#' # Small self-contained example
#' roi <- dplyr::tibble(
#'   trial   = 1L,
#'   word_id = 1:3,
#'   word    = c("The", "quick", "fox"),
#'   x_start = c(100, 200, 320),
#'   x_end   = c(195, 315, 420),
#'   y_start = c(380, 380, 380),
#'   y_end   = c(420, 420, 420)
#' )
#' fixations <- dplyr::tibble(
#'   trial      = 1L,
#'   start_time = c(0L,  200L, 400L, 600L),
#'   end_time   = c(150L, 380L, 550L, 750L),
#'   duration   = c(150L, 180L, 150L, 150L),
#'   avg_x      = c(145,  260,  160,  370),
#'   avg_y      = c(400,  400,  400,  400)
#' )
#' measures <- compute_eye_measures(fixations, roi)
#' print(measures)
compute_eye_measures <- function(
    fixations,
    roi,
    trial_col       = "trial_nr",
    eye_col         = "eye",
    include_word_col = TRUE,
    trial_db        = NULL
) {
  stopifnot(is.data.frame(fixations), is.data.frame(roi))

  # Smart fallback for trial column if default is not found
  if (identical(trial_col, "trial_nr") && !("trial_nr" %in% names(fixations)) && "trial" %in% names(fixations)) {
    trial_col <- "trial"
  } else if (identical(trial_col, "trial") && !("trial" %in% names(fixations)) && "trial_nr" %in% names(fixations)) {
    trial_col <- "trial_nr"
  }

  required_fix <- c("start_time", "end_time", "duration", "avg_x", "avg_y")
  required_roi <- c(trial_col, "word_id", "x_start", "x_end", "y_start", "y_end")

  missing_fix <- setdiff(required_fix, names(fixations))
  missing_roi <- setdiff(required_roi, names(roi))
  if (length(missing_fix) > 0L) {
    stop("fixations is missing columns: ", paste(missing_fix, collapse = ", "))
  }
  if (length(missing_roi) > 0L) {
    stop("roi is missing columns: ", paste(missing_roi, collapse = ", "))
  }

  has_trial <- trial_col %in% names(fixations)
  has_eye   <- !is.null(eye_col) && eye_col %in% names(fixations)

  # Validate trial_db if provided
  if (!is.null(trial_db)) {
    stopifnot(is.data.frame(trial_db))
    required_tdb <- c(trial_col, "t_display_on", "t_display_off")
    missing_tdb  <- setdiff(required_tdb, names(trial_db))
    if (length(missing_tdb) > 0L) {
      stop("trial_db is missing columns: ", paste(missing_tdb, collapse = ", "))
    }
  }

  # Assign fixations to ROIs
  fix_assigned <- .assign_fixations_to_roi(fixations, roi, trial_col)

  group_cols <- c(trial_col)
  if (has_eye) group_cols <- c(group_cols, eye_col)

  # Compute measures per trial (× eye)
  all_keys <- if (has_eye) {
    unique(fix_assigned[, c(trial_col, eye_col), drop = FALSE])
  } else {
    unique(fix_assigned[, trial_col, drop = FALSE])
  }

  results <- vector("list", nrow(all_keys))
  for (i in seq_len(nrow(all_keys))) {
    mask <- rep(TRUE, nrow(fix_assigned))
    for (col in names(all_keys)) {
      mask <- mask & (fix_assigned[[col]] == all_keys[[col]][[i]])
    }
    trial_fix <- fix_assigned[mask, , drop = FALSE]
    trial_fix <- trial_fix[order(trial_fix$start_time), , drop = FALSE]

    # Filter to display-on / display-off window when trial_db is supplied
    if (!is.null(trial_db)) {
      tr_val  <- all_keys[[trial_col]][[i]]
      tdb_row <- trial_db[trial_db[[trial_col]] == tr_val, , drop = FALSE]
      if (nrow(tdb_row) >= 1L) {
        disp_on  <- tdb_row$t_display_on[[1L]]
        disp_off <- tdb_row$t_display_off[[1L]]
        if (!is.na(disp_on)) {
          trial_fix <- trial_fix[trial_fix$start_time > disp_on, , drop = FALSE]
        }
        if (!is.na(disp_off)) {
          trial_fix <- trial_fix[trial_fix$end_time < disp_off, , drop = FALSE]
        }
      }
    }

    measures  <- .compute_trial_measures(trial_fix)
    # Attach key columns
    for (col in names(all_keys)) {
      measures[[col]] <- all_keys[[col]][[i]]
    }
    results[[i]] <- measures
  }

  out <- dplyr::bind_rows(results)

  # Attach word text and sentence_nr if requested/available
  if ("word" %in% names(roi) || "sentence_nr" %in% names(roi)) {
    cols_to_join <- intersect(c(trial_col, "word_id", "word", "sentence_nr"), names(roi))
    word_lookup <- unique(roi[, cols_to_join, drop = FALSE])
    join_by <- c(trial_col, "word_id")
    out <- dplyr::left_join(out, word_lookup, by = join_by)
  }

  # Reorder output columns
  lead_cols    <- c(trial_col,
                    if ("sentence_nr" %in% names(out)) "sentence_nr",
                    if (has_eye) eye_col, "word_id",
                    if ("word" %in% names(out)) "word")
  measure_cols <- c("ffd", "gd", "gpt", "tvt", "n_fixations")
  col_order    <- c(lead_cols, measure_cols)
  col_order    <- col_order[col_order %in% names(out)]
  out[, col_order, drop = FALSE]
}

# ---------------------------------------------------------------------------
# Internal helpers
# ---------------------------------------------------------------------------

#' Assign each fixation to a word ROI
#' @noRd
.assign_fixations_to_roi <- function(fixations, roi, trial_col) {
  # For each fixation, find the word_id (if any) whose ROI contains avg_x, avg_y
  n_fix  <- nrow(fixations)
  word_ids <- integer(n_fix)
  word_ids[] <- NA_integer_

  for (i in seq_len(n_fix)) {
    fx <- fixations$avg_x[[i]]
    fy <- fixations$avg_y[[i]]
    tr <- if (trial_col %in% names(fixations)) fixations[[trial_col]][[i]] else NA

    # Subset ROI to this trial
    if (!is.na(tr)) {
      roi_trial <- roi[roi[[trial_col]] == tr, , drop = FALSE]
    } else {
      roi_trial <- roi
    }

    # Find matching ROI
    hit <- which(
      roi_trial$x_start <= fx & fx <= roi_trial$x_end &
        roi_trial$y_start <= fy & fy <= roi_trial$y_end
    )
    if (length(hit) >= 1L) {
      word_ids[[i]] <- roi_trial$word_id[[hit[[1L]]]]
    }
  }

  out <- fixations
  out$word_id <- word_ids
  out
}

#' Compute FFD, GD, GPT, TVT for all words in a single trial sequence
#' @noRd
.compute_trial_measures <- function(trial_fix) {
  # trial_fix is sorted by start_time
  # word_id may be NA for fixations outside all ROIs

  # All words that received at least one fixation
  word_ids <- sort(unique(stats::na.omit(trial_fix$word_id)))
  if (length(word_ids) == 0L) {
    return(dplyr::tibble(
      word_id     = integer(0),
      ffd         = integer(0),
      gd          = integer(0),
      gpt         = integer(0),
      tvt         = integer(0),
      n_fixations = integer(0)
    ))
  }

  n_fix <- nrow(trial_fix)

  results <- vector("list", length(word_ids))
  for (wi in seq_along(word_ids)) {
    w <- word_ids[[wi]]

    # Indices of all fixations on this word
    on_word <- which(!is.na(trial_fix$word_id) & trial_fix$word_id == w)

    if (length(on_word) == 0L) next

    # --- TVT ---
    tvt_val <- sum(trial_fix$duration[on_word])

    # --- First-pass detection ---
    # The first fixation index overall on this word
    first_fix_idx <- on_word[[1L]]

    # Was there a prior fixation on a *later* word (word_id > w)?
    prior_ids <- if (first_fix_idx > 1L) {
      stats::na.omit(trial_fix$word_id[seq_len(first_fix_idx - 1L)])
    } else {
      integer(0)
    }
    skip <- any(prior_ids > w)

    # FFD: duration of the first fixation if it was a first-pass fixation
    ffd_val <- if (!skip) {
      as.integer(trial_fix$duration[[first_fix_idx]])
    } else {
      NA_integer_
    }

    # GD: sum of first-pass fixations (continuous run of fixations on the word
    #     starting from first_fix_idx)
    if (!skip) {
      fp_end <- first_fix_idx
      while (fp_end < n_fix) {
        next_id <- trial_fix$word_id[[fp_end + 1L]]
        if (!is.na(next_id) && next_id == w) {
          fp_end <- fp_end + 1L
        } else {
          break
        }
      }
      gd_val <- as.integer(sum(trial_fix$duration[first_fix_idx:fp_end]))
    } else {
      gd_val <- NA_integer_
    }

    # GPT: from first fixation on word until first rightward exit
    # i.e., sum durations from first_fix_idx until a fixation on word_id > w
    if (!skip) {
      gpt_end <- first_fix_idx
      while (gpt_end < n_fix) {
        next_id <- trial_fix$word_id[[gpt_end + 1L]]
        if (!is.na(next_id) && next_id > w) {
          break
        }
        gpt_end <- gpt_end + 1L
      }
      gpt_val <- as.integer(sum(trial_fix$duration[first_fix_idx:gpt_end]))
    } else {
      gpt_val <- NA_integer_
    }

    results[[wi]] <- dplyr::tibble(
      word_id     = as.integer(w),
      ffd         = ffd_val,
      gd          = gd_val,
      gpt         = gpt_val,
      tvt         = as.integer(tvt_val),
      n_fixations = as.integer(length(on_word))
    )
  }

  dplyr::bind_rows(results)
}
