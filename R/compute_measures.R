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
#' | First Fixation Duration | FFD | Duration of the *first* valid fixation that lands on the word during the first pass. |
#' | Gaze Duration | GD | Sum of all valid fixation durations during the first pass through the word (from first entry until the gaze exits to the right or regresses left). |
#' | Go-Past Time | GPT | Time from when the eye first enters the word until it first exits to the *right* (includes regressive fixations to earlier words). Also called *regression-path duration*. |
#' | Total Viewing Time | TVT | Sum of all fixation durations on the word across the entire trial (all passes). |
#' | Regression Out | oreg | `1` if a first-pass fixation on the word was immediately followed by a regression back past the left boundary; `0` otherwise; `NA` if no valid first-pass fixation. |
#' | Regression In | ireg | `1` if any fixation on the word arrived from a position to the right of the word's right boundary; `0` otherwise; `NA` if the word was never fixated. |
#' | First-pass fixation probability | pfix | `1` if the word received at least one valid first-pass fixation; `0` otherwise. |
#' | First-pass fixation count | nfix | Number of valid fixations on the word before the gaze first leaves the word during the first pass. |
#' | Second-pass time | sp | Sum of fixation durations on the word after the gaze first exits to the right (second and later passes). |
#'
#' ## Duration filtering (`shorttime` / `longtime`)
#'
#' When `shorttime` is set, fixations with `duration <= shorttime` are treated
#' as saccadic artefacts: they are **skipped** (not accumulated) for FFD, GD,
#' and GPT, and they do **not** end the first-pass scan when they fall outside
#' the word's boundaries.  This matches the *eyedry* / Robodoc convention.
#' TVT includes all fixations regardless of `shorttime`.
#'
#' When `longtime` is set, encountering a fixation with
#' `duration > longtime` on the word causes FFD, GD, GPT, and `sp` to be set
#' to `NA` (the trial data is discarded for that word).  For TVT, the scan
#' stops at a `longtime` fixation but any previously accumulated time is
#' retained; if `ttcutoff` is also set and the accumulated TVT exceeds it, TVT
#' is set to `NA`.
#'
#' ## Display-onset filtering (`trial_db` / `truncate_at_display_on`)
#'
#' When `trial_db` is supplied, fixations are filtered to the display window
#' before measures are computed.  The default behaviour (`truncate_at_display_on
#' = FALSE`) **excludes** fixations whose `start_time` is at or before
#' `t_display_on` (strict filter).  This means a fixation that *straddles* the
#' display onset (started before but ended after it) is dropped entirely.
#'
#' Setting `truncate_at_display_on = TRUE` mirrors the *eyedry* / UMass
#' EyeTrack convention: fixations whose `end_time` is after `t_display_on` are
#' retained, and those that straddle the onset have their `start_time` set to
#' `t_display_on` and their `duration` recomputed accordingly.  This typically
#' retains one additional fixation per trial.
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
#'   `t_display_off`.  When supplied, fixations are filtered to the display
#'   window for each trial (see `truncate_at_display_on` for details).  `NA`
#'   values in `t_display_on` or `t_display_off` disable the respective bound
#'   for that trial.  Defaults to `NULL` (no filtering).
#' @param shorttime Numeric scalar or `NULL`.  If non-`NULL`, fixations with
#'   `duration <= shorttime` (ms) are skipped for FFD, GD, and GPT
#'   computations and do not terminate the first-pass scan when they fall
#'   outside the word's boundaries.  They are still included in TVT.
#'   Defaults to `NULL` (no short-fixation filtering).
#' @param longtime Numeric scalar or `NULL`.  If non-`NULL`, encountering a
#'   fixation with `duration > longtime` (ms) on the word causes FFD, GD, GPT,
#'   and `sp` to be set to `NA` for that word.  TVT accumulation stops at the
#'   first `longtime` fixation but retains previously accumulated time.
#'   Defaults to `NULL` (no long-fixation filtering).
#' @param ttcutoff Numeric scalar or `NULL`.  If non-`NULL` and `longtime` is
#'   also set, TVT is set to `NA` when its cumulative value exceeds `ttcutoff`
#'   (ms).  Defaults to `NULL`.
#' @param truncate_at_display_on Logical.  When `TRUE` and `trial_db` is
#'   supplied, fixations that *straddle* `t_display_on` are retained but
#'   truncated (their `start_time` is set to `t_display_on` and `duration`
#'   recomputed), rather than being excluded entirely.  Defaults to `FALSE`.
#'
#' @return A [tibble][tibble::tibble] with one row per trial × word
#'   combination (all words from the ROI are included).  Words that received
#'   no fixations have `NA` for `ffd`, `gd`, `gpt`, `tvt`, `oreg`, `ireg`,
#'   and `sp`; `n_fixations`, `pfix`, and `nfix` are set to `0L`.  Columns:
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
#'     \item{`n_fixations`}{Total number of fixations on the word (`0L` if
#'       not fixated).}
#'     \item{`oreg`}{Regression out (0/1); `NA` if no valid first-pass
#'       fixation.}
#'     \item{`ireg`}{Regression in (0/1); `NA` if the word was never
#'       fixated.}
#'     \item{`pfix`}{First-pass fixation probability (0/1); `0L` if not
#'       fixated.}
#'     \item{`nfix`}{Number of first-pass fixations; `0L` if not fixated.}
#'     \item{`sp`}{Second-pass time (ms); `NA` if no second-pass fixations.}
#'   }
#'   If `eye_col` is present, the `eye` column is also included and measures are
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
    trial_col              = "trial_nr",
    eye_col                = "eye",
    include_word_col       = TRUE,
    trial_db               = NULL,
    shorttime              = NULL,
    longtime               = NULL,
    ttcutoff               = NULL,
    truncate_at_display_on = FALSE
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
          if (truncate_at_display_on) {
            # Keep fixations that end after display onset; truncate straddling ones
            trial_fix <- trial_fix[trial_fix$end_time > disp_on, , drop = FALSE]
            straddle  <- trial_fix$start_time <= disp_on
            if (any(straddle)) {
              trial_fix$start_time[straddle] <- disp_on
              trial_fix$duration[straddle]   <-
                trial_fix$end_time[straddle] - disp_on
            }
          } else {
            trial_fix <- trial_fix[trial_fix$start_time > disp_on, , drop = FALSE]
          }
        }
        if (!is.na(disp_off)) {
          trial_fix <- trial_fix[trial_fix$end_time < disp_off, , drop = FALSE]
        }
      }
    }

    # Get the ROI rows for this trial
    tr_val    <- all_keys[[trial_col]][[i]]
    trial_roi <- roi[roi[[trial_col]] == tr_val, , drop = FALSE]

    measures  <- .compute_trial_measures(trial_fix, trial_roi,
                                         shorttime, longtime, ttcutoff)
    # Attach key columns
    for (col in names(all_keys)) {
      measures[[col]] <- all_keys[[col]][[i]]
    }
    results[[i]] <- measures
  }

  out <- dplyr::bind_rows(results)

  # Expand to include ALL trial × word combinations from ROI (unfixated words
  # get NA for every measure)
  roi_trial_word_cols <- intersect(c(trial_col, "word_id"), names(roi))
  word_lookup <- unique(roi[, roi_trial_word_cols, drop = FALSE])

  if (has_eye && nrow(out) > 0L) {
    all_eyes <- unique(out[[eye_col]])
    eye_tbl  <- stats::setNames(data.frame(all_eyes), eye_col)
    word_lookup <- merge(word_lookup, eye_tbl, all = TRUE)
  }

  if (nrow(out) > 0L) {
    out <- dplyr::right_join(out, word_lookup, by = names(word_lookup))
  } else {
    out <- dplyr::as_tibble(word_lookup)
  }

  # Unfixated words: set count-based measures to 0 instead of NA
  if ("n_fixations" %in% names(out)) {
    out$n_fixations <- ifelse(is.na(out$n_fixations), 0L, out$n_fixations)
  }
  if ("pfix" %in% names(out)) {
    out$pfix <- ifelse(is.na(out$pfix), 0L, out$pfix)
  }
  if ("nfix" %in% names(out)) {
    out$nfix <- ifelse(is.na(out$nfix), 0L, out$nfix)
  }

  # Attach word text and sentence_nr if requested/available
  if ("word" %in% names(roi) || "sentence_nr" %in% names(roi)) {
    cols_to_join <- intersect(c(trial_col, "word_id", "word", "sentence_nr"), names(roi))
    word_lookup <- unique(roi[, cols_to_join, drop = FALSE])
    join_by <- c(trial_col, "word_id")
    out <- dplyr::left_join(out, word_lookup, by = join_by)
  }

  # Sort by trial, eye, word_id
  sort_cols <- c(trial_col, if (has_eye) eye_col, "word_id")
  ord       <- do.call(order, lapply(sort_cols, function(col) out[[col]]))
  out       <- out[ord, , drop = FALSE]

  # Reorder output columns
  lead_cols    <- c(trial_col,
                    if ("sentence_nr" %in% names(out)) "sentence_nr",
                    if (has_eye) eye_col, "word_id",
                    if ("word" %in% names(out)) "word")
  measure_cols <- c("ffd", "gd", "gpt", "tvt", "n_fixations",
                    "oreg", "ireg", "pfix", "nfix", "sp")
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

#' Compute FFD, GD, GPT, TVT, oreg, ireg, pfix, nfix, sp for all words
#' in a single trial sequence using the eyedry-compatible algorithm.
#' @noRd
.compute_trial_measures <- function(trial_fix, trial_roi,
                                    shorttime, longtime, ttcutoff) {
  # trial_fix is sorted by start_time; has word_id, duration, avg_x columns.
  # trial_roi has word_id, x_start, x_end columns.

  word_ids <- sort(unique(stats::na.omit(trial_fix$word_id)))
  if (length(word_ids) == 0L) {
    return(dplyr::tibble(
      word_id     = integer(0),
      ffd         = integer(0),
      gd          = integer(0),
      gpt         = integer(0),
      tvt         = integer(0),
      n_fixations = integer(0),
      oreg        = integer(0),
      ireg        = integer(0),
      pfix        = integer(0),
      nfix        = integer(0),
      sp          = integer(0)
    ))
  }

  n_fix <- nrow(trial_fix)
  dur   <- trial_fix$duration
  px    <- trial_fix$avg_x

  results <- vector("list", length(word_ids))
  for (wi in seq_along(word_ids)) {
    w <- word_ids[[wi]]

    # Get word x boundaries from ROI
    roi_row <- trial_roi[trial_roi$word_id == w, , drop = FALSE]
    if (nrow(roi_row) == 0L) next
    x_s <- roi_row$x_start[[1L]]
    x_e <- roi_row$x_end[[1L]]

    # Precompute per-fixation spatial and duration flags
    in_r   <- px >= x_s & px <= x_e   # fixation is in word w's region
    past_r <- px > x_e                # fixation is to the right of the region
    past_l <- px < x_s                # fixation is to the left of the region
    is_sht <- if (!is.null(shorttime)) dur <= shorttime else rep(FALSE, n_fix)
    is_lng <- if (!is.null(longtime))  dur > longtime   else rep(FALSE, n_fix)

    # Total number of fixations on word (raw count, no duration filter)
    n_fix_w <- sum(in_r)

    # ---- FFD ---------------------------------------------------------------
    # First valid (non-short, non-long) first-pass fixation on the word.
    # Short fixations past the right boundary do NOT end the first pass.
    ffd_V <- 0L; ffd_NV <- 0L
    for (i in seq_len(n_fix)) {
      if (past_r[i] && !is_sht[i]) break          # non-short rightward exit
      if (in_r[i]) {
        if (is_lng[i]) { ffd_V <- 0L; ffd_NV <- 0L; break }
        if (is_sht[i]) next
        ffd_V <- dur[i]; ffd_NV <- 1L; break      # record first valid fix
      }
    }
    ffd_val <- if (ffd_NV > 0L) as.integer(ffd_V) else NA_integer_

    # ---- GD ----------------------------------------------------------------
    # Sum of valid in-region fixations during first pass.
    # Short fixations outside the region are skipped (don't break first pass).
    gd_V <- 0L; gd_NV <- 0L
    for (i in seq_len(n_fix)) {
      if (past_r[i] && !is_sht[i]) break          # non-short rightward exit
      if (past_l[i] && gd_NV > 0L) break          # leftward regression after entering
      if (in_r[i]) {
        if (is_lng[i]) { gd_V <- 0L; gd_NV <- 0L; break }
        if (is_sht[i]) next
        gd_V <- gd_V + dur[i]; gd_NV <- gd_NV + 1L
      }
    }
    gd_val <- if (gd_NV > 0L) as.integer(gd_V) else NA_integer_

    # ---- GPT ---------------------------------------------------------------
    # Time from first entry into word until first non-short rightward exit.
    # Includes regressions (fixations to the left while got_fix is TRUE).
    # Unassigned fixations with avg_x <= x_e are included.
    gpt_V <- 0L; gpt_NV <- 0L; gpt_got <- FALSE
    for (i in seq_len(n_fix)) {
      if (in_r[i]) gpt_got <- TRUE
      if (gpt_got && past_r[i] && !is_sht[i]) break   # non-short rightward exit
      if (gpt_got && !past_r[i]) {                     # in or to the left
        if (is_lng[i]) { gpt_V <- 0L; gpt_NV <- 0L; break }
        if (is_sht[i]) next
        gpt_V <- gpt_V + dur[i]; gpt_NV <- gpt_NV + 1L
      }
    }
    gpt_val <- if (gpt_NV > 0L) as.integer(gpt_V) else NA_integer_

    # ---- TVT ---------------------------------------------------------------
    # Sum of ALL in-region fixation durations across the entire trial.
    # A longtime fixation stops the scan but keeps accumulated time.
    # ttcutoff discards TVT if cumulative value exceeds the cutoff.
    tvt_V <- 0L; tvt_NV <- 0L
    for (i in seq_len(n_fix)) {
      if (!in_r[i]) next
      if (is_lng[i]) break                           # stop scan, keep accumulated
      tvt_V <- tvt_V + dur[i]; tvt_NV <- tvt_NV + 1L
      if (!is.null(ttcutoff) && tvt_V > ttcutoff) {
        tvt_V <- 0L; tvt_NV <- 0L; break
      }
    }
    tvt_val <- if (tvt_NV > 0L) as.integer(tvt_V) else NA_integer_

    # ---- oreg (regression out) ---------------------------------------------
    # During first pass: was any valid in-region fixation immediately followed
    # by a fixation to the LEFT of the region that is also valid (non-short,
    # strictly < longtime)?
    oreg_V <- 0L; oreg_NV <- 0L
    for (i in seq_len(n_fix)) {
      if (past_r[i] && !is_sht[i]) break
      if (in_r[i] && !is_sht[i] && !is_lng[i]) {
        oreg_NV <- 1L
        if (i < n_fix) {
          np <- px[i + 1L]; nt <- dur[i + 1L]
          is_next_sht <- !is.null(shorttime) && nt <= shorttime
          is_next_lng <- !is.null(longtime)  && nt >= longtime  # strictly < longtime for oreg
          if (np < x_s && !is_next_sht && !is_next_lng) {
            oreg_V <- 1L
          }
        }
      }
    }
    oreg_val <- if (oreg_NV > 0L) as.integer(oreg_V) else NA_integer_

    # ---- ireg (regression in) ----------------------------------------------
    # Was any valid in-region fixation preceded by a fixation to the RIGHT
    # of the region that was also valid (non-short, strictly < longtime)?
    ireg_V <- 0L; ireg_NV <- 0L
    for (i in seq_len(n_fix)) {
      if (in_r[i]) {
        ireg_NV <- 1L
        if (!is_sht[i] && !is_lng[i] && i > 1L) {
          lp <- px[i - 1L]; lt <- dur[i - 1L]
          is_last_sht <- !is.null(shorttime) && lt <= shorttime
          is_last_lng <- !is.null(longtime)  && lt >= longtime  # strictly < longtime for ireg
          if (lp > x_e && !is_last_sht && !is_last_lng) {
            ireg_V <- 1L
            break
          }
        }
      }
    }
    ireg_val <- if (ireg_NV > 0L) as.integer(ireg_V) else NA_integer_

    # ---- pfix (first-pass fixation probability) ----------------------------
    # Was there at least one non-short first-pass fixation on the word?
    pfix_V <- 0L
    for (i in seq_len(n_fix)) {
      if (past_r[i] && !is_sht[i]) break
      if (in_r[i] && !is_sht[i]) { pfix_V <- 1L; break }
    }
    pfix_val <- as.integer(pfix_V)

    # ---- nfix (number of first-pass fixations) -----------------------------
    # Count of non-short fixations on the word before the first exit.
    nfix_V <- 0L; nfix_entered <- FALSE
    for (i in seq_len(n_fix)) {
      if (past_r[i] && !is_sht[i]) break
      if (in_r[i]) {
        if (!is_sht[i]) { nfix_V <- nfix_V + 1L; nfix_entered <- TRUE }
      } else if (nfix_entered && past_l[i]) {
        break                                        # leftward exit after entering
      }
    }
    nfix_val <- as.integer(nfix_V)

    # ---- sp (second-pass time) ---------------------------------------------
    # Sum of valid in-region fixation durations after the first non-short
    # rightward exit.
    sp_V <- 0L; sp_NV <- 0L; sp_past <- FALSE
    for (i in seq_len(n_fix)) {
      if (past_r[i] && !is_sht[i]) sp_past <- TRUE
      if (sp_past && in_r[i]) {
        if (is_lng[i]) { sp_V <- 0L; sp_NV <- 0L; break }
        if (is_sht[i]) next
        sp_NV <- sp_NV + 1L; sp_V <- sp_V + dur[i]
      }
    }
    sp_val <- if (sp_NV > 0L) as.integer(sp_V) else NA_integer_

    results[[wi]] <- dplyr::tibble(
      word_id     = as.integer(w),
      ffd         = ffd_val,
      gd          = gd_val,
      gpt         = gpt_val,
      tvt         = tvt_val,
      n_fixations = as.integer(n_fix_w),
      oreg        = oreg_val,
      ireg        = ireg_val,
      pfix        = pfix_val,
      nfix        = nfix_val,
      sp          = sp_val
    )
  }

  dplyr::bind_rows(results)
}
