#' @title Add landing position information to a fixations tibble
#'
#' @description
#' Takes a fixations tibble (with an `avg_x` column) and a word-boundaries
#' tibble (with `word_id`, `x_start`, `x_end`, and optionally a word-text
#' column named `word` or `word_text`) and returns the fixations tibble with
#' four new columns:
#'
#' * `word_id` – the word that the fixation landed on, matched by testing
#'   whether `avg_x` falls in `[x_start, x_end]`.  `NA` when no word boundary
#'   contains `avg_x`.  When multiple words overlap at `avg_x`, the word with
#'   the smallest boundary width is chosen; ties are broken by the smallest
#'   `x_start`.
#' * `fixation_type` – `"first"` for the first fixation on a given `word_id`
#'   within each trial (grouped by `trial_nr` if present; also by `eye` if
#'   present), `"refixation"` for every subsequent fixation on the same word.
#'   Temporal order is determined by `start_time` (preferred) or `start`.
#'   `NA` when `word_id` is `NA`.
#' * `word_proportion` – where within the word the fixation landed:
#'   `(avg_x - x_start) / (x_end - x_start)`.  Clamped to `[0, 1]` for
#'   slight numeric drift.  `NA` when the word width is <= 0 or `word_id` is
#'   `NA`.
#' * `fixated_char` – assuming a monospace font, the 1-based character
#'   position that `word_proportion` maps to.  For a word of `N` characters:
#'   `pmin(N, pmax(1, floor(word_proportion * N) + 1))`.  `NA` when
#'   `word_proportion` or `N` is `NA` / 0.
#'
#' @section Join keys:
#' The function determines how to match fixations to word boundaries:
#' \enumerate{
#'   \item If `trial_nr` is present in **both** tibbles, boundaries are
#'     matched per trial.
#'   \item Otherwise, if `sentence_nr` is present in both, boundaries are
#'     matched per sentence.
#'   \item Otherwise, a cross join is performed and a warning is emitted.
#'     In this case every fixation is matched against every word boundary,
#'     so the boundaries must correspond to the same trial/sentence as the
#'     fixations.
#' }
#'
#' @param fixations A [tibble][tibble::tibble] (or data frame) of fixations.
#'   Must contain at least `avg_x` (average fixation x position, numeric).
#'   Columns `trial_nr`, `sentence_nr`, `eye`, `start_time` / `start` are
#'   used when present (see Details).
#' @param word_boundaries A [tibble][tibble::tibble] (or data frame) of word
#'   boundaries.  Must contain `word_id`, `x_start`, and `x_end`.  May also
#'   contain a word-text column named `word` or `word_text` (used for
#'   `fixated_char`), and `trial_nr` / `sentence_nr` join keys.
#'
#' @return The input `fixations` tibble with four additional columns:
#'   `word_id` (integer), `fixation_type` (character), `word_proportion`
#'   (double, `[0, 1]`), and `fixated_char` (integer).  The row order matches
#'   the input.
#'
#' @export
#'
#' @examples
#' fixations <- dplyr::tibble(
#'   trial_nr   = c(1L, 1L, 1L),
#'   start_time = c(100L, 300L, 500L),
#'   avg_x      = c(150, 600, 150)   # word 1, word 2, word 1 again
#' )
#' word_boundaries <- dplyr::tibble(
#'   trial_nr = c(1L, 1L),
#'   word_id  = c(1L, 2L),
#'   word     = c("Hello", "world"),
#'   x_start  = c(100, 500),
#'   x_end    = c(300, 800)
#' )
#' result <- get_landing_info(fixations, word_boundaries)
#' result[, c("word_id", "fixation_type", "word_proportion", "fixated_char")]
get_landing_info <- function(fixations, word_boundaries) {
  stopifnot(is.data.frame(fixations))
  stopifnot(is.data.frame(word_boundaries))

  # ---- Validate required columns -------------------------------------------
  if (!"avg_x" %in% names(fixations)) {
    stop("'fixations' must contain column 'avg_x'")
  }
  required_wb <- c("word_id", "x_start", "x_end")
  missing_wb  <- setdiff(required_wb, names(word_boundaries))
  if (length(missing_wb) > 0L) {
    stop("'word_boundaries' is missing required columns: ",
         paste(missing_wb, collapse = ", "))
  }

  fix_names <- names(fixations)
  wb_names  <- names(word_boundaries)

  # ---- Determine join key(s) -----------------------------------------------
  if ("trial_nr" %in% fix_names && "trial_nr" %in% wb_names) {
    join_key <- "trial_nr"
  } else if ("sentence_nr" %in% fix_names && "sentence_nr" %in% wb_names) {
    join_key <- "sentence_nr"
  } else {
    warning(
      "No shared key column ('trial_nr' or 'sentence_nr') found in both ",
      "'fixations' and 'word_boundaries'. Using a cross join; ",
      "word boundaries must correspond to the same trial/sentence."
    )
    join_key <- character(0L)
  }

  # ---- Determine ordering column for fixation_type -------------------------
  order_col <- if ("start_time" %in% fix_names) "start_time" else
               if ("start"      %in% fix_names) "start"      else
               NULL

  # ---- Determine word-text column for fixated_char -------------------------
  word_col <- if ("word"      %in% wb_names) "word"      else
              if ("word_text" %in% wb_names) "word_text" else
              NULL

  # ---- 1. Match avg_x to word boundaries ------------------------------------
  n_fix             <- nrow(fixations)
  word_id_matched   <- rep(NA_integer_,   n_fix)
  x_start_matched   <- rep(NA_real_,      n_fix)
  x_end_matched     <- rep(NA_real_,      n_fix)
  word_text_matched <- rep(NA_character_, n_fix)

  # Build a minimal word_boundaries data frame for matching
  wb_keep <- unique(c(join_key, "word_id", "x_start", "x_end", word_col))
  wb_sub  <- word_boundaries[, wb_keep[wb_keep %in% wb_names], drop = FALSE]

  # Tag each fixation row with an index for tracking after joins
  fix_work          <- as.data.frame(fixations, stringsAsFactors = FALSE)
  fix_work$.fix_idx <- seq_len(n_fix)

  # Remove word_id from fix_work to avoid column collision in the join
  if ("word_id" %in% names(fix_work)) {
    fix_work <- fix_work[, setdiff(names(fix_work), "word_id"), drop = FALSE]
  }

  # Perform the join (key-based or cross)
  if (length(join_key) > 0L) {
    joined <- merge(fix_work, wb_sub, by = join_key, all.x = TRUE,
                    suffixes = c("", ".wb"))
  } else {
    fix_work$.cross_key <- 1L
    wb_sub$.cross_key   <- 1L
    joined <- merge(fix_work, wb_sub, by = ".cross_key",
                    all.x = TRUE, suffixes = c("", ".wb"))
    joined$.cross_key <- NULL
  }

  # Filter to rows where avg_x falls within the word boundary.
  # A small tolerance (1e-9) is used to handle slight numeric drift so that
  # fixations landing exactly at the boundary edge are still matched.
  .eps <- 1e-9
  in_bounds <- !is.na(joined$avg_x) &
               !is.na(joined$x_start) &
               !is.na(joined$x_end) &
               joined$avg_x >= joined$x_start - .eps &
               joined$avg_x <= joined$x_end   + .eps
  matches <- joined[in_bounds, , drop = FALSE]

  if (nrow(matches) > 0L) {
    # For fixations with multiple word matches, choose the best:
    # smallest boundary width, then smallest x_start
    matches$width <- matches$x_end - matches$x_start
    matches <- matches[order(matches$.fix_idx, matches$width,
                             matches$x_start), ]
    best <- matches[!duplicated(matches$.fix_idx), ]

    idx <- best$.fix_idx
    word_id_matched[idx] <- as.integer(best$word_id)
    x_start_matched[idx] <- as.numeric(best$x_start)
    x_end_matched[idx]   <- as.numeric(best$x_end)
    if (!is.null(word_col) && word_col %in% names(best)) {
      word_text_matched[idx] <- as.character(best[[word_col]])
    }
  }

  # ---- 2. Compute word_proportion -------------------------------------------
  width           <- x_end_matched - x_start_matched
  word_proportion <- (fixations$avg_x - x_start_matched) / width
  # Width <= 0 → NA (degenerate boundary)
  word_proportion[!is.na(width) & width <= 0] <- NA_real_
  # Clamp to [0, 1] for slight numeric drift
  word_proportion <- pmax(0, pmin(1, word_proportion))
  # Where word_id is NA, proportion is NA
  word_proportion[is.na(word_id_matched)] <- NA_real_

  # ---- 3. Compute fixated_char ----------------------------------------------
  N            <- nchar(word_text_matched)
  N[!is.na(N) & N == 0L] <- NA_integer_
  fixated_char <- as.integer(floor(word_proportion * N) + 1L)
  fixated_char <- pmin(N, pmax(1L, fixated_char))
  fixated_char[is.na(N) | is.na(word_proportion)] <- NA_integer_

  # ---- 4. Attach columns to fixations output --------------------------------
  result              <- fixations
  result$word_id      <- word_id_matched
  result$word_proportion <- word_proportion
  result$fixated_char <- fixated_char

  # ---- 5. Compute fixation_type in temporal order ---------------------------
  # Group by trial_nr (if present) and eye (if present)
  group_cols <- character(0L)
  if ("trial_nr" %in% names(result)) group_cols <- c(group_cols, "trial_nr")
  if ("eye"      %in% names(result)) group_cols <- c(group_cols, "eye")

  # Record original row positions so we can restore order afterwards
  result$.orig_order <- seq_len(nrow(result))

  # Build sort keys for temporal ordering
  sort_list <- lapply(group_cols, function(col) result[[col]])
  if (!is.null(order_col) && order_col %in% names(result)) {
    sort_list <- c(sort_list, list(result[[order_col]]))
  }
  sort_list <- c(sort_list, list(result$.orig_order))
  sorted_idx <- do.call(order, sort_list)
  result <- result[sorted_idx, ]

  # Compute fixation_type row by row in temporal order
  result$fixation_type <- NA_character_
  if (length(group_cols) > 0L) {
    group_id <- do.call(
      paste,
      c(lapply(group_cols, function(col) as.character(result[[col]])),
        sep = "\x00")
    )
  } else {
    group_id <- rep("__all__", nrow(result))
  }

  seen_words <- list()
  for (i in seq_len(nrow(result))) {
    wid <- result$word_id[i]
    if (!is.na(wid)) {
      key <- paste0(group_id[i], "\x00", wid)
      if (is.null(seen_words[[key]])) {
        result$fixation_type[i] <- "first"
        seen_words[[key]]       <- TRUE
      } else {
        result$fixation_type[i] <- "refixation"
      }
    }
  }

  # ---- 6. Restore original row order ----------------------------------------
  result <- result[order(result$.orig_order), ]
  result$.orig_order <- NULL

  result
}
