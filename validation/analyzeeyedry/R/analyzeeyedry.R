"_PACKAGE"


# ---------------------------------------------------------------------------
# Internal helper: parse a line of whitespace-separated numbers
# ---------------------------------------------------------------------------
.parse_nums <- function(line) {
  suppressWarnings(as.numeric(unlist(strsplit(trimws(line), "\\s+"))))
}


# ---------------------------------------------------------------------------
#' Read a CNT region-boundary file
#'
#' @param filepath Path to the .cnt file.
#' @return A data frame with columns: item, cond, nreg, end_last, and
#'   start_R1 through start_R\{n\} where n is the maximum number of regions
#'   found across all items in the file. Items with fewer regions have NA in
#'   the extra start columns.
#' @export
read_cnt <- function(filepath) {
  lines <- readLines(filepath, warn = FALSE)
  lines <- lines[nchar(trimws(lines)) > 0]

  records <- lapply(lines, function(line) {
    parts <- .parse_nums(line)
    parts <- parts[!is.na(parts)]
    if (length(parts) < 4) return(NULL)

    item  <- as.integer(parts[1])
    cond  <- as.integer(parts[2])
    nreg  <- as.integer(parts[3])
    # parts[4] = 0 (start of region 1, always 0)
    # parts[5 .. (3+nreg)] = starts of regions 2 .. nreg
    # parts[4+nreg]        = end of last region
    if (length(parts) < 4 + nreg) return(NULL)

    starts   <- as.integer(parts[4:(3 + nreg)])   # length nreg
    end_last <- as.integer(parts[4 + nreg])

    list(item = item, cond = cond, nreg = nreg,
         starts = starts, end_last = end_last)
  })
  records <- Filter(Negate(is.null), records)

  if (length(records) == 0) {
    return(data.frame(item = integer(), cond = integer(), nreg = integer(),
                      end_last = integer()))
  }

  max_nreg <- max(vapply(records, `[[`, 0L, "nreg"))

  rows <- lapply(records, function(r) {
    row <- data.frame(item = r$item, cond = r$cond, nreg = r$nreg,
                      end_last = r$end_last, stringsAsFactors = FALSE)
    for (i in seq_len(max_nreg)) {
      col <- paste0("start_R", i)
      row[[col]] <- if (i <= r$nreg) r$starts[i] else NA_integer_
    }
    row
  })

  do.call(rbind, rows)
}

# ---------------------------------------------------------------------------
#' Write a region-boundary data frame to a standard EYEDRY .cnt file
#'
#' @param df       A data frame as returned by \code{\link{read_cnt}} or
#'                 \code{\link{extract_regions_from_script}}, containing
#'                 columns \code{item}, \code{cond}, \code{nreg},
#'                 \code{end_last}, and \code{start_R1 ... start_Rn}.
#' @param filepath File path where the .cnt file will be written.
#' @export
write_cnt <- function(df, filepath) {
  if (nrow(df) == 0L) {
    writeLines(character(0), filepath)
    return(invisible(NULL))
  }

  # Order by item then cond
  df <- df[order(df$item, df$cond), ]

  # Identify start columns
  start_cols <- names(df)[grepl("^start_R\\d+$", names(df))]
  # Sort numerically by regional order
  start_cols <- start_cols[order(as.integer(sub("^start_R", "", start_cols)))]

  lines <- apply(df, 1L, function(row) {
    nreg <- as.integer(row["nreg"])
    # Values: item cond nreg starts end_last
    parts <- c(
      sprintf("%5d", as.integer(row["item"])),
      sprintf("%5d", as.integer(row["cond"])),
      sprintf("%5d", nreg)
    )

    # Add starts (only up to nreg)
    for (i in seq_len(nreg)) {
      parts <- c(parts, sprintf("%5d", as.integer(row[start_cols[i]])))
    }

    # Add end_last
    parts <- c(parts, sprintf("%5d", as.integer(row["end_last"])))

    # Join with spaces (using format that matches EYEDRY's alignment)
    paste(parts, collapse = " ")
  })

  writeLines(lines, filepath)
  invisible(df)
}


# ---------------------------------------------------------------------------
#' Read a DA1 fixation data file (produced by Robodoc.py)
#'
#' Each line of a DA1 file encodes one trial:
#'   seq cond item <header fields> numfix <header> x1 y1 s1 e1 x2 y2 s2 e2 ...
#'
#' @param filepath  Path to the .da1 file.
#' @param cpos      1-indexed field position of the condition number (default 2).
#' @param ipos      1-indexed field position of the item number (default 3).
#' @param npos      1-indexed field position of the number of fixations (default 7).
#' @param dpos      1-indexed field position where fixation data begin (default 9).
#' @param width     Character-grid width used to linearise (x, y) → position
#'                  (default 80; y is multiplied by this).
#' @return A list with elements \code{trials} (list of per-trial lists) and
#'   \code{subj} (NULL; set by the caller).
#' @export
read_da1 <- function(filepath, cpos = 2L, ipos = 3L,
                     npos = 7L, dpos = 9L, width = 80L) {
  lines <- readLines(filepath, warn = FALSE)
  lines <- lines[nchar(trimws(lines)) > 0]

  trials <- lapply(lines, function(line) {
    parts <- suppressWarnings(as.integer(unlist(strsplit(trimws(line), "\\s+"))))
    if (length(parts) < dpos) return(NULL)

    seq_n  <- parts[1L]
    cond   <- parts[cpos]
    item   <- parts[ipos]
    numfix <- parts[npos]

    if (is.na(numfix) || numfix <= 0L) {
      return(list(seq = seq_n, cond = cond, item = item,
                  fixations = data.frame(x = integer(), y = integer(),
                                         s = integer(), e = integer())))
    }

    # How many complete (x,y,s,e) groups are available?
    n_available <- (length(parts) - dpos + 1L) %/% 4L
    n_fix <- min(numfix, n_available)

    if (n_fix <= 0L) {
      return(list(seq = seq_n, cond = cond, item = item,
                  fixations = data.frame(x = integer(), y = integer(),
                                         s = integer(), e = integer())))
    }

    idx0 <- dpos  # 1-indexed start of fixation data
    fix_vals <- parts[idx0:(idx0 + n_fix * 4L - 1L)]
    fix_mat  <- matrix(fix_vals, ncol = 4L, byrow = TRUE)

    fixations <- data.frame(
      x = fix_mat[, 1L],
      y = fix_mat[, 2L],
      s = fix_mat[, 3L],
      e = fix_mat[, 4L]
    )

    list(seq = seq_n, cond = cond, item = item, fixations = fixations)
  })

  trials <- Filter(Negate(is.null), trials)
  list(trials = trials, subj = NULL)
}


# ---------------------------------------------------------------------------
#' Read an EyeLink ASC file and extract fixations in character coordinates
#'
#' Parses TRIALID, REGION CHAR, SYNCTIME, and EFIX records from an EyeLink
#' ASC file, converting pixel fixation positions to character indices using
#' the character bounding boxes provided by REGION CHAR messages.
#'
#' @param filepath      Path to the .asc file.
#' @param lowest_cond   Minimum condition number to include (default 1).
#' @param highest_cond  Maximum condition number to include (default 999).
#' @return A list with elements \code{trials} and \code{subj} (NULL).
#' @export
read_asc <- function(filepath, lowest_cond = 1L, highest_cond = 999L) {
  lines <- readLines(filepath, warn = FALSE)
  n <- length(lines)

  trials <- list()
  in_trial <- FALSE
  cond <- NA_integer_; item <- NA_integer_; seq_n <- 0L
  synctime <- NA_real_
  xpos_l <- numeric(0); xpos_r <- numeric(0)  # left/right pixel boundaries per char
  fixations <- list()

  for (line in lines) {
    # ---- Trial start (TRIALID) ----
    if (grepl("TRIALID\\s+E(\\d+)I(\\d+)", line)) {
      m <- regmatches(line, regexpr("TRIALID\\s+E(\\d+)I(\\d+)D?(\\d*)", line))
      if (length(m) > 0) {
        nums <- regmatches(m, gregexpr("\\d+", m))[[1]]
        if (length(nums) >= 2) {
          cond  <- as.integer(nums[1])
          item  <- as.integer(nums[2])
          seq_n <- seq_n + 1L
          in_trial <- TRUE
          synctime <- NA_real_
          xpos_l <- numeric(0); xpos_r <- numeric(0)
          fixations <- list()
        }
      }
      next
    }

    if (!in_trial) next

    # ---- REGION CHAR: character bounding boxes ----
    if (grepl("REGION CHAR", line)) {
      parts <- unlist(strsplit(trimws(line), "\\s+"))
      # Format: MSG timestamp REGION CHAR idx line char left_x top_y right_x bottom_y
      # When the character is a space (' '), whitespace splitting removes it,
      # leaving 10 parts instead of 11.  Use end-based indexing (as Robodoc.py
      # does with fields[-4] and fields[-2]) so space characters are handled
      # correctly: left_x is always parts[np-3] and right_x is parts[np-1].
      np <- length(parts)
      if (np >= 10L) {
        idx    <- as.integer(parts[5L]) + 1L  # 0-indexed → 1-indexed
        left_x <- as.numeric(parts[np - 3L])
        right_x <- as.numeric(parts[np - 1L])
        if (!is.na(idx) && idx >= 1L) {
          if (idx > length(xpos_l)) {
            length(xpos_l) <- idx
            length(xpos_r) <- idx
          }
          xpos_l[idx] <- left_x
          xpos_r[idx] <- right_x
        }
      }
      next
    }

    # ---- SYNCTIME: relative timing origin ----
    if (grepl("SYNCTIME", line)) {
      parts <- unlist(strsplit(trimws(line), "\\s+"))
      # MSG timestamp SYNCTIME
      if (length(parts) >= 2L) {
        synctime <- as.numeric(parts[2L])
      }
      next
    }

    # ---- EFIX: end-of-fixation record ----
    if (grepl("^EFIX\\s+R", line)) {
      # Only process fixations after SYNCTIME (display onset).
      # Pre-display fixations belong to the previous trial's display.
      if (is.na(synctime)) next

      parts <- unlist(strsplit(trimws(line), "\\s+"))
      # EFIX R start_time end_time duration mean_x mean_y pupil
      if (length(parts) >= 7L) {
        t_start  <- as.numeric(parts[3L])
        t_end    <- as.numeric(parts[4L])
        mean_x   <- as.numeric(parts[6L])
        mean_y   <- as.numeric(parts[7L])

        # Convert pixel x to character index (0-indexed).
        # Matches Robodoc.py: fixations outside the character region
        # (pixel x < left boundary of first char, or > right boundary of last
        # char) are stored as x = -1 so that compute_trial_measures excludes
        # them from any region's analysis.
        char_x <- NA_integer_
        if (length(xpos_r) > 0L && !is.na(mean_x)) {
          xl_first <- xpos_l[!is.na(xpos_l)][1L]   # left boundary of first char
          xr_last  <- tail(xpos_r[!is.na(xpos_r)], 1L)  # right boundary of last char
          if (!is.na(xl_first) && mean_x < xl_first) {
            char_x <- -1L   # to the left of the first character
          } else if (!is.na(xr_last) && mean_x > xr_last) {
            char_x <- -1L   # to the right of the last character
          } else {
            for (xi in seq_along(xpos_r)) {
              if (!is.na(xpos_r[xi]) && mean_x <= xpos_r[xi]) {
                char_x <- xi - 1L  # back to 0-indexed
                break
              }
            }
            # If still NA (e.g. all xpos_r values are NA), use last index
            if (is.na(char_x) && length(xpos_r) > 0L) {
              char_x <- length(xpos_r) - 1L
            }
          }
        }

        # Compute times relative to SYNCTIME (display onset).
        s_rel <- as.integer(round(t_start - synctime))
        e_rel <- as.integer(round(t_end   - synctime))

        # Skip fixations that end before the display onset.
        if (e_rel <= 0L) next

        # Fixations that started before the display onset are truncated to
        # start at 0 (matching the behaviour of Robodoc.py and Tim10drop.pl).
        if (s_rel < 0L) s_rel <- 0L

        fixations[[length(fixations) + 1L]] <- list(
          x = if (is.na(char_x)) 0L else as.integer(char_x),
          y = 0L,  # single-line: y always 0
          s = s_rel,
          e = e_rel
        )
      }
      next
    }

    # ---- TRIAL_RESULT: end of trial ----
    if (grepl("TRIAL_RESULT", line)) {
      if (in_trial && !is.na(cond) && !is.na(item) &&
          cond >= lowest_cond && cond <= highest_cond &&
          length(fixations) > 0L) {
        fix_df <- data.frame(
          x = vapply(fixations, `[[`, 0L, "x"),
          y = vapply(fixations, `[[`, 0L, "y"),
          s = vapply(fixations, `[[`, 0L, "s"),
          e = vapply(fixations, `[[`, 0L, "e")
        )
        trials[[length(trials) + 1L]] <- list(
          seq = seq_n, cond = cond, item = item,
          fixations = fix_df
        )
      }
      in_trial <- FALSE
      next
    }
  }

  list(trials = trials, subj = NULL)
}


# ---------------------------------------------------------------------------
#' Compute all eyedry measures for one trial
#'
#' Implements the exact algorithms from eyedry.c (Clifton, 1990) for:
#' first fixation (ff), first pass / gaze duration (fp), total time (tt),
#' regression out (oreg), regression in (ireg), probability of first-pass
#' fixation (pfix), number of first-pass fixations (nfix), second pass (sp),
#' and go-past time (gp).
#'
#' @param fix_df        Data frame with columns x, y, s (start time), e (end time).
#' @param region_starts Integer vector of region start positions (character units,
#'                      0-indexed). Length equals the number of regions.
#' @param region_end_last End position of the last region (character units).
#' @param shorttime     Fixations with duration <= shorttime are "short" and
#'                      ignored (default 80 ms).
#' @param longtime      Fixations with duration > longtime cause the containing
#'                      region on that trial to be discarded (default 800 ms).
#' @param fpcutoff      If cumulative first-pass time exceeds this, discard
#'                      region (default 2000 ms).
#' @param ttcutoff      If cumulative total time exceeds this, discard region
#'                      (default 4000 ms).
#' @param max_y         Maximum y coordinate; fixations with y >= max_y are
#'                      discarded (default 1000).
#' @param width         Character-grid width: position = x + y * width
#'                      (default 80).
#' @return Named list of numeric vectors, each of length \code{length(region_starts)}.
#'   Elements: ff, fp, tt, oreg, ireg, pfix, nfix, sp, gp. NA indicates no
#'   valid data for that region on this trial.
#' @export
compute_trial_measures <- function(fix_df, region_starts, region_end_last,
                                   shorttime = 80L, longtime = 800L,
                                   fpcutoff = 2000L, ttcutoff = 4000L,
                                   max_y = 1000L, width = 80L) {
  nreg        <- length(region_starts)
  region_ends <- c(region_starts[-1L], region_end_last)

  n_raw   <- nrow(fix_df)
  pos_raw <- fix_df$x + fix_df$y * width
  dur_raw <- fix_df$e - fix_df$s

  # ------------------------------------------------------------------
  # Apply start_record filter (skip leading fixations with pos < -1)
  # and y-range filter.  Mirrors eyedry.c sortsubject() inner loop.
  # ------------------------------------------------------------------
  start_record <- FALSE
  valid <- logical(n_raw)
  for (i in seq_len(n_raw)) {
    if (!start_record && pos_raw[i] >= -1L) start_record <- TRUE
    if (start_record && fix_df$y[i] >= 0L && fix_df$y[i] < max_y) {
      valid[i] <- TRUE
    }
  }

  pos_v <- pos_raw[valid]
  dur_v <- dur_raw[valid]
  y_v   <- fix_df$y[valid]
  n_v   <- sum(valid)

  ff_v   <- rep(NA_real_, nreg)
  fp_v   <- rep(NA_real_, nreg)
  tt_v   <- rep(NA_real_, nreg)
  oreg_v <- rep(NA_real_, nreg)
  ireg_v <- rep(NA_real_, nreg)
  pfix_v <- rep(NA_real_, nreg)
  nfix_v <- rep(NA_real_, nreg)
  sp_v   <- rep(NA_real_, nreg)
  gp_v   <- rep(NA_real_, nreg)

  for (r in seq_len(nreg)) {
    s_r <- region_starts[r]
    e_r <- region_ends[r]

    # ---- First Fixation (analysis 1, poke_anal = 1) ------------------
    # eyedry.c ff(): record duration of the first valid fixation in region
    # during first pass.
    {
      V <- 0L; NV <- 0L
      for (i in seq_len(n_v)) {
        p <- pos_v[i]; t <- dur_v[i]
        # Past region on the right → end first pass
        if (p >= e_r && t > shorttime) break
        # Regression back past start (NV==9999 marks "got first, still scanning")
        # Not needed for poke_anal=1 but included for completeness
        if (p < s_r && NV == 9999L) break
        if (p >= s_r && p < e_r) {
          if (t > longtime) { V <- 0L; NV <- 0L; break }
          if (t <= shorttime) next
          # First valid fixation (poke_anal = 1): record and stop
          if (NV == 0L) { V <- t; NV <- 1L; break }
        }
      }
      if (NV > 0L) ff_v[r] <- V
    }

    # ---- First Pass / Gaze Duration (analysis 2) ---------------------
    # eyedry.c fp(): sum durations of all first-pass fixations in region.
    {
      V <- 0L; NV <- 0L
      for (i in seq_len(n_v)) {
        p <- pos_v[i]; t <- dur_v[i]
        if (p >= e_r && t > shorttime) break        # left region
        if (p < s_r && NV != 0L) break              # regression out
        if (p >= s_r && p < e_r) {
          if (t > longtime) { V <- 0L; NV <- 0L; break }
          if (t <= shorttime) next
          V <- V + t; NV <- NV + 1L
          if (V > fpcutoff) { V <- 0L; NV <- 0L; break }
        }
      }
      if (NV > 0L) fp_v[r] <- V
    }

    # ---- Total Time (analysis 3) -------------------------------------
    # eyedry.c tt(): sum of ALL fixation durations in region across the
    # whole trial.  When a longtime fixation is encountered eyedry.c sets
    # fixation = endsen (ending the scan) WITHOUT zeroing V/NV, so any
    # previously accumulated time is preserved.
    {
      V <- 0L; NV <- 0L
      for (i in seq_len(n_v)) {
        p <- pos_v[i]; t <- dur_v[i]
        if (p >= s_r && p < e_r) {
          if (t > longtime) break               # end scan; keep accumulated V/NV
          if (t <= shorttime) next
          V <- V + t; NV <- NV + 1L
          if (V > ttcutoff) { V <- 0L; NV <- 0L; break }
        }
      }
      if (NV > 0L) tt_v[r] <- V
    }

    # ---- Regression Out (analysis 4) ---------------------------------
    # eyedry.c oreg(): did any first-pass fixation in the region lead to
    # a regression back past the start of the region?
    # Output: 0 (no regression) or 1 (regression occurred).
    # NV = 1 only when at least one valid fixation found in region.
    {
      V <- 0L; NV <- 0L
      for (i in seq_len(n_v)) {
        p <- pos_v[i]; t <- dur_v[i]
        if (p >= e_r && t > shorttime) break
        if (p >= s_r && p < e_r) {
          if (t > shorttime && t <= longtime) {
            NV <- 1L
            # Peek at the NEXT fixation (requires i < n_v)
            if (i < n_v) {
              np <- pos_v[i + 1L]; nt <- dur_v[i + 1L]
              # nexttime uses strictly < longtime (as in eyedry.c oreg)
              if (np < s_r && np > 0L &&
                  nt > shorttime && nt < longtime) {
                V <- 1L
              }
            }
          }
        }
      }
      if (NV > 0L) oreg_v[r] <- V
    }

    # ---- Regression In (analysis 5) ----------------------------------
    # eyedry.c ireg(): did any fixation in the region arrive from a
    # position to the RIGHT of the region end?
    # NV = 1 for ANY fixation in region (including short/long).
    {
      V <- 0L; NV <- 0L
      for (i in seq_len(n_v)) {
        p <- pos_v[i]; t <- dur_v[i]
        if (p >= s_r && p < e_r) {
          NV <- 1L
          if (t > shorttime && t <= longtime && i > 1L) {
            lp <- pos_v[i - 1L]
            lt <- dur_v[i - 1L]
            ly <- y_v[i - 1L]
            # lasttime uses strictly < longtime (as in eyedry.c ireg)
            if (lp >= e_r && lt > shorttime && lt < longtime &&
                ly >= 0L && ly < max_y) {
              V <- 1L
              break
            }
          }
        }
      }
      if (NV > 0L) ireg_v[r] <- V
    }

    # ---- Probability of first-pass fixation (analysis 6) -------------
    # eyedry.c p_init_fix(), poke_anal = 1:
    # NV is always 1.  V = 1 if any first-pass fixation > shorttime,
    # else 0.
    {
      V <- 0L
      for (i in seq_len(n_v)) {
        p <- pos_v[i]; t <- dur_v[i]
        if (p >= e_r && t > shorttime) break   # past region: end first pass
        if (p >= s_r && p < e_r) {
          if (t > shorttime) { V <- 1L; break }
        }
      }
      pfix_v[r] <- V
    }

    # ---- Number of first-pass fixations (analysis 7, poke_anal = 1) --
    # eyedry.c no_fixes(): count fixations > shorttime before the eye
    # leaves the region (rightward OR leftward after entering).
    # NV is always 1; count > longtime fixations too (eyedry.c counts
    # all fixations > shorttime regardless of longtime).
    {
      V <- 0L
      for (i in seq_len(n_v)) {
        p <- pos_v[i]; t <- dur_v[i]
        if (p >= e_r && t > shorttime) break   # went right past region
        if (p >= s_r && p < e_r) {
          if (t > shorttime) V <- V + 1L
        } else if (V > 0L && p < s_r) {
          break  # went left of region after entering
        }
      }
      nfix_v[r] <- V
    }

    # ---- Second Pass Time (analysis 8, poke_anal = 1) ----------------
    # eyedry.c sec_pass(): time spent in region after the eye first goes
    # past the right boundary.
    {
      V <- 0L; NV <- 0L
      past_r <- FALSE
      for (i in seq_len(n_v)) {
        p <- pos_v[i]; t <- dur_v[i]
        if (p >= e_r && t > shorttime) past_r <- TRUE
        if (p >= s_r && p < e_r && past_r) {
          if (t > longtime) { V <- 0L; NV <- 0L; break }
          if (t <= shorttime) next
          NV <- NV + 1L; V <- V + t
        }
      }
      if (NV > 0L) sp_v[r] <- V
    }

    # ---- Go-Past Time (analysis 13, poke_anal = 1) -------------------
    # eyedry.c go_past(): total time from first entering region to first
    # going past it (including time spent regressing before going past).
    {
      V <- 0L; NV <- 0L
      got_fix <- FALSE
      for (i in seq_len(n_v)) {
        p <- pos_v[i]; t <- dur_v[i]
        if (p >= s_r && p < e_r) got_fix <- TRUE
        if (p >= e_r && t > shorttime) break         # went past: stop
        if (got_fix && p < e_r) {
          if (t > longtime) { V <- 0L; NV <- 0L; break }
          if (t <= shorttime) next
          V <- V + t; NV <- NV + 1L
        }
      }
      if (NV > 0L) gp_v[r] <- V
    }
  }

  list(ff   = ff_v,
       fp   = fp_v,
       tt   = tt_v,
       oreg = oreg_v,
       ireg = ireg_v,
       pfix = pfix_v,
       nfix = nfix_v,
       sp   = sp_v,
       gp   = gp_v)
}


# ---------------------------------------------------------------------------
#' Analyze eye movement data using eyedry algorithms
#'
#' Reads one or more DA1 or ASC files together with a CNT region-boundary
#' file, computes all standard reading-time measures for every trial, and
#' returns the results as a wide-format tibble that is compatible with
#' eyedry's IXS output format.
#'
#' @param asc_files    Character vector of paths to EyeLink ASC files, or
#'                     \code{NULL} (default).
#' @param da1_files    Character vector of paths to DA1 files, or \code{NULL}
#'                     (default).
#' @param cnt_file     Path to the CNT region-boundary file (required).
#' @param shorttime    Fixation duration threshold (ms) below which fixations
#'                     are treated as "short" and ignored (default 80).
#' @param longtime     Fixation duration threshold (ms) above which the region
#'                     on that trial is discarded (default 800).
#' @param fpcutoff     Cumulative first-pass time cutoff in ms (default 2000).
#' @param ttcutoff     Cumulative total-time cutoff in ms (default 4000).
#' @param width        Character-grid width for linearising (x, y) positions
#'                     (default 80).
#' @param max_y        Maximum y coordinate; fixations with y >= max_y are
#'                     discarded (default 1000).
#' @param lowest_cond  Minimum condition number to include (default 1).
#' @param highest_cond Maximum condition number to include (default 999).
#' @param nregmax      Number of region columns to include in the output
#'                     (default 7).
#' @return A \code{\link[tibble]{tibble}} with one row per subject × item ×
#'   condition combination.  Columns:
#'   \describe{
#'     \item{subj}{Subject number (integer; file order).}
#'     \item{item}{Item number.}
#'     \item{cond}{Condition number.}
#'     \item{seq}{Presentation sequence number.}
#'     \item{ff_R1 ... ff_R\{nregmax\}}{First fixation duration (ms).}
#'     \item{fp_R1 ... fp_R\{nregmax\}}{First pass / gaze duration (ms).
#'       Corresponds to GD.IXS in eyedry output.}
#'     \item{tt_R1 ... tt_R\{nregmax\}}{Total reading time (ms).
#'       Corresponds to TVT.IXS in eyedry output.}
#'     \item{oreg_R1 ... oreg_R\{nregmax\}}{Regression out (0 = no, 1 = yes).}
#'     \item{ireg_R1 ... ireg_R\{nregmax\}}{Regression in (0 = no, 1 = yes).}
#'     \item{pfix_R1 ... pfix_R\{nregmax\}}{Probability of first-pass fixation
#'       (0 or 1).}
#'     \item{nfix_R1 ... nfix_R\{nregmax\}}{Number of first-pass fixations.}
#'     \item{sp_R1 ... sp_R\{nregmax\}}{Second pass time (ms).}
#'     \item{gp_R1 ... gp_R\{nregmax\}}{Go-past time (ms).}
#'   }
#'   \code{NA} is used when there was no valid fixation in a region
#'   (\code{NV = 0} in eyedry) or when the region does not exist for
#'   the item / condition.
#' @examples
#' \dontrun{
#' result <- analyze_eyedry(
#'   da1_files = "example/comas/C1L1.da1",
#'   cnt_file  = "example/comas/frases_c.cnt",
#'   shorttime = 80,
#'   longtime  = 800
#' )
#' # First pass time for item 1, condition 1, region 1:
#' subset(result, item == 1 & cond == 1)$fp_R1  # should be 1250
#' }
#' @export
# ===========================================================================
# Step-by-step workflow functions
# ===========================================================================


# ---------------------------------------------------------------------------
#' Extract region boundary information from a delimited script file
#'
#' Reads an EyeTrack script file in which `^` characters (or another
#' delimiter) mark the start of each new analysis region within the
#' \code{inline} sentence text.  The function computes the 0-indexed
#' character start positions for every region and writes them out in the same
#' format as \code{\link{read_cnt}}.
#'
#' The inline format expected is
#' \preformatted{inline = |<text><delim><text><delim>...<text>}
#' where \code{|} marks the beginning of the displayed sentence and
#' \code{<delim>} (default \code{^}) separates region boundaries.
#' Multi-display-line sentences use a literal \code{\\n} sequence to separate
#' display lines; each additional line is offset by \code{multiline_offset}
#' character positions (default 160).
#'
#' An additional trailing (spillover) region is appended after the last
#' character of the sentence text with a gap of \code{spillover_gap}
#' characters (default 1) and a fixed width of \code{spillover_size}
#' characters (default 20).  These defaults reproduce exactly the region
#' boundaries in the \code{frases_c.cnt} example file.
#'
#' @param filepath        Path to the delimited script file.
#' @param delimiter       Single character used as the region delimiter
#'                        (default \code{"^"}).
#' @param lowest_cond     Minimum condition number to include (default 1).
#' @param highest_cond    Maximum condition number to include (default 999).
#' @param spillover_gap   Number of blank character positions between the end
#'                        of the last inline region and the start of the
#'                        trailing spillover region (default 1).
#' @param spillover_size  Width in characters of the trailing spillover region
#'                        (default 20).
#' @param multiline_offset Character position offset for each additional
#'                        display line (default 160).
#' @return A data frame with the same column structure as \code{\link{read_cnt}}:
#'   \code{item}, \code{cond}, \code{nreg}, \code{end_last}, and
#'   \code{start_R1} through \code{start_R\{n\}}.
#' @examples
#' \dontrun{
#' regions <- extract_regions_from_script(
#'   "example/comas/frases_comas_delimited.script",
#'   lowest_cond = 1, highest_cond = 2
#' )
#' }
#' @export
extract_regions_from_script <- function(filepath,
                                        delimiter       = "^",
                                        lowest_cond     = 1L,
                                        highest_cond    = 999L,
                                        spillover_gap   = 1L,
                                        spillover_size  = 20L,
                                        multiline_offset = 160L) {
  lines <- readLines(filepath, warn = FALSE)

  records <- list()
  in_trial  <- FALSE
  cond      <- NA_integer_
  item      <- NA_integer_

  for (line in lines) {
    # ---- Trial start ----
    if (grepl("^trial\\s+", line)) {
      tid <- sub("^trial\\s+(\\S+).*", "\\1", line)
      # Match EyI and EDI formats: E<cond>I<item>D<n> or E<cond>I<item>
      m <- regmatches(tid,
                      regexpr("^[A-Za-z]+(\\d+)[A-Za-z](\\d+)", tid))
      if (length(m) > 0L) {
        nums <- regmatches(m, gregexpr("\\d+", m))[[1L]]
        if (length(nums) >= 2L) {
          cond_n <- as.integer(nums[1L])
          item_n <- as.integer(nums[2L])
          if (!is.na(cond_n) && !is.na(item_n) &&
              cond_n >= lowest_cond && cond_n <= highest_cond) {
            cond <- cond_n
            item <- item_n
            in_trial <- TRUE
          } else {
            in_trial <- FALSE
          }
        } else {
          in_trial <- FALSE
        }
      } else {
        in_trial <- FALSE
      }
      next
    }

    if (!in_trial) next

    # ---- inline line ----
    if (grepl("^\\s*inline\\s*=", line) && grepl("\\|", line)) {
      # Extract text after '|'
      after_pipe <- sub(".*\\|", "", line)

      # Split on literal \n for multi-line sentences
      display_lines <- strsplit(after_pipe, "\\\\n", fixed = TRUE)[[1L]]

      region_counter <- list()
      for (n_line in seq_along(display_lines)) {
        dl <- display_lines[[n_line]]
        # Remove trailing newline / whitespace from the last display line
        if (n_line == length(display_lines)) {
          dl <- sub("\\s+$", "", dl)
        }
        offset <- as.integer(n_line - 1L) * multiline_offset

        parts <- strsplit(dl, delimiter, fixed = TRUE)[[1L]]
        # Accumulate region starts for this display line
        cumlen <- 0L
        for (k in seq_along(parts)) {
          region_counter[[length(region_counter) + 1L]] <-
            list(start = cumlen + offset, len = nchar(parts[[k]]))
          cumlen <- cumlen + nchar(parts[[k]])
        }
      }

      # Compute total text length (end of last real region)
      text_end <- 0L
      for (rc in region_counter) {
        text_end <- max(text_end, rc$start + rc$len)
      }

      # Build region starts: start of each region is the cumulative position
      # up to (not including) that region's text.
      region_starts <- vapply(region_counter, `[[`, 0L, "start")

      # Append trailing spillover region
      spill_start    <- text_end + spillover_gap
      spill_end_last <- spill_start + spillover_size
      region_starts  <- c(region_starts, spill_start)
      end_last       <- spill_end_last

      nreg <- length(region_starts)

      records[[length(records) + 1L]] <- list(
        item         = item,
        cond         = cond,
        nreg         = nreg,
        starts       = region_starts,
        end_last     = end_last
      )
      in_trial <- FALSE
      next
    }

    # ---- end of trial ----
    if (grepl("^end\\s+", line)) {
      in_trial <- FALSE
      next
    }
  }

  if (length(records) == 0L) {
    return(data.frame(item = integer(), cond = integer(), nreg = integer(),
                      end_last = integer()))
  }

  max_nreg <- max(vapply(records, `[[`, 0L, "nreg"))

  rows <- lapply(records, function(r) {
    row <- data.frame(item     = r$item,
                      cond     = r$cond,
                      nreg     = r$nreg,
                      end_last = r$end_last,
                      stringsAsFactors = FALSE)
    for (i in seq_len(max_nreg)) {
      col <- paste0("start_R", i)
      row[[col]] <- if (i <= r$nreg) r$starts[i] else NA_integer_
    }
    row
  })

  do.call(rbind, rows)
}


# ---------------------------------------------------------------------------
#' Extract word-based region boundary information from a script file
#'
#' Similar to \code{\link{extract_regions_from_script}}, but instead of
#' searching for an explicit delimiter (like \code{^}), it splits each
#' \code{inline} sentence into words by whitespace.  Each word (including any
#' punctuation attached to it and its following space) is treated as a separate
#' region.
#'
#' @param filepath        Path to the script file.
#' @param lowest_cond     Minimum condition number to include (default 1).
#' @param highest_cond    Maximum condition number to include (default 999).
#' @param spillover_gap   Number of blank character positions between the end
#'                        of the last word and the start of the trailing
#'                        spillover region (default 1).
#' @param spillover_size  Width in characters of the trailing spillover region
#'                        (default 20).
#' @param multiline_offset Character position offset for each additional
#'                        display line (default 160).
#' @return A data frame with the same column structure as \code{\link{read_cnt}}.
#' @export
extract_word_regions_from_script <- function(filepath,
                                             lowest_cond      = 1L,
                                             highest_cond     = 999L,
                                             spillover_gap    = 1L,
                                             spillover_size   = 20L,
                                             multiline_offset = 160L) {
  lines <- readLines(filepath, warn = FALSE)

  records <- list()
  in_trial  <- FALSE
  cond      <- NA_integer_
  item      <- NA_integer_

  for (line in lines) {
    if (grepl("^trial\\s+", line)) {
      tid <- sub("^trial\\s+(\\S+).*", "\\1", line)
      if (grepl("^[A-Za-z]+(\\d+)[A-Za-z](\\d+)", tid)) {
        m <- regmatches(tid, regexpr("^[A-Za-z]+(\\d+)[A-Za-z](\\d+)", tid))
        nums <- regmatches(m, gregexpr("\\d+", m))[[1L]]
        c_n <- as.integer(nums[1L]); i_n <- as.integer(nums[2L])
        if (!is.na(c_n) && !is.na(i_n) && c_n >= lowest_cond && c_n <= highest_cond) {
          cond <- c_n; item <- i_n; in_trial <- TRUE
        } else in_trial <- FALSE
      } else in_trial <- FALSE
      next
    }
    if (!in_trial) next

    if (grepl("^\\s*inline\\s*=", line) && grepl("\\|", line)) {
      after_pipe <- sub(".*\\|", "", line)
      display_lines <- strsplit(after_pipe, "\\\\n", fixed = TRUE)[[1L]]

      region_starts <- integer(0)
      text_end <- 0L

      for (n_line in seq_along(display_lines)) {
        dl <- display_lines[[n_line]]
        if (n_line == length(display_lines)) dl <- sub("\\s+$", "", dl)
        offset <- as.integer(n_line - 1L) * multiline_offset

        m_info <- gregexpr("[^\\s]+", dl, perl = TRUE)
        starts <- as.integer(m_info[[1L]])
        if (length(starts) > 0 && starts[1L] != -1L) {
          if (n_line == 1L) starts[1L] <- 1L
          cur_starts <- (starts - 1L) + offset
          region_starts <- c(region_starts, cur_starts)

          lens <- attr(m_info[[1L]], "match.length")
          line_end <- max(starts + lens - 1L) + offset
          text_end <- max(text_end, line_end)
        }
      }

      if (length(region_starts) > 0) {
        spill_start    <- text_end + spillover_gap
        spill_end_last <- spill_start + spillover_size
        region_starts  <- c(region_starts, spill_start)

        records[[length(records) + 1L]] <- list(
          item = item, cond = cond, nreg = length(region_starts),
          starts = region_starts, end_last = spill_end_last
        )
      }
      in_trial <- FALSE
      next
    }
    if (grepl("^end\\s+", line)) { in_trial <- FALSE; next }
  }

  if (length(records) == 0L) {
    return(data.frame(item = integer(), cond = integer(), nreg = integer(),
                      end_last = integer()))
  }

  max_nreg <- max(vapply(records, `[[`, 0L, "nreg"))
  rows <- lapply(records, function(r) {
    df_row <- data.frame(item = r$item, cond = r$cond, nreg = r$nreg,
                         end_last = r$end_last, stringsAsFactors = FALSE)
    for (i in seq_len(max_nreg)) {
      col <- paste0("start_R", i)
      df_row[[col]] <- if (i <= r$nreg) r$starts[i] else NA_integer_
    }
    df_row
  })
  do.call(rbind, rows)
}


# ---------------------------------------------------------------------------
#' Read an EyeLink ASC file and return fixations as a long-format tibble
#'
#' This function is the long-format equivalent of \code{\link{read_asc}}.
#' Instead of returning a list of per-trial fixation data frames, it returns
#' a single flat \code{\link[tibble]{tibble}} where each row is one fixation.
#' The character-position encoding (pixel x → 0-indexed character index) is
#' identical to the one used by \code{\link{read_asc}} and to the encoding
#' produced by Robodoc.py, so the resulting \code{x} values are directly
#' comparable to the fixation positions stored in DA1 files.
#'
#' @param filepath      Path to the EyeLink ASC file.
#' @param subj          Subject identifier to attach to every row (default 1).
#' @param lowest_cond   Minimum condition number to include (default 1).
#' @param highest_cond  Maximum condition number to include (default 999).
#' @return A \code{\link[tibble]{tibble}} with one row per fixation and
#'   columns:
#'   \describe{
#'     \item{subj}{Subject identifier (passed in as \code{subj}).}
#'     \item{seq}{Trial sequence number (incremented for every TRIALID seen).}
#'     \item{cond}{Condition number extracted from the TRIALID message.}
#'     \item{item}{Item number extracted from the TRIALID message.}
#'     \item{fix_num}{1-indexed fixation number within the trial.}
#'     \item{x}{0-indexed character position (pixel x mapped to character
#'       grid via REGION CHAR messages; \code{-1} if outside the text area).}
#'     \item{y}{Line number (always 0 for single-line displays).}
#'     \item{s}{Fixation start time in ms relative to display onset
#'       (SYNCTIME); fixations that began before display onset are truncated
#'       to 0.}
#'     \item{e}{Fixation end time in ms relative to display onset.}
#'   }
#' @seealso \code{\link{read_asc}} for the original list-based reader,
#'   \code{\link{assign_regions_to_fixations}} for the next step in the
#'   step-by-step workflow.
#' @examples
#' \dontrun{
#' fix <- asc_to_fixations("example/comas/C1L1.asc", subj = 1L)
#' }
#' @export
asc_to_fixations <- function(filepath,
                              subj          = 1L,
                              lowest_cond   = 1L,
                              highest_cond  = 999L) {
  # Re-use the parsing logic from read_asc, then flatten to a tibble.
  raw <- read_asc(filepath,
                  lowest_cond  = lowest_cond,
                  highest_cond = highest_cond)
  raw$subj <- as.integer(subj)

  rows <- list()
  for (trial in raw$trials) {
    fix_df <- trial$fixations
    if (nrow(fix_df) == 0L) next
    n_fix <- nrow(fix_df)
    for (fi in seq_len(n_fix)) {
      rows[[length(rows) + 1L]] <- list(
        subj    = as.integer(subj),
        seq     = as.integer(trial$seq),
        cond    = as.integer(trial$cond),
        item    = as.integer(trial$item),
        fix_num = as.integer(fi),
        x       = as.integer(fix_df$x[fi]),
        y       = as.integer(fix_df$y[fi]),
        s       = as.integer(fix_df$s[fi]),
        e       = as.integer(fix_df$e[fi])
      )
    }
  }

  if (length(rows) == 0L) {
    return(tibble::tibble(
      subj    = integer(),
      seq     = integer(),
      cond    = integer(),
      item    = integer(),
      fix_num = integer(),
      x       = integer(),
      y       = integer(),
      s       = integer(),
      e       = integer()
    ))
  }

  tibble::as_tibble(do.call(rbind, lapply(rows, as.data.frame,
                                          stringsAsFactors = FALSE)))
}


# ---------------------------------------------------------------------------
#' Assign regions to fixations
#'
#' Takes a long-format fixation tibble (as returned by
#' \code{\link{asc_to_fixations}}) together with region boundary information
#' (as returned by \code{\link{extract_regions_from_script}} or
#' \code{\link{read_cnt}}) and adds a \code{region} column containing the
#' 1-indexed region number for each fixation.
#'
#' Region assignment follows the same logic as the eyedry algorithms: a
#' fixation at linear position \code{p = x + y * width} falls in region
#' \code{r} if \code{start_R{r} <= p < start_R{r+1}} (or \code{< end_last}
#' for the last region).  Fixations outside all defined regions (including
#' those with \code{x == -1} or positions beyond \code{end_last}) receive
#' \code{NA} for \code{region}.
#'
#' @param fixations  A tibble with at least columns \code{item}, \code{cond},
#'   \code{x}, \code{y} (as produced by \code{\link{asc_to_fixations}} or
#'   converted from a DA1 file).
#' @param regions    A data frame with region boundary information (as produced
#'   by \code{\link{extract_regions_from_script}} or \code{\link{read_cnt}}).
#'   Must contain columns \code{item}, \code{cond}, \code{nreg},
#'   \code{end_last}, and \code{start_R1 ... start_R\{n\}}.
#' @param width      Character-grid width used to linearise (x, y) →
#'   position: \code{p = x + y * width} (default 80).
#' @return The \code{fixations} tibble with two additional columns:
#'   \describe{
#'     \item{pos}{Linear character position (\code{x + y * width}).}
#'     \item{region}{1-indexed region number, or \code{NA} if the fixation
#'       falls outside all defined regions.}
#'   }
#' @seealso \code{\link{asc_to_fixations}}, \code{\link{compute_fixation_measures}}
#' @examples
#' \dontrun{
#' regions <- extract_regions_from_script("frases_comas_delimited.script")
#' fix     <- asc_to_fixations("C1L1.asc", subj = 1L)
#' fix_r   <- assign_regions_to_fixations(fix, regions)
#' }
#' @export
assign_regions_to_fixations <- function(fixations, regions, width = 80L) {
  n_fix <- nrow(fixations)

  pos_col    <- integer(n_fix)
  region_col <- integer(n_fix)

  for (i in seq_len(n_fix)) {
    xi    <- fixations$x[i]
    yi    <- fixations$y[i]
    item  <- fixations$item[i]
    cond  <- fixations$cond[i]

    pos_col[i] <- xi + yi * width

    reg_row <- regions[!is.na(regions$item) & regions$item == item &
                       !is.na(regions$cond) & regions$cond == cond, ]
    if (nrow(reg_row) == 0L) {
      region_col[i] <- NA_integer_
      next
    }

    nreg     <- reg_row$nreg[1L]
    end_last <- reg_row$end_last[1L]
    p        <- pos_col[i]

    r_assigned <- NA_integer_
    for (r in seq_len(nreg)) {
      s_r <- reg_row[[paste0("start_R", r)]][1L]
      e_r <- if (r < nreg) reg_row[[paste0("start_R", r + 1L)]][1L] else end_last
      if (!is.na(s_r) && p >= s_r && p < e_r) {
        r_assigned <- r
        break
      }
    }
    region_col[i] <- r_assigned
  }

  fixations$pos    <- pos_col
  fixations$region <- region_col
  fixations
}


# ---------------------------------------------------------------------------
#' Compute fixation time measures from long-format fixation data
#'
#' Implements the same eyedry algorithms as \code{\link{compute_trial_measures}}
#' but operates on the long-format fixation tibble produced by
#' \code{\link{assign_regions_to_fixations}}.  For each trial (unique
#' combination of \code{subj}, \code{cond}, \code{item}) all standard
#' reading-time measures are computed and returned in a wide-format tibble
#' that is identical in structure to the output of \code{\link{analyze_eyedry}}.
#'
#' @param fixations  A tibble with columns \code{subj}, \code{seq},
#'   \code{cond}, \code{item}, \code{x}, \code{y}, \code{s}, \code{e}
#'   as produced by \code{\link{assign_regions_to_fixations}} (the
#'   \code{pos} and \code{region} columns are ignored; positions are
#'   recomputed internally using \code{width} and \code{max_y}).
#' @param regions    A data frame with region boundary information (as
#'   produced by \code{\link{extract_regions_from_script}} or
#'   \code{\link{read_cnt}}).
#' @param shorttime  Fixation duration threshold (ms): fixations
#'   \eqn{\le} \code{shorttime} are skipped (default 80).
#' @param longtime   Fixation duration threshold (ms): fixations
#'   \eqn{>} \code{longtime} end the scan for fp/ff, or end the scan for
#'   tt while keeping accumulated time (default 800).
#' @param fpcutoff   Cumulative first-pass time cutoff in ms (default 2000).
#' @param ttcutoff   Cumulative total-time cutoff in ms (default 4000).
#' @param width      Character-grid width for linearising (x, y): position =
#'   x + y * width (default 80).
#' @param max_y      Maximum y coordinate; fixations with y ≥ max_y are
#'   discarded (default 1000).
#' @param nregmax    Number of region columns in the output (default 7).
#' @return A \code{\link[tibble]{tibble}} with one row per subject × item ×
#'   condition combination, with exactly the same columns as
#'   \code{\link{analyze_eyedry}}: \code{subj}, \code{item}, \code{cond},
#'   \code{seq}, and per-region columns for \code{ff}, \code{fp}, \code{tt},
#'   \code{oreg}, \code{ireg}, \code{pfix}, \code{nfix}, \code{sp},
#'   \code{gp}.
#' @seealso \code{\link{assign_regions_to_fixations}},
#'   \code{\link{compute_trial_measures}}, \code{\link{analyze_eyedry}}
#' @examples
#' \dontrun{
#' regions <- extract_regions_from_script("frases_comas_delimited.script")
#' fix     <- asc_to_fixations("C1L1.asc", subj = 1L)
#' fix_r   <- assign_regions_to_fixations(fix, regions)
#' result  <- compute_fixation_measures(fix_r, regions,
#'                                      shorttime = 80, longtime = 4000)
#' }
#' @export
compute_fixation_measures <- function(fixations,
                                      regions,
                                      shorttime = 80L,
                                      longtime  = 800L,
                                      fpcutoff  = 2000L,
                                      ttcutoff  = 4000L,
                                      width     = 80L,
                                      max_y     = 1000L,
                                      nregmax   = 7L) {
  measures <- c("ff", "fp", "tt", "oreg", "ireg", "pfix", "nfix", "sp", "gp")
  reg_cols  <- paste0("R", seq_len(nregmax))
  all_measure_cols <- as.character(outer(measures, reg_cols,
                                         FUN = function(m, r) paste0(m, "_", r)))

  # Identify unique trials
  trial_keys <- unique(fixations[, c("subj", "cond", "item", "seq")])

  result_rows <- list()

  for (ti in seq_len(nrow(trial_keys))) {
    subj  <- trial_keys$subj[ti]
    cond  <- trial_keys$cond[ti]
    item  <- trial_keys$item[ti]
    seq_n <- trial_keys$seq[ti]

    # Subset fixations for this trial
    trial_fix <- fixations[fixations$subj == subj &
                             fixations$cond == cond &
                             fixations$item == item &
                             fixations$seq  == seq_n, ]

    # Sort by fix_num if present, otherwise by row order (already ordered)
    if ("fix_num" %in% names(trial_fix)) {
      trial_fix <- trial_fix[order(trial_fix$fix_num), ]
    }

    fix_df <- data.frame(
      x = trial_fix$x,
      y = trial_fix$y,
      s = trial_fix$s,
      e = trial_fix$e
    )

    # Look up region boundaries
    cnt_row <- regions[!is.na(regions$item) & regions$item == item &
                       !is.na(regions$cond) & regions$cond == cond, ]
    if (nrow(cnt_row) == 0L) next

    nreg <- cnt_row$nreg[1L]
    start_cols <- paste0("start_R", seq_len(nreg))
    if (!all(start_cols %in% names(cnt_row))) next

    region_starts   <- as.integer(unlist(cnt_row[1L, start_cols]))
    region_end_last <- as.integer(cnt_row$end_last[1L])

    m <- compute_trial_measures(
      fix_df          = fix_df,
      region_starts   = region_starts,
      region_end_last = region_end_last,
      shorttime       = shorttime,
      longtime        = longtime,
      fpcutoff        = fpcutoff,
      ttcutoff        = ttcutoff,
      max_y           = max_y,
      width           = width
    )

    row <- list(subj = subj, item = item, cond = cond, seq = seq_n)
    for (mname in measures) {
      vals <- m[[mname]]
      for (ri in seq_len(nregmax)) {
        col <- paste0(mname, "_R", ri)
        row[[col]] <- if (ri <= nreg) vals[ri] else NA_real_
      }
    }

    result_rows[[length(result_rows) + 1L]] <- row
  }

  if (length(result_rows) == 0L) {
    empty <- tibble::tibble(
      subj = integer(), item = integer(), cond = integer(), seq = integer()
    )
    for (col in all_measure_cols) empty[[col]] <- numeric()
    return(empty)
  }

  out <- tibble::as_tibble(do.call(rbind, lapply(result_rows, as.data.frame,
                                                  stringsAsFactors = FALSE)))
  out$subj <- as.integer(out$subj)
  out$item <- as.integer(out$item)
  out$cond <- as.integer(out$cond)
  out$seq  <- as.integer(out$seq)
  out
}


# ---------------------------------------------------------------------------
#' Generate a CNT region-boundary data frame from an EyeLink ASC file
#'
#' Parses \code{REGION CHAR} and \code{TRIALID} messages from an EyeLink ASC
#' file and produces a region-boundary data frame in the same format as
#' \code{\link{read_cnt}}.  Words are reconstructed from characters by
#' detecting space characters; each word becomes one analysis region.
#'
#' @param filepath        Path to the EyeLink ASC file.
#' @param trial_pattern   Regular expression matching the trial type prefix
#'                        in TRIALID messages to include (default \code{"^E"}
#'                        for experimental trials only).
#' @param lowest_cond     Minimum condition number to include (default 1).
#' @param highest_cond    Maximum condition number to include (default 999).
#' @param spillover_gap   Number of blank character positions between the end
#'                        of the last word and the start of the trailing
#'                        spillover region (default 1).
#' @param spillover_size  Width in characters of the trailing spillover region
#'                        (default 20).
#' @return A data frame with columns \code{item}, \code{cond}, \code{nreg},
#'   \code{end_last}, and \code{start_R1} through \code{start_R\{n\}} where
#'   n is the maximum number of word regions found across all included trials.
#' @export
asc_to_cnt <- function(filepath,
                        trial_pattern  = "^E",
                        lowest_cond    = 1L,
                        highest_cond   = 999L,
                        spillover_gap  = 1L,
                        spillover_size = 20L) {
  lines <- readLines(filepath, warn = FALSE)
  n <- length(lines)

  records <- list()
  in_trial <- FALSE
  cond <- NA_integer_; item <- NA_integer_
  char_data <- list()  # list of (idx, char) per trial

  for (line in lines) {
    # ---- Trial start ----
    if (grepl("TRIALID", line)) {
      # Extract the trial ID value and check if it matches the pattern
      m_tid <- regmatches(line, regexpr("TRIALID\\s+(\\S+)", line))
      if (length(m_tid) > 0) {
        trial_id <- sub("TRIALID\\s+", "", m_tid)
        if (grepl(trial_pattern, trial_id)) {
          # Parse components
          m <- regmatches(trial_id, regexpr("^[A-Za-z]+(\\d+)[A-Za-z](\\d+)", trial_id))
          if (length(m) > 0) {
            nums <- regmatches(m, gregexpr("\\d+", m))[[1]]
            if (length(nums) >= 2) {
              cond  <- as.integer(nums[1])
              item  <- as.integer(nums[2])
              in_trial <- TRUE
              char_data <- list()
            } else {
              in_trial <- FALSE
            }
          } else {
            in_trial <- FALSE
          }
        } else {
          in_trial <- FALSE
        }
      } else {
        in_trial <- FALSE
      }
      next
    }

    if (!in_trial) next

    # ---- REGION CHAR ----
    if (grepl("REGION CHAR", line)) {
      parts <- unlist(strsplit(trimws(line), "\\s+"))
      np <- length(parts)
      # Format: MSG timestamp REGION CHAR idx word_id char x_start y_start x_end y_end
      # When char is a space, whitespace splitting removes it → 10 parts instead of 11.
      # Use end-based indexing: x_start is always parts[np-3], x_end is parts[np-1].
      # idx is always parts[5], word_id is always parts[6].
      # char is parts[7] when np==11, or " " when np==10.
      if (np >= 10L) {
        idx <- as.integer(parts[5L])
        word_id <- as.integer(parts[6L])
        ch <- if (np >= 11L) parts[7L] else " "
        char_data[[length(char_data) + 1L]] <- list(idx = idx, word_id = word_id, char = ch)
      }
      next
    }

    # ---- TRIAL_RESULT / end of trial ----
    if (grepl("TRIAL_RESULT", line)) {
      if (in_trial && !is.na(cond) && !is.na(item) &&
          cond >= lowest_cond && cond <= highest_cond &&
          length(char_data) > 0L) {
        # Sort by character index
        char_data <- char_data[order(vapply(char_data, `[[`, 0L, "idx"))]
        chars <- vapply(char_data, `[[`, "", "char")
        idxs  <- vapply(char_data, `[[`, 0L, "idx")

        # Reconstruct words by detecting spaces
        # A new word starts at the first character, or at any non-space character
        # that follows a space character.
        word_starts <- integer(0)
        for (ci in seq_along(chars)) {
          if (ci == 1L) {
            word_starts <- c(word_starts, idxs[ci])
          } else if (chars[ci - 1L] == " " && chars[ci] != " ") {
            word_starts <- c(word_starts, idxs[ci])
          }
        }

        if (length(word_starts) > 0L) {
          # Last character index + gap + spillover
          text_end <- max(idxs)
          spill_start <- text_end + spillover_gap
          spill_end_last <- spill_start + spillover_size
          region_starts <- c(word_starts, spill_start)
          nreg <- length(region_starts)

          records[[length(records) + 1L]] <- list(
            item     = item,
            cond     = cond,
            nreg     = nreg,
            starts   = region_starts,
            end_last = spill_end_last
          )
        }
      }
      in_trial <- FALSE
      next
    }
  }

  if (length(records) == 0L) {
    return(data.frame(item = integer(), cond = integer(), nreg = integer(),
                      end_last = integer()))
  }

  max_nreg <- max(vapply(records, `[[`, 0L, "nreg"))

  rows <- lapply(records, function(r) {
    row <- data.frame(item     = r$item,
                      cond     = r$cond,
                      nreg     = r$nreg,
                      end_last = r$end_last,
                      stringsAsFactors = FALSE)
    for (i in seq_len(max_nreg)) {
      col <- paste0("start_R", i)
      row[[col]] <- if (i <= r$nreg) r$starts[i] else NA_integer_
    }
    row
  })

  do.call(rbind, rows)
}


# ===========================================================================
# Original monolithic analyze_eyedry function (preserved)
# ===========================================================================

#' @export
analyze_eyedry <- function(asc_files    = NULL,
                           da1_files    = NULL,
                           cnt_file,
                           shorttime    = 80L,
                           longtime     = 800L,
                           fpcutoff     = 2000L,
                           ttcutoff     = 4000L,
                           width        = 80L,
                           max_y        = 1000L,
                           lowest_cond  = 1L,
                           highest_cond = 999L,
                           nregmax      = 7L) {
  # ---- Read region boundaries ----
  cnt <- read_cnt(cnt_file)
  if (nrow(cnt) == 0L) stop("No region data found in CNT file: ", cnt_file)

  # ---- Collect all subject-level trial lists ----
  all_subject_data <- list()
  subj_id <- 0L

  if (!is.null(da1_files)) {
    for (fname in da1_files) {
      subj_id <- subj_id + 1L
      dat <- read_da1(fname, width = width)
      dat$subj <- subj_id
      all_subject_data[[length(all_subject_data) + 1L]] <- dat
    }
  }

  if (!is.null(asc_files)) {
    for (fname in asc_files) {
      subj_id <- subj_id + 1L
      dat <- read_asc(fname, lowest_cond = lowest_cond,
                      highest_cond = highest_cond)
      dat$subj <- subj_id
      all_subject_data[[length(all_subject_data) + 1L]] <- dat
    }
  }

  if (length(all_subject_data) == 0L) {
    stop("No input files specified. Provide da1_files or asc_files.")
  }

  # ---- Measure column names ----
  measures  <- c("ff", "fp", "tt", "oreg", "ireg", "pfix", "nfix", "sp", "gp")
  reg_cols  <- paste0("R", seq_len(nregmax))
  all_measure_cols <- as.character(outer(measures, reg_cols,
                                         FUN = function(m, r) paste0(m, "_", r)))

  # ---- Process each subject ----
  result_rows <- list()

  for (subj_dat in all_subject_data) {
    subj <- subj_dat$subj

    for (trial in subj_dat$trials) {
      item <- trial$item
      cond <- trial$cond
      seq_n <- trial$seq

      # Filter by condition range
      if (is.na(cond) || cond < lowest_cond || cond > highest_cond) next

      # Look up region boundaries in CNT
      cnt_row <- cnt[!is.na(cnt$item) & cnt$item == item &
                     !is.na(cnt$cond) & cnt$cond == cond, ]
      if (nrow(cnt_row) == 0L) next  # item/cond not in CNT

      nreg <- cnt_row$nreg[1L]

      # Extract region starts (start_R1 ... start_R{nreg})
      start_cols <- paste0("start_R", seq_len(nreg))
      if (!all(start_cols %in% names(cnt_row))) next

      region_starts  <- as.integer(unlist(cnt_row[1L, start_cols]))
      region_end_last <- as.integer(cnt_row$end_last[1L])

      # Compute all measures
      m <- compute_trial_measures(
        fix_df          = trial$fixations,
        region_starts   = region_starts,
        region_end_last = region_end_last,
        shorttime       = shorttime,
        longtime        = longtime,
        fpcutoff        = fpcutoff,
        ttcutoff        = ttcutoff,
        max_y           = max_y,
        width           = width
      )

      # Build output row (pad to nregmax)
      row <- list(subj = subj, item = item, cond = cond, seq = seq_n)
      for (mname in measures) {
        vals <- m[[mname]]  # length nreg
        for (ri in seq_len(nregmax)) {
          col <- paste0(mname, "_R", ri)
          row[[col]] <- if (ri <= nreg) vals[ri] else NA_real_
        }
      }

      result_rows[[length(result_rows) + 1L]] <- row
    }
  }

  if (length(result_rows) == 0L) {
    # Return empty tibble with correct columns
    empty <- tibble::tibble(
      subj = integer(), item = integer(), cond = integer(), seq = integer()
    )
    for (col in all_measure_cols) {
      empty[[col]] <- numeric()
    }
    return(empty)
  }

  # Bind rows into a tibble
  out <- tibble::as_tibble(do.call(rbind, lapply(result_rows, as.data.frame,
                                                  stringsAsFactors = FALSE)))

  # Ensure integer columns for identifiers
  out$subj <- as.integer(out$subj)
  out$item <- as.integer(out$item)
  out$cond <- as.integer(out$cond)
  out$seq  <- as.integer(out$seq)

  out
}
