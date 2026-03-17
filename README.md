# fixated

<!-- badges: start -->
<!-- badges: end -->

`fixated` is an R package for processing and analyzing eye movement data
recorded by eye trackers such as the SR Research EyeLink system.

## Features

- **Read raw data** – Parse EyeLink ASC files (`read_asc()`) or generic CSV
  files (`read_samples_csv()`) into tidy data frames.
- **Detect fixations** – Identify fixations from raw gaze samples using the
  dispersion-threshold identification (I-DT) algorithm (`detect_fixations()`).
- **Clean fixations** – Merge short adjacent fixations and remove outliers
  based on duration or spatial bounds (`clean_fixations()`).
- **Read regions of interest** – Load word-level bounding boxes from a CSV
  file (`read_roi()`).
- **Compute eye-movement measures** – Calculate standard reading-research
  measures per word (`compute_eye_measures()`):
  - **FFD** – First fixation duration
  - **GD** – Gaze duration (first-pass reading time)
  - **GPT** – Go-past time (regression-path duration)
  - **TVT** – Total viewing time

## Installation

You can install the development version from GitHub:

```r
# install.packages("remotes")
remotes::install_github("bernhardangele/fixated")
```

## Quick start

The following example shows how to read an EyeLink ASC file, compute reading measures, and launch an interactive visualisation.

```r
library(fixated)

# 1. Read an EyeLink ASC file
# result is a list containing samples, events, word_boundaries, and trial_db
asc_file <- system.file("extdata", "sub_1_example.asc", package = "fixated")
result <- read_asc(asc_file)

# 2. Extract fixations and word ROIs
fixations <- result$events[result$events$type == "FIXATION", ]
roi       <- result$word_boundaries

# 3. Compute per-word eye-movement measures (FFD, GD, GPT, TVT)
measures <- compute_eye_measures(fixations, roi, trial_db = result$trial_db)
head(measures)

# 4. Launch interactive Shiny visualisation
# Browsing trials with gaze overlays and word boundaries
if (interactive()) {
  plot_trials_shiny(result)
}
```

## Input file formats

### ASC file (EyeLink)

ASC files are produced by SR Research's `edf2asc` converter.  `read_asc()`
extracts raw samples (time, x, y, pupil, eye) and events (FIXATION, SACCADE,
BLINK) from the file.

### Samples CSV

A flat CSV with at least `time`, `x`, and `y` columns.  Additional columns
such as `pupil`, `eye`, `trial`, and `participant` are automatically detected.
Column names can be remapped via the `col_map` argument of `read_samples_csv()`.

### ROI CSV

A CSV with columns `trial`, `word_id`, `x_start`, `x_end`, `y_start`,
`y_end`, and optionally `word`.  Each row defines the bounding box of one
word in one trial.  Column names can be remapped via the `col_map` argument
of `read_roi()`.

Example:

| trial | word_id | word  | x_start | x_end | y_start | y_end |
|------:|--------:|-------|--------:|------:|--------:|------:|
|     1 |       1 | The   |     100 |   195 |     380 |   420 |
|     1 |       2 | quick |     200 |   315 |     380 |   420 |
|     1 |       3 | fox   |     320 |   420 |     380 |   420 |

## Coordinate system

All spatial coordinates follow the **EyeLink convention**: (0, 0) is the
**top-left corner** of the display, with x increasing rightward and y
increasing downward.

## Development

```r
# Install development dependencies
install.packages(c("devtools", "testthat", "roxygen2"))

# Load the package interactively
devtools::load_all()

# Run tests
devtools::test()

# Rebuild documentation
devtools::document()

# Run full R CMD check
devtools::check()
```

## License

MIT © Bernhard Angele