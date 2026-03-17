library(fixated)
library(dplyr)

# Path to the EyeLink ASC file
# Note: This uses the sub_1.asc file from the data_to_incorporate folder
asc_path <- "data_to_incorporate/sub_1.asc"

if (!file.exists(asc_path)) {
  stop("Please ensure sub_1.asc exists at: ", asc_path)
}

# 1. Read the ASC file
# This function automatically parses samples, events, word boundaries, 
# and trial structure (for EyeLink + OpenSesame recordings).
cat("Reading ASC file...\n")
result <- read_asc(asc_path)

# 2. Subset to the first 3 trials (Trial 0, 1, and 2)
# Here we filter the various components of the result list
cat("Subsetting to the first 3 trials...\n")
trials_to_keep <- 0:2

result_subset <- result
result_subset$samples <- result$samples %>% 
  filter(trial_nr %in% trials_to_keep)

result_subset$events <- result$events %>% 
  filter(trial_nr %in% trials_to_keep)

if (!is.null(result$trial_db)) {
  result_subset$trial_db <- result$trial_db %>% 
    filter(trial_nr %in% trials_to_keep)
}

if (!is.null(result$word_boundaries)) {
  result_subset$word_boundaries <- result$word_boundaries %>% 
    filter(trial_nr %in% trials_to_keep)
}

# 3. Obtain word-based measures
# compute_eye_measures uses fixations and regions of interest (ROI).
# By default, it looks for trial_nr column.
cat("Computing word-based measures...\n")
# We extract fixations from events first (or use clean_fixations)
fixations <- result_subset$events %>% 
  filter(type == "FIXATION")

# use the word_boundaries from the ASC file as our ROI
roi <- result_subset$word_boundaries

measures <- compute_eye_measures(
  fixations = fixations,
  roi = roi,
  trial_db = result_subset$trial_db # Optional: for display-on filtering
)

cat("Successfully computed measures for", nrow(measures), "word-fixation entries.\n")
print(head(measures))

# 4. Launch the interactive Shiny plot
# This will open a browser window for visual inspection of the trials.
cat("Launching Shiny visualisation...\n")
# Note: In a non-interactive environment, launch.browser = FALSE
# plot_trials_shiny(result_subset)
