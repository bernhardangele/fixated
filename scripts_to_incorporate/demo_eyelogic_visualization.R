# Demo: EyeLogic Data Visualization with Fixation Detection
# This script demonstrates how to load EyeLogic data, detect fixations,
# and visualize the results using plot_trials_shiny_fast
library(tidyverse)
# Load the fixated package
devtools::load_all(recompile = TRUE)

# -------------------------------------------------------------------------
# Step 1: Load the EyeLogic CSV file
# -------------------------------------------------------------------------
message("Loading EyeLogic CSV file...")

# Read the EyeLogic file
# This returns a list with samples, word_boundaries, trial_db, etc.
eyelogic_result <- read_eyelogic("data_to_incorporate/subject-5.eyelogic.csv", opensesame_csv_path = "data_to_incorporate/subject-999.csv")

asc_result <- read_asc("data_to_incorporate/sub_1.asc")

# Check what was loaded
message(sprintf("Loaded %d samples", nrow(eyelogic_result$samples)))
message(sprintf("Found %d trials", nrow(eyelogic_result$trial_db)))
message(sprintf("Found %d word boundaries", nrow(eyelogic_result$word_boundaries)))

# View the data structure
cat("\n--- Samples (first 6 rows) ---\n")
print(head(eyelogic_result$samples))

cat("\n--- Trial DB (first 6 rows) ---\n")
print(head(eyelogic_result$trial_db))

cat("\n--- Word Boundaries (first 6 rows) ---\n")
print(head(eyelogic_result$word_boundaries))

# increment trial nr in samples by one
eyelogic_result$samples <- eyelogic_result$samples %>% mutate(trial_nr = trial_nr + 1)
eyelogic_result$trial_db <- eyelogic_result$trial_db %>% mutate(trial_nr = trial_nr + 1)

# -------------------------------------------------------------------------
# Step 2: Detect fixations from raw samples
# -------------------------------------------------------------------------
message("\nDetecting fixations using I-DT algorithm...")

# Note: read_eyelogic converts timestamps to milliseconds by default,
# so we can use standard millisecond values for min_duration

# Detect fixations
# - min_duration: minimum fixation duration in milliseconds
# - max_dispersion: maximum spatial dispersion in pixels
# - trial_col: column identifying trials
# - eye_col: column identifying eyes
fixations <- detect_fixations(method = "saccades",
  samples = eyelogic_result$samples,
  min_duration = 100,         # 100ms minimum fixation duration
  max_dispersion = 50,        # Maximum dispersion in pixels
  trial_col = "trial_nr",
  eye_col = "eye"
)

message(sprintf("Detected %d fixations", nrow(fixations)))

cat("\n--- Detected Fixations (first 6 rows) ---\n")
print(head(fixations))

# -------------------------------------------------------------------------
# Step 3: Launch the interactive Shiny visualization
# -------------------------------------------------------------------------
message("\nLaunching interactive visualization...")
message("The Shiny app will open in your default browser.")
message("Use the controls to:")
message("  - Navigate between trials")
message("  - Select which eye to view (L/R)")
message("  - Toggle display of samples, fixations, and word regions")
message("  - Filter samples by display onset/offset")

# Launch plot_trials_shiny_fast
# This function accepts either:
#   - An asc_result list (from read_asc or read_eyelogic), OR
#   - Individual data frames (samples, fixations, rois, etc.)

# Method 1: Pass the result directly (events will be NULL, so fixations is passed separately)
plot_trials_shiny_fast(
  asc_result = NULL,          # Set to NULL since EyeLogic doesn't have events
  samples = eyelogic_result$samples,
  fixations = fixations,      # Our detected fixations
  rois = eyelogic_result$word_boundaries,
  trial_db = eyelogic_result$trial_db,
  launch.browser = TRUE
)

# -------------------------------------------------------------------------
# Alternative: If you want to skip the visualization and just explore the data
# -------------------------------------------------------------------------
if (FALSE) {
  # Get fixations for a specific trial
  trial_nr <- 1
  trial_fixations <- fixations[fixations$trial_nr == trial_nr, ]
  trial_samples <- eyelogic_result$samples[eyelogic_result$samples$trial_nr == trial_nr, ]
  
  cat(sprintf("\nTrial %d: %d fixations, %d samples\n", 
              trial_nr, nrow(trial_fixations), nrow(trial_samples)))
  
  # Compute basic measures using compute_eye_measures
  measures <- compute_eye_measures(
    fixations = trial_fixations,
    rois = eyelogic_result$word_boundaries[eyelogic_result$word_boundaries$trial_nr == trial_nr, ],
    trial_db = eyelogic_result$trial_db[eyelogic_result$trial_db$trial_nr == trial_nr, ]
  )
  
  cat("\n--- Eye Movement Measures ---\n")
  print(measures)
}
