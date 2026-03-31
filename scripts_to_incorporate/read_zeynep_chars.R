#' Example: Reading non-standard character boundaries
#'
#' This script demonstrates how to use the `char_pattern` parameter in `read_asc`
#' to handle ASC files with "incorrectly" formatted character ROI messages,
#' such as those found in `sub_1_zeynep.asc`.
#'
#' In `sub_1_zeynep.asc`, the lines look like:
#' `MSG 1063932 TRIAL 1 ITEM 1005 WORD 1 L CHAR 196`
#'
#' These lines missing the word-level grouping and use the `WORD` label for 
#' character indices.

# If the package is not installed, you can source the function directly:
# source("R/read_asc.R")
# library(dplyr)
# library(stringr)

#library(fixated)

devtools::load_all()

# Path to the target file
asc_file <- "data_to_incorporate/sub_1_fixed.asc"

# Custom pattern for Zeynep's file:
# 1. Trial number
# 2. Item/Sentence number
# 3. Character ID (labeled as WORD in the file)
# 4. The character itself
# 5. The X-coordinate (labeled as CHAR in the file)
zeynep_pattern <- "^MSG\\t\\d+\\s+TRIAL\\s+(\\d+)\\s+ITEM\\s+(\\d+)\\s+WORD\\s+(\\d+)\\s+(\\S+)\\s+CHAR\\s+(\\d+)"

# Read the file
message("Reading ASC file with custom character pattern...")
zeynep_data <- read_asc(asc_file) #, char_pattern = zeynep_pattern)

# Check the results
if (!is.null(data$character_boundaries)) {
  message("Successfully parsed character boundaries!")
  print(head(data$character_boundaries))
} else {
  warning("No character boundaries found. Check if the pattern matches the file content.")
}

fixations <- get_eyelink_fixations(zeynep_data$events)

zeynep_data$fixations <- clean_fixations(fixations)

plot_trials_shiny_fast(zeynep_data)
