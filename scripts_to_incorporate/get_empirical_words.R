library(readr)
library(dplyr)
library(stringr)

# 1. Read the exact words we evaluated
df <- read_csv("eyelogic_all_words_measures.csv", show_col_types = FALSE)

# 2. Extract unique lowercase stripped words
words <- df$word %>% 
  tolower() %>% 
  str_replace_all("[[:punct:]]", "") %>% 
  unique() %>%
  .[!is.na(.) & . != ""] %>%
  sort()

# 3. Save as strict UTF-8
writeLines(words, "all_trial_words_utf8.txt", useBytes = TRUE)
cat("Extracted", length(words), "unique words from the empirical dataset to 'all_trial_words_utf8.txt'\n")
