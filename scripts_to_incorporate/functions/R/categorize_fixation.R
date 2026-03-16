categorize_fixation <- function(sequence) {
  categories <- character(length(sequence))
  # these words can receive first pass fixations
  first_occurrences <- unique(sequence)
  
  for (i in seq_along(sequence)) {
    element <- sequence[i]
    
    if (element %in% first_occurrences) {
      categories[i] <- "first pass"
      # remove words that no longer can receive first pass fixations
      # this includes all word numbers < current element, even if they were not fixated before
      first_occurrences <-
        setdiff(first_occurrences, min(sequence):element)
    } else if (i > 1 &&
               sequence[i] == sequence[i - 1] &
               (categories[i - 1] == "first pass" |
                categories[i - 1] == "first pass refixation")) {
      categories[i] <- "first pass refixation"
    } else {
      categories[i] <- "other fixation"
    }
  }
  
  return(categories)
}
