#' Read Eyelink ASC File and Extract Data
#'
#' This function reads an Eyelink ASC file from an experiment conducted with Opensesame, and extracts various data including raw samples, fixations, blinks, and word information that was written into the ASC file by Opensesame.
#'
#' @param data_file The path to the ASC data file to be read.
#' @param rate The sampling rate of the data. 
#' @param sentence_start_x The x-coordinate of the sentence start. Default is 125.
#'
#' @return A list containing various data frames and lists including trial information, messages, samples, words, fixations, saccades, and blinks.
#'
#' @import readr
#' @import dplyr
#'
#' @examples
#' # Provide the path to the ASC file
#' data_file <- "path/to/your/data.asc"
#'
#' # Read the ASC file and extract data
#' subject_data <- read_asc_file(data_file)
#'
#' @export
read_asc_file <- function(data_file, rate = rate, sentence_start_x = 125) {
  # Function code here
}

read_asc_file <- function(data_file, rate = rate,  sentence_start_x = 125){
  subject <- list()
  subject$filename <- data_file
  subject$rate = rate
  
  cat("\n"); cat(sprintf("Loading data file: %s", data_file), "\n")
  dataF <- readLines(data_file)
  
  ### get calibration data
  
  extract_validation_info <- function(dataF) {
    #' Extracts validation information from Eyelink EDF ASC file lines.
    #'
    #' @param dataF A character vector, where each element is a line from the EDF ASC file.
    #'
    #' @return A data frame. Each row contains the validation
    #'   information extracted from a single "!CAL VALIDATION HV9 LR" line.
    #'   Returns an empty data frame if no validation lines are found.
    
    calibration_lines <- grep("!CAL VALIDATION HV9 LR", dataF, value = TRUE)
    
    if (length(calibration_lines) == 0) {
      return(data.frame())  # Return an empty data frame
    }
    
    # Use a regular expression to extract the relevant parts
    matches <- regmatches(
      calibration_lines,
      regexec(
        "MSG\\t(\\d+) !CAL VALIDATION HV9 LR (LEFT|RIGHT)\\s+(\\w+)\\s+ERROR\\s+([0-9.]+)\\s+avg\\.\\s+([0-9.]+)\\s+max\\s+OFFSET\\s+([0-9.]+)\\s+deg\\.\\s+([-0-9.]+),([-0-9.]+) pix\\.",
        calibration_lines
      )
    )
    
    # Convert the list of matches into a data frame
    #  - First, remove the first element of each match (which is the entire matched string)
    matches <- lapply(matches, function(x) x[-1])
    
    # - Then, convert the list to a matrix (filling with NA if necessary) and then to a data frame
    if (length(matches) > 0 ) { #check that matches exists
      df <- data.frame(do.call(rbind, lapply(matches, function(x) {
        if(length(x) < 8) { #make it the correct length if the regex somehow fails
          length(x) <- 8
        }
        x
      }
      )))
      
      # Set column names and types
      colnames(df) <- c("timestamp", "eye", "quality", "avg_error", "max_error", "offset_deg", "x_offset", "y_offset")
      df$timestamp <- as.integer(df$timestamp)
      df$avg_error <- as.numeric(df$avg_error)
      df$max_error <- as.numeric(df$max_error)
      df$offset_deg <- as.numeric(df$offset_deg)
      df$x_offset <- as.numeric(df$x_offset)
      df$y_offset <- as.numeric(df$y_offset)
      return(df)
    } else {
      return(data.frame()) #return the empty df if matches has length 0
    }
  }
  
  
  calibration_results <- extract_validation_info(dataF)
  
  ### get start and end times ###
  
  S_W <- which(grepl('TRIAL \\d+ ITEM \\d+ WORD 1\\s', dataF)) 
  end_stop<- which(grepl('stop_trial', dataF))
  trial_start_t<- which(grepl('start_trial', dataF))
  
  ### get trial infos:
  ID<- which(grepl('TRIALID', dataF));
  trial_text<- dataF[ID]
  trials<- substr(trial_text, unlist(gregexpr(pattern =' ',trial_text[1]))[2]+1, nchar(trial_text))
  trials<- gsub(" ", "", trials)
  itemN <- as.numeric(str_match(trials, pattern = '\\d{1,4}'))
  cond <- str_match(trials, pattern = '_(\\w{3,9})')[,2]
  
  trial_db <- data.frame(itemN, cond, trial_start_t, end_stop, S_W)
  
  ntrials<- nrow(trial_db)
  
  cat("Found", ntrials, "trials.\n")
  
  # add more columns to trial_db
  #trial_db$sentence_start_x <- sentence_start_x
  trial_db$has_display_off <- NA
  trial_db$sentence_start_x <- NA
  trial_db$sentence_number <- NA
  trial_db$subject_nr <- NA
  trial_db$target_word_nr <- NA
  trial_db$target <- NA
  trial_db$trial_type <- NA
  trial_db$timestamp_display_on <- NA
  trial_db$timestamp_display_off <- NA
  trial_db$response <- NA
  trial_db$acceptable <- NA
  trial_db$recording_sampling_rate <- NA
  
  
  trial_msgs <- list()
  trial_samples <- list()
  trial_words <- list()
  
  # eyelink fixations/saccades/blinks
  trial_fixations <- list()
  trial_saccades <- list()
  trial_blinks <- list()
  
  
  for (i in 1:nrow(trial_db)) {
    start_line <- trial_db$S_W[i]
    start_line_samples <- trial_db$trial_start_t[i]
    end_line_samples <- trial_db$end_stop[i]
    if (i < nrow(trial_db)){
      end_line <- trial_db$S_W[i+1]-1  
    } else {
      end_line <- end_line_samples
    }
    
    # get trial lines from file (use all the lines for messages so we don't miss the trial info, but only lines after trial start flag for samples)
    trial_lines_msgs <- dataF[start_line:end_line]
    
    trial_lines_samples <- dataF[start_line_samples:end_line_samples]
    
    # get messages from the built-in Eyelink Saccade detection algorithm
    trial_lines_efix <- grep(x = trial_lines_samples, pattern = "EFIX", value = TRUE)
    fixations <- read_table(trial_lines_efix, col_names = c("EFIX", "eye", "t_start", "t_end", "duration", "x", "y", "pupil")) %>% select(-EFIX)
    
    trial_lines_esacc <- grep(x = trial_lines_samples, pattern = "ESACC", value = TRUE)
    saccades <- read_table(trial_lines_esacc, col_names = c("ESACC", "eye", "t_start", "t_end", "duration", "x_start", "y_start", "x_end", "y_end", "amplitude", "peak_velocity")) %>% select(-ESACC)
    
    trial_lines_eblink <- grep(x = trial_lines_samples, pattern = "EBLINK", value = TRUE)
    if(length(trial_lines_eblink) > 1){
      blinks <- read_table(trial_lines_eblink, col_names = c("EBLINK", "eye", "t_start", "t_end", "duration")) %>% select(-EBLINK)
    } else if(length(trial_lines_eblink) == 1) {
      # we have a problem if there is only one single blink since read_table interprets the string as a file name
      # fix: duplicate the line and then only save the first row of the resulting tibble
      trial_lines_eblink <- c(trial_lines_eblink, trial_lines_eblink)
      blinks <- read_table(trial_lines_eblink, col_names = c("EBLINK", "eye", "t_start", "t_end", "duration")) %>% select(-EBLINK) %>% .[1,]
    } else {
      blinks <- NA
    }
    
    
    # filter lines into messages and samples
    pattern_samples <- "^\\d{5,10}(?:\\s+\\-{0,1}\\d{0,5}\\.{1}\\d{0,1}){6}\\s[\\.ICR]{5}$"
    pattern_msg <- "^MSG\\s+\\d{5,10}"
    trial_msg <- trial_lines_msgs[grepl(pattern_msg, trial_lines_msgs, perl = TRUE)]
    trial_sample <- trial_lines_samples[grepl(pattern_samples, trial_lines_samples, perl = TRUE)]
    
    # read_table is by far the fastest way to read the samples into a data frame/tibble
    sample <- read_table(trial_sample,
                         col_names = c("timestamp", "x_l", "y_l", "pupil_l", "x_r", "y_r", "pupil_r", "flags"),
                         col_types = cols(
                           timestamp = col_integer(),
                           x_l = col_number(),
                           y_l = col_number(),
                           pupil_l = col_number(),
                           x_r = col_number(),
                           y_r = col_number(),
                           pupil_r = col_number(),
                           flags = col_character()),
                         na = "."
    )
    trial_samples[[i]] <- sample
    cat(i, " ")
    
    # get more information about the trial from the messages
    
    # get_trial_info_from_msg finds the matching line, then extracts the info from the single capture group
    trial_db[i,]$sentence_start_x <- get_trial_info_from_msg(trial_msg, ("MSG\\t\\d+ var sentence_start_x (\\d+)")) 
    trial_db[i,]$sentence_number <- get_trial_info_from_msg(trial_msg, "MSG\\t\\d+ var sentence_number (\\d+)")
    trial_db[i,]$subject_nr <- get_trial_info_from_msg(trial_msg, "MSG\\t\\d+ var subject_nr (\\d+)")
    trial_db[i,]$target_word_nr <- get_trial_info_from_msg(trial_msg, "MSG\\t\\d+ var target_word_nr (\\d+)")
    trial_db[i,]$target <- get_trial_info_from_msg(trial_msg, "MSG\\t\\d+ var target ([\\?a-z]+)")
    trial_db[i,]$trial_type <- get_trial_info_from_msg(trial_msg, "MSG\\t\\d+ var trial_type ([a-z]+)")
    trial_db[i,]$timestamp_display_on <- get_trial_info_from_msg(trial_msg, "MSG\\t(\\d+) DISPLAY ON")
    trial_db[i,]$timestamp_display_off <-  get_trial_info_from_msg(trial_msg, "MSG\\t(\\d+) DISPLAY OFF")
    # some trials were never properly terminated. In this case, this flag helps us remove them from the analysis.
    trial_db[i,]$has_display_off <- !is.na(trial_db[i,]$timestamp_display_off)
    trial_db[i,]$response <- get_trial_info_from_msg(trial_msg, "MSG\\t\\d+ var response ([0-5return]+)")
    trial_db[i,]$acceptable <- get_trial_info_from_msg(trial_msg, "MSG\\t\\d+ var acceptable ([TRUEFALS]+)")
    trial_db[i,]$recording_sampling_rate <- get_trial_info_from_msg(trial_msg, "MSG\\t\\d+ !MODE RECORD [CR]+ (\\d+) \\d \\d [LR]+", multiple_allowed = TRUE)
    
    # save the msgs just in case
    trial_msgs[[i]] <- trial_msg
    
    # get_word_info_from_msg makes a data frame with all the words in the sentence from the messages written by Opensesame into the ASC at each trial start
    trial_words[[i]] <- get_word_info_from_msg(trial_msg)
    
    # save Eyelink saccade detection algorithm information
    
    trial_fixations[[i]] <- fixations
    trial_saccades[[i]] <- saccades
    trial_blinks[[i]] <- blinks
    
  }
  
  # make sure variables are in the correct format in trial_db before we save it
  trial_db <- trial_db %>% mutate(
    response = case_match(.x = trial_db$response, 
                          "1" ~ 1,
                          "2" ~ 2,
                          "return" ~ 3,
                          "3" ~ 4,
                          "4" ~ 5),
    timestamp_display_on = as.numeric(timestamp_display_on),
    timestamp_display_off = as.numeric(timestamp_display_off))
  
  subject$calibration_results <- calibration_results
  
  subject$trial_db <- trial_db
  subject$trial_msgs <- trial_msgs
  subject$trial_samples <- trial_samples
  subject$trial_words <- trial_words
  
  subject$trial_fixations <- trial_fixations
  subject$trial_saccades <- trial_saccades
  subject$trial_blinks <- trial_blinks
  return(subject)
}
