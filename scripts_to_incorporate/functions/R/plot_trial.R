#' Visualize Eye Tracking Experiment Trials
#'
#' This function generates a visualization of eye tracking data for a specific trial of the sampling rate eye tracking experiment.
#'
#' @param subject The subject data object containing eye tracking and trial information.
#' @param trial_nr The trial number to visualize.
#' @param show_original_samples Display the original eye tracking samples.
#' @param show_downsampled_samples Display the downsampled eye tracking samples.
#' @param downsampling_rate The rate at which to downsample the samples (in Hz).
#' @param show_eyelink_fixations Display Eyelink fixations.
#' @param show_EK_fixations Display fixations determined by the Engbert and Kliegl algorithm from the original samples.
#' @param show_EK_fixations_downsampled Display fixations determined by the Engbert and Kliegl algorithm from the downsampled samples.
#' @param show_words Display word boundaries and labels.
#' @param show_word_boundaries Display word boundaries.
#' @param x_limits The x-axis limits for the plot.
#' @param y_limits The y-axis limits for the plot.
#' @param lambda Smoothing parameter for fixation identification.
#' @param lambda_downsampled Smoothing parameter for fixation identification on downsampled samples.
#' @param smooth.coords Whether to perform coordinate smoothing on fixation data.
#' @param smooth.saccades Whether to perform saccade smoothing on fixation data.
#' @param min_duration_for_elimination Minimum duration for eliminating fixations.
#' @param max_duration_for_elimination_or_merge Maximum duration for elimination or merge criteria.
#' @param max_distance_for_merge Maximum distance for merge criteria.
#' @param letter_width The width of a single letter.
#'
#' @details This function visualizes eye tracking data for a specific trial, including original and downsampled samples, Eyelink fixations, EyeKnows fixations, word boundaries, and labels. It provides options to control which elements to display and allows for customization of plot aesthetics. It may be possible to use this function to plot other eye tracking trials as well.
#'
#' @import ggplot2
#' @import dplyr
#'
#' @return A ggplot object representing the eye tracking data visualization.
#'
#' @export

plot_trial <- function(subject,
                       trial_nr,
                       use_eye = "R",
                       show_original_samples = TRUE,
                       show_downsampled_samples = TRUE,
                       downsampling_method = "drop",
                       downsampling_rate = 125,
                       show_eyelink_fixations = TRUE,
                       show_EK_fixations = TRUE,
                       show_EK_fixations_downsampled = TRUE,
                       show_words = TRUE,
                       show_word_boundaries = TRUE,
                       x_limits = c(0, 1920),
                       y_limits = c(1080, 0),
                       lambda = 6,
                       lambda_downsampled = lambda,
                       smooth.coords = FALSE,
                       smooth.saccades = TRUE,
                       min_duration_for_elimination = 800,
                       max_duration_for_elimination_or_merge = 80,
                       max_distance_for_merge = 12,
                       letter_width = 12,
                       words_font_size = 12,
                       df_with_fixed_words = sentences_words_espal) {
  if (trial_nr <= length(subject$trial_samples) &
      !is.null(subject$trial_samples[[trial_nr]])) {
    samples <- subject$trial_samples[[trial_nr]]
  } else {
    stop("Trial", trial_nr, "does not exist.")
  }
  
  if (trial_nr <= length(subject$trial_words) &
      !is.null(subject$trial_words[[trial_nr]])) {
    words <- subject$trial_words[[trial_nr]]
  } else {
    stop("No word information found for Trial", trial_nr, ".")
  }
  
  
  if (!is.na(subject$trial_db[trial_nr, "trial_start_t"])) {
    trial_db_for_trial <- subject$trial_db[trial_nr, ]
  } else {
    stop("Trial", trial_nr, "not found in trial_db.")
  }
  
  if (!is.null(df_with_fixed_words)) {
    words <-
      words %>% mutate(
        condition = paste("sentence", trial_db_for_trial$cond, sep = "_"),
        has_missing_char = str_detect(words$word, "\\?")
      ) %>%
      left_join(
        df_with_fixed_words,
        by = join_by(
          condition == frequency_condition,
          sentence_nr == sentence_number,
          word_nr == word_number
        ),
        suffix = c("", ".espal")
      ) %>%
      mutate(word = ifelse(!has_missing_char, word, word.espal))
  }
  
  original_sampling_rate <-
    trial_db_for_trial$recording_sampling_rate %>% as.numeric()
  
  modified_timestamps <- samples$timestamp
  if (original_sampling_rate == 2000) {
    # in the 2000 Hz files, there are two timestamps per millisecond. Disambiguate them by adding .5 to the second one
    # this works if there are no breaks in recording
    duplicated_indices <-
      which(duplicated(modified_timestamps))
    
    # Add 0.5 to the second occurrence of each duplicated value
    modified_timestamps[duplicated_indices] <-
      modified_timestamps[duplicated_indices] + 0.5
  }
  samples$timestamp <- modified_timestamps
  
  if (!is.null(downsampling_rate)) {
    downsampling_factor <-
      determine_downsampling_factor(original_sampling_rate, downsampling_rate)
    
    if (downsampling_method == "drop") {
      selected_sample_indices <-
        seq(from = 1,
            to = nrow(samples),
            by = downsampling_factor)
      samples_downsampled <- samples[selected_sample_indices,]
    } else if (downsampling_method == "average") {
      samples_downsampled <- samples %>%
        mutate(sample_group = ceiling(row_number() / downsampling_factor)) %>%
        group_by(sample_group) %>%
        summarise(
          timestamp = mean(timestamp),
          x_l = round(mean(x_l), 1),
          x_r = round(mean(x_r), 1),
          y_l = round(mean(y_l), 1),
          y_r = round(mean(y_r), 1),
          .groups = "drop_last"
        ) %>%
        dplyr::select(timestamp, x_l, y_l, x_r, y_r)
    }
    
    
  }
  
  if (show_eyelink_fixations) {
    if (length(subject$trial_fixations) >= trial_nr &
        !is.null(subject$trial_fixations[[trial_nr]])) {
      eyelink_fixations <-
        subject$trial_fixations[[trial_nr]] %>% filter(
          eye == use_eye & t_start > trial_db_for_trial$timestamp_display_on &
            t_end < trial_db_for_trial$timestamp_display_off
        )
    } else {
      stop("No Eyelink fixations found for trial", trial_nr, ".")
    }
  }
  
  samples_original <- samples %>%  dplyr::filter(
    timestamp > trial_db_for_trial$timestamp_display_on &
      timestamp < trial_db_for_trial$timestamp_display_off
  )
  
  if (show_EK_fixations) {
    fixations_EK_original <-
      process_EK_trial(
        trial_samples = samples_original,
        trial_db_for_trial = trial_db_for_trial,
        trial_words = words,
        use_eye = use_eye,
        lambda = lambda,
        smooth.coords = smooth.coords,
        smooth.saccades = smooth.saccades,
        min_duration_for_elimination = min_duration_for_elimination,
        max_duration_for_elimination_or_merge = max_duration_for_elimination_or_merge,
        max_distance_for_merge = max_distance_for_merge
      )
    
  }
  
  if (show_downsampled_samples | show_EK_fixations_downsampled) {
    samples_downsampled <- samples_downsampled %>%  dplyr::filter(
      timestamp > trial_db_for_trial$timestamp_display_on &
        timestamp < trial_db_for_trial$timestamp_display_off
    )
    
    fixations_EK_downsampled <-
      process_EK_trial(
        trial_samples = samples_downsampled,
        trial_db_for_trial = trial_db_for_trial,
        trial_words = words,
        use_eye = use_eye,
        lambda = lambda,
        smooth.coords = smooth.coords,
        smooth.saccades = smooth.saccades,
        min_duration_for_elimination = min_duration_for_elimination,
        max_duration_for_elimination_or_merge = max_duration_for_elimination_or_merge,
        max_distance_for_merge = max_distance_for_merge
      )
    
  }
  if (show_words) {
    words <-
      words %>% mutate(left_x = lag(word_right_x_boundary, default = 115))
    letter_data <- words %>%
      mutate(letters = strsplit(word, "")) %>%
      unnest(letters) %>%
      group_by(trial_nr, sentence_nr, word_nr) %>%
      mutate(letter_nr = row_number(),
             letter_x = left_x + (letter_nr - 1) * letter_width)
    
  }
  samples_plot <- ggplot(samples_original, aes(x = x_r, y = y_r)) +
    coord_equal(expand = FALSE) +
    scale_x_continuous(name = "x", limits = x_limits) +
    scale_y_reverse(name = "y", limits = y_limits) +
    labs(x = "X Coordinate", y = "Y Coordinate", color = "Timestamp")
  
  if (show_original_samples) {
    samples_plot <- samples_plot + geom_point(size = .01, alpha = 0.6)
  }
  
  if (show_downsampled_samples) {
    samples_plot <-
      samples_plot + geom_point(
        data = samples_downsampled,
        aes(x = x_r, y = y_r),
        color = "orange",
        size = .015, alpha = 0.7
      )
  }
  if (show_eyelink_fixations) {
    samples_plot <-
      samples_plot + geom_point(
        data = eyelink_fixations,
        aes(x = x, y = y, size = duration),
        color = "blue",
        alpha = 0.5
      ) +
      geom_path(
        data = eyelink_fixations,
        aes(x = x, y = y),
        color = "blue",
        alpha = 0.5
      )
  }
  if (show_EK_fixations_downsampled) {
    samples_plot <-
      samples_plot + geom_point(
        data = fixations_EK_downsampled,
        aes(x = x, y = y, size = duration),
        color = "red",
        alpha = 0.5
      ) +
      geom_path(
        data = fixations_EK_downsampled,
        aes(x = x, y = y),
        color = "red",
        alpha = 0.7
      )
  }
  if (show_EK_fixations) {
    samples_plot <-
      samples_plot + geom_point(
        data = fixations_EK_original,
        aes(x = x, y = y, size = duration),
        color = "green",
        alpha = 0.5
      ) +
      geom_path(
        data = fixations_EK_original,
        aes(x = x, y = y),
        color = "green",
        alpha = 0.5
      )
  }
  if (show_words) {
    samples_plot <-
      samples_plot + geom_text(
        data = letter_data,
        aes(x = letter_x + 6, y = 537, label = letters),
        vjust = 0,
        color = "black",
        size = words_font_size,
        family = "sans"
      )
  }
  if (show_word_boundaries) {
    samples_plot <-
      samples_plot + geom_vline(
        data = words,
        aes(xintercept = word_right_x_boundary),
        linetype = "dashed",
        color = "grey",
        alpha = 0.5
      )
  }
  samples_plot
}
