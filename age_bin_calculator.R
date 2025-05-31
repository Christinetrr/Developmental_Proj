# setwd("C:/Users/Caleb Solomon/Documents/GitHub/School/UMD/Research/toolkit_2.0_scripting/developmental-project")
load("/Users/christine/NTR/Toolkit/Toolkit_v2.0.RData")
library(dplyr)
library(readr)  # Better CSV handling

# Get the age of acquisition in years for each word we are interested in
tryCatch({
    # Read files using readr which handles encodings better
    df <- read_csv("prons.csv", show_col_types = FALSE)
    aoa <- read_csv("AoA_mainsheet.csv", show_col_types = FALSE)
    
    # Convert to data frame
    df <- as.data.frame(df)
    aoa <- as.data.frame(aoa)
    
    # Print raw column names for debugging
    print("Raw column names:")
    print(names(aoa))
    
    # Find and rename the WORD column by position (it should be the first column)
    if (length(names(aoa)) > 0) {
        names(aoa)[1] <- "WORD"
        print("Renamed first column to WORD")
    } else {
        stop("No columns found in aoa data frame")
    }
    
}, error = function(e) {
    print(paste("Error reading files:", e$message))
    stop("Failed to read input files properly")
})

# Verify column names are correct before proceeding
print("Column names in aoa after cleaning:")
print(names(aoa))
print("Column names in df:")
print(names(df))

# Create subset with age of acquisition data
aoa_subset <- aoa %>% select(WORD, AoArating_age_years)
df <- df %>%
    left_join(aoa_subset, by = c("word" = "WORD"))

# Print first few rows to verify join worked
print("First few rows after join:")
print(head(df))

# Take a subset of words whose age of acquisition is within a desired range (left-exclusive, right-inclusive)
subset_by_AoA_range <- function(df, lb, ub) {
    subset_df <- df %>%
        filter(.data$AoArating_age_years > lb & .data$AoArating_age_years <= ub)
    return(subset_df)
}

# This section of functions is from the extract_all_measures.R file on the Github
extract_summary <- function(grain, weight, measure) {
  # Create the appropriate dataset name
  dataset <- paste0(grain, weight)

  summary <- summarize_words(get(dataset), measure)
  colnames(summary)[1] <- "spelling"
  colnames(summary)[2] <- "pronunciation"

  return(summary)
}
filter_columns <- function(df, statistics) {
  col_names <- colnames(df)

  keep_cols <- sapply(col_names, function(col) {
    if (col %in% c("spelling", "pronunciation")) {
      return(TRUE) # Always keep these columns
    }
    if (grepl("\\.", col)) {
      suffix <- sub(".*\\.", "", col) # Extract part after last period
      return(suffix %in% statistics) # Exclude columns with unwanted statistics
    }
    return(TRUE) # Keep columns without a period
  })

  return(df[, keep_cols, drop = FALSE])
}
get_measures <- function(grain_sizes, weight_options, measures, statistics) {
  results_list <- list()

  i <- 1
  for (measure in measures) {
    for (grain in names(grain_sizes)) {
      for (weight in names(weight_options)) {
        print(paste0("doing iter ", i, " for dataset ", grain_sizes[[grain]], weight_options[[weight]], " and measure ", measure))

        summary_result <- extract_summary(grain_sizes[[grain]], weight_options[[weight]], measure)

        if (length(results_list) == 0) {
          # Include spelling and pronunciation columns only in the first iteration
          results_list[[paste0(grain, "_", weight, "_", measure)]] <- summary_result
        } else {
          # Subsequent summaries exclude spelling and pronunciation columns
          results_list[[paste0(grain, "_", weight, "_", measure)]] <- summary_result[, -c(1, 2)]
        }
        i <- i + 1
      }
    }
  }
  
  # Combine all summaries into a single data frame
  combined_results <- do.call(cbind, results_list)
  colnames(combined_results)[1:2] <- c("spelling", "pronunciation")
  
  combined_results <- filter_columns(combined_results, statistics)

  return(combined_results)
}


# These three functions will be adapted to a "map words" pipeline in the toolkit guide 
batch_map_words <- function(spellings, pronunciations, grain_sizes, name) {
    dataset_names <- c()

    for (grain in names(grain_sizes)) {
        # Name of the mapped words dataset to be stored
        var_name <- paste0(name, grain_sizes[[grain]])

        # Call the relevant map function at the desired level
        data <- get(paste0("map", grain_sizes[[grain]]))(spellings, pronunciations, FALSE)

        # Save the dataset to the environment
        assign(var_name, data, envir = .GlobalEnv)

        # Add name of generated dataset to list
        dataset_names <- c(dataset_names, var_name)
    }

    # Return a list of names of generated datasets for easy removal from the environment later
    return(dataset_names)
}
batch_make_tables <- function(mapped_words_name, grain_sizes, weight_options, weight_data, table_name) {
    dataset_names <- c()

    for (grain in names(grain_sizes)) {
        mapped_words <- get(paste0(mapped_words_name, grain_sizes[[grain]]))
        for (weight in names(weight_options)) {
            # This is bad right now but works
            var_name <- paste0(table_name, grain_sizes[[grain]], weight_options[[weight]])
            data <- c() # init empty

            if (weight == "default") {
                data <- make_tables(mapped_words)
            } else if (weight == "noposition") {
                data <- make_tables(mapped_words, positional = FALSE)
            } else if (weight == "freq") {
                data <- make_tables(mapped_words, weight = weight_data)
            } else if (weight == "freq_noposition") {
                data <- make_tables(mapped_words, weight = weight_data, positional = FALSE)
            }
            assign(var_name, data, envir = .GlobalEnv)

            dataset_names <- c(dataset_names, var_name)
        }
    }

    return(dataset_names)
}
batch_map_values <- function(spellings, pronunciations, table_name, grain_sizes, weight_options, mapped_value_name) {
    dataset_names <- c()
    
    for (grain in names(grain_sizes)) {
        for (weight in names(weight_options)) {
            data_table <- get(paste0(table_name, grain_sizes[[grain]], weight_options[[weight]]))
            var_name <- paste0(mapped_value_name, grain_sizes[[grain]], weight_options[[weight]])

            data <- map_value(spellings, pronunciations, grain, data_table)

            assign(var_name, data, envir = .GlobalEnv)

            dataset_names <- c(dataset_names, var_name)
        }
    }

    return(dataset_names)
}


# Gets all of the toolkit's measures for a set of spellings and corresponding pronunciations
get_measures_from_bin <- function(mapped_value_dataset_name, bin) {
    # Need to edit this list if not using all 4, same goes for below lists
    # This will be abstracted to inputs to the function in the future + for toolkit guide + repo
    grain_sizes <- list(
        "PG" = paste0(mapped_value_dataset_name, "_PG"), 
        "ONC" = paste0(mapped_value_dataset_name, "_ONC"), 
        "OC" = paste0(mapped_value_dataset_name, "_OC"), 
        "OR" = paste0(mapped_value_dataset_name, "_OR")
    )
    weight_options <- list(
        "default" = "", 
        "noposition" = "_noposition", 
        "freq" = "_freq", 
        "freq_noposition" = "_freq_noposition"
    )
    measures <- c("PG", "GP", "PG_freq", "G_freq", "P_freq")
    statistics <- c("mean", "median", "max", "min", "sd")

    measures_for_bin <- get_measures(grain_sizes, weight_options, measures, statistics)
    dataset_name <- paste0("bin_", bin, "_data.csv")
    write.csv(measures_for_bin, dataset_name)
    print(paste0("Wrote dataset for bin ", bin, " to file ", dataset_name))
}


# Iterate through all age bins
for (ub in 1:24) {
    grain_sizes <- list(
        "PG" = "_PG", 
        "ONC" = "_ONC", 
        "OC" = "_OC", 
        "OR" = "_OR"
    )
    weight_options <- list(
        "default" = "", 
        "noposition" = "_noposition", 
        "freq" = "_freq", 
        "freq_noposition" = "_freq_noposition"
    )
    print(ub)
    subset_df <- subset_by_AoA_range(df, 0, ub)
    if (nrow(subset_df) == 0) {
        print("is 0")
        next
    }

    # Get the Toolkit's pronunciations for each word
    age_bin_df <- subset_df %>% select(word)
    age_bin_df <- merge(age_bin_df, wordlist_v2_0[, c("spelling", "pronunciation", "freq")], by.x = "word", by.y = "spelling", all.x = TRUE)
    age_bin_df <- unique(age_bin_df)

    mapped_words_dataset_name <- paste0("mwords_bin_", ub)
    made_tables_dataset_name <- paste0("mtables_bin_", ub)
    mapped_values_dataset_name <- paste0("mvalues_bin_", ub)

    # Process them as one lexicon and save the result
    # MAPPING AT ALL LEVELS - see map_words.R file

    s <- age_bin_df$word
    p <- age_bin_df$pronunciation
    f <- age_bin_df$freq

    mapped_word_datasets <- batch_map_words(s, p, grain_sizes, mapped_words_dataset_name)
    print(mapped_word_datasets)
    table_datasets <- batch_make_tables(mapped_words_dataset_name, grain_sizes, weight_options, f, made_tables_dataset_name)
    print(table_datasets)
    mapped_value_datasets <- batch_map_values(s, p, made_tables_dataset_name, grain_sizes, weight_options, mapped_values_dataset_name)
    print(mapped_value_datasets)

    get_measures_from_bin(mapped_values_dataset_name, ub)

    # need to say list= for these
    rm(list=mapped_word_datasets)
    rm(list=table_datasets)
    rm(list=mapped_value_datasets)
    print(paste0("Completed run for bin ", ub))
}