library(dplyr)
library(readr)
library(tidyr)

# Calculate consistency for a single frequency mapping file
calculate_consistency <- function(file) {
  # Read the frequency mapping data
  data <- read_csv(file, show_col_types = FALSE)
  
  positions <- c("wi", "si", "sm", "sf", "wf")
  
  # Calculate consistency for each position
  position_consistencies <- lapply(positions, function(pos) {
    # Calculate total frequency for each phoneme
    phoneme_metrics <- data %>%
      group_by(phoneme) %>%
      summarize(
        total_freq = sum(!!sym(pos)),
        .groups = "drop"
      )
    
    # Calculate consistency
    data %>%
      left_join(phoneme_metrics, by = "phoneme") %>%
      mutate(
        consistency = if_else(total_freq > 0, !!sym(pos)/total_freq, 0)
      ) %>%
      select(phoneme, grapheme, consistency) %>%
      rename(!!paste0(pos, "_consistency") := consistency)
  })
  
  # Combine all position metrics
  result <- Reduce(function(x, y) {
    left_join(x, y, by = c("phoneme", "grapheme"))
  }, position_consistencies)
  
  # Calculate average consistency across positions
  result <- result %>%
    mutate(
      consistency = rowMeans(select(., ends_with("_consistency")), na.rm = TRUE)
    ) %>%
    mutate(`PG mapping` = paste(phoneme, grapheme, sep = "-"))
  
  # Join with original frequency data and arrange
  final_result <- data %>%
    mutate(`PG mapping` = paste(phoneme, grapheme, sep = "-")) %>%
    select(`PG mapping`, wi, si, sm, sf, wf) %>%
    left_join(
      result %>% select(
        `PG mapping`, 
        consistency,
        ends_with("_consistency")
      ),
      by = "PG mapping"
    ) %>%
    arrange(`PG mapping`)
  
  return(final_result)
}

dir.create("consistency_mapping_bin_data", showWarnings = FALSE)

files <- list.files("frequency_mapping_bin_data", pattern = "frequency_mapping_bin_.*\\.csv$", full.names = TRUE)


for (file in files) {
  bin_num <- gsub(".*bin_(\\d+)_data\\.csv$", "\\1", file)
  
  # Calculate consistency
  consistency_data <- calculate_consistency(file)
  
  output_file <- file.path("consistency_mapping_bin_data", paste0("consistency_bin_", bin_num, "_data.csv"))
  write_csv(consistency_data, output_file)
  
  cat("Processed bin", bin_num, "\n")
}

# Create a summary across bins
all_bins_data <- list()
for (file in list.files("consistency_mapping_bin_data", pattern = "consistency_bin_.*\\.csv$", full.names = TRUE)) {
  bin_num <- as.numeric(gsub(".*bin_(\\d+)_data\\.csv$", "\\1", file))  # Extract and convert to numeric
  data <- read_csv(file, show_col_types = FALSE) %>%
    select(`PG mapping`, consistency)  # Only select PG mapping and consistency
  names(data)[2] <- paste0("consistency_bin_", bin_num)  # Rename only consistency column
  all_bins_data[[as.character(bin_num)]] <- data  # Use bin number as key
}

# Get sorted bin numbers and combine data in numerical order
bin_numbers <- as.numeric(names(all_bins_data))
sorted_bin_numbers <- sort(bin_numbers)

# Combine all bins in sorted order
development_summary <- Reduce(function(x, y) {
  left_join(x, all_bins_data[[as.character(y)]], by = "PG mapping")
}, sorted_bin_numbers[-1], init = all_bins_data[[as.character(sorted_bin_numbers[1])]])

# Save development summary
write_csv(development_summary, "consistency_development_summary.csv")

cat("Processing complete. Files saved in consistency_mapping_bin_data/\n")
cat("Development summary saved as consistency_development_summary.csv\n") 