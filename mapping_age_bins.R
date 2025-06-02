library(dplyr)

csv_files <- list.files(
  path        = "~/Documents/NTR Project/Developmental Project/age_bin_data",    # change to your folder
  pattern     = "\\.csv$",                # only .csv files
  full.names  = TRUE                      # get absolute paths
)



# RETURNS PHONEME,GRAPHEME,POSITION PACKETS 
extract_triplet <- function(spelling, pronunciation){
  segmat <- map_value(spelling, pronunciation, "PG")[[1]]
  
  # pull out just the three rows we care about
  sub <- segmat[c("phoneme", "grapheme", "position"), , drop = FALSE]
  
  # transpose so that each segment is a row
  df <- as.data.frame(t(sub), stringsAsFactors = FALSE, check.names = FALSE)
  
  # name the columns
  colnames(df) <- c("phoneme", "grapheme", "position")
  
  rownames(df) <- NULL
  df
}


age_bin_mappings <- function(df, bin, template) {
  # df: your input with spelling in col 2, inhouse pron in col 3
  # template: master table with cols “grapheme”, “phoneme”, plus position cols (“wi”, “si”, …) all numeric
  
  # 1) outer loop over every row of df
  for (i in seq_len(nrow(df))) {
    spelling    <- df[i, 2]
    pron_inhouse<- df[i, 3]
    word_map    <- extract_triplet(spelling, pron_inhouse)  # returns a data.frame
    
    # 2) inner loop over each segment
    for (j in seq_len(nrow(word_map))) {
      g   <- word_map$grapheme[j]
      p   <- inhouse_to_ipa(word_map$phoneme[j])
      pos <- word_map$position[j]
      
      # 3) find matching row in template
      row_idx <- which(
        template$grapheme == g &
          template$phoneme  == p
      )
      # 4) find matching column index
      col_idx <- match(pos, names(template))
      
      # 5) only if exactly one row & a valid column, increment & print
      template[row_idx, col_idx] <- template[row_idx, col_idx] + 1
    }
  }
  dataset_name <- paste0("mapping_bin_", bin, "_data.csv")
  write.csv(template, dataset_name)
  print(paste0("Wrote dataset for bin ", bin, " to file ", dataset_name))
}




for (ub in 2:24) {
  filename <- paste0("/Users/victorbhattacharjee/Documents/NTR Project/Developmental Project/age_bin_data/bin_", ub)
  filename <- paste0(filename, "_data.csv")
  df <- read.csv(filename, stringsAsFactors = FALSE)
  age_bin_mappings(df, ub, PtoG_table)
  print("Done with ")
  print(ub)
}

for (ub in 2:24) {
  filename <- paste0("/Users/victorbhattacharjee/Documents/NTR Project/Developmental Project/mapping_bin_", ub)
  filename <- paste0(filename, "_data.csv")
  df <- read.csv(filename, stringsAsFactors = FALSE)
  calculate_frequency(df, ub)
  print("Done with ")
  print(ub)
}

calculate_frequency <- function(df, bin){
  df$total = df$wi+df$si+df$sm+df$sf+df$wf
  
  df$wi_freq = log10(df$wi)
  df$si_freq = log10(df$si)
  df$sm_freq = log10(df$sm)
  df$sf_freq = log10(df$sf)
  df$wf_freq = log10(df$wf)
  df$freq = log10(df$total)
  
  dataset_name <- paste0("frequency_mapping_bin_", bin, "_data.csv")
  write.csv(df, dataset_name)
}
