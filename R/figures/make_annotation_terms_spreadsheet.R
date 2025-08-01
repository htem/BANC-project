# load libraries
library(bancr)
library(readr)
library(dplyr)
library(stringr)
library(purrr)
library(tidyr)
library(bit64)
library(openxlsx)

version <- 626

codex_annotations_flat_table <- banc_codex_annotations(live = FALSE, version = version)

# Get unique terms for each column, splitting comma-separated values
selected_columns <- codex_annotations_flat_table %>%
  select(flow, super_class, cell_class, cell_sub_class, region, side, 
         cell_function, cell_function_detailed, peripheral_target_type, 
         body_part_sensory, body_part_effector, nerve, hemilineage, 
         neurotransmitter_verified, neuropeptide_verified, neurotransmitter_predicted)

# Function to split comma-separated values and get unique terms
get_unique_split_terms <- function(column) {
  # Remove NA and empty strings first
  clean_values <- column[!is.na(column) & column != ""]
  
  # Split by comma and flatten
  split_values <- unlist(str_split(clean_values, ","))
  
  # Trim whitespace and remove empty strings again
  trimmed_values <- str_trim(split_values)
  final_values <- trimmed_values[trimmed_values != ""]
  
  # Return sorted unique values
  sort(unique(final_values))
}

# Apply the function to each column
unique_terms_list <- selected_columns %>%
  map(get_unique_split_terms)

# Find the maximum length to pad shorter columns
max_length <- max(map_int(unique_terms_list, length))

# Pad shorter columns with NA to make them all the same length
annotation_terms <- unique_terms_list %>%
  map(~ c(.x, rep(NA, max_length - length(.x)))) %>%
  as_tibble()

# Write to Excel file
annotation_terms_filepath <- "/Users/papers/BANC-project/figures/annotation_terms_list.xlsx"
write.xlsx(annotation_terms, annotation_terms_filepath, rowNames = FALSE)

cat("Excel file saved to:", annotation_terms_filepath)