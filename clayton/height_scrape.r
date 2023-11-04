library(tidyverse)
library(baseballr)
library(data.table)
library(xgboost)
# Load data
data <- read_csv("Y:/departments/research_and_development/baseball_operations/clayton_goodiez/csv/2021_data.csv")
data_2022 <- read_csv("Y:/departments/research_and_development/baseball_operations/clayton_goodiez/csv/2022_data.csv")
data_2023 <- read_csv("Y:/departments/research_and_development/baseball_operations/clayton_goodiez/csv/2023_MLB_Seaspm.csv")

# Convert the vectors to data frames if they are not already
pitcher_df1 <- data.frame(name = savant_clean$pitcher)
pitcher_df2 <- data.frame(name = savant_clean_2023$pitcher)
pitcher_df3 <- data.frame(name = savant_clean_2022$pitcher)

bind_rows(pitcher_df1, pitcher_df2, pitcher_df3) -> all_names

# Create a list of unique names
unique_names = unique(all_names$name)
results <- list()
# Loop through each unique name (assuming they are MLBAMIDs) and retrieve player information
for (i in seq_along(unique_names)) {
  player_info <- mlb_people(person_ids = unique_names[i])
  results[[i]] <- player_info
  print(player_info)
}


# Use rbind.fill to combine the list of data frames
combined_results <- rbindlist(results, fill = TRUE)

height = combined_results %>%
select(id, height)

convert_height_to_decimal <- function(height_str) {
  # Split the string on the apostrophe and space ' '
  parts <- strsplit(height_str, "'\\s*")[[1]]
  
  # Extract feet and inches parts
  feet <- as.numeric(parts[1])
  inches <- as.numeric(sub("\"", "", parts[2])) # Remove the inch symbol and convert to numeric
  
  # Convert inches to a fraction of a foot and add to feet
  total_height <- feet + (inches / 12)
  
  return(total_height)
}

height$height_numeric <- round(sapply(height$height, convert_height_to_decimal), 2)


result_df <- height %>%
  select(id, height_numeric)

result_df <- as.data.frame(result_df)


write.csv(result_df, "Y:/departments/research_and_development/baseball_operations/clayton_goodiez/csv/player_heights.csv")