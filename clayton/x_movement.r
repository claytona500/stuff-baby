library(tidyverse)
library(baseballr)
library(data.table)
library(xgboost)
# Load data
data <- read_csv("Y:/departments/research_and_development/baseball_operations/clayton_goodiez/csv/2022_data.csv")

savant <- data %>% 
    mutate(pitch_type_condensed = ifelse(pitch_type == "FF", "FF", 
    ifelse(pitch_type == "FT", "SI",  
    ifelse(pitch_type == "FC", "CT", 
    ifelse(pitch_type == "SL" | pitch_type == "ST" | pitch_type == "SV", "SL", 
    ifelse(pitch_type == "CH" | pitch_type == "FS", "CH", 
    ifelse(pitch_type == "CU" | pitch_type == "KC" | pitch_type == "CS", "CB", 
    ifelse(pitch_type == "SI", "SI", ifelse(pitch_type == "KN", "KN", "FAHCK"))))))))) %>%
    mutate(release_pos_y = 60.5 - release_extension) %>%
    mutate(pitch_id_raw = paste0(game_pk, "_", batter, "_", pitcher, "_", pitch_number, "_", at_bat_number, "_", inning)) %>%
    distinct()
    
savant_clean  <- savant %>%
    select(pitcher, player_name, pitch_id_raw, pitch_type_condensed, p_throws, spin_dir, 
    vx0, vy0, vz0, ay, ax, az, spin_axis, release_pos_y, release_speed,
    release_pos_x, pfx_z, pfx_x, release_spin_rate, effective_speed) %>%
    filter(pitch_type_condensed %in% c("FF", "SI", "CT"))

names = savant_clean$pitcher

# Create a list of unique names
unique_names = unique(names)

# create a vector of length equal to the number of unique names
# this will be used to store the results
results = vector("list", length(unique_names))

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

arm_slot <- savant_clean %>%
  filter(!is.na(release_speed)) %>%
  left_join(result_df, by = c("pitcher" = "id")) %>%
  mutate(
    height_ratio = release_pos_x / height_numeric,
    pfx_x_cor = if_else(p_throws == 'L', pfx_x * -1, pfx_x)
  ) %>%
  select(-pfx_x, -p_throws)
# Separate the metadata that you want to keep for the leaderboard
arm_slot_metadata <- arm_slot %>%
  select(pitcher, player_name, pitch_id_raw, pitch_type_condensed)

# Separate the data that will be used for training the model
arm_slot_features <- arm_slot %>%
  select(-pitcher, -player_name, -pitch_id_raw, -pitch_type_condensed)

# Create indices for the training set
set.seed(42) # for reproducibility
train_indices <- sample(nrow(arm_slot_features), size = 0.75 * nrow(arm_slot_features))

# Create the training and test sets
train_data <- arm_slot_features[train_indices, ]
train_metadata <- arm_slot_metadata[train_indices, ]
test_data <- arm_slot_features[-train_indices, ]
test_metadata <- arm_slot_metadata[-train_indices, ]

# Prepare matrices for XGBoost
train_matrix <- xgb.DMatrix(data = as.matrix(train_data %>% select(-pfx_z)), 
                            label = train_data$pfx_z)
test_matrix <- xgb.DMatrix(data = as.matrix(test_data %>% select(-pfx_z)), 
                           label = test_data$pfx_z)

# Define parameters
params <- list(
  booster = "gbtree",
  objective = "reg:squarederror",
  eta = 0.3,
  max_depth = 6,
  min_child_weight = 1,
  subsample = 0.5,
  colsample_bytree = 0.5
)

# Train the model
xgb_model <- xgb.train(
  params = params,
  data = train_matrix,
  nrounds = 100,
  watchlist = list(eval = test_matrix, train = train_matrix),
  print_every_n = 10,
  early_stopping_rounds = 10,
  maximize = FALSE
)
