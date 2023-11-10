library(tidyverse)
library(baseballr)
library(data.table)
library(xgboost)
library(caret)
# Load data
data <- read_csv("Y:/departments/research_and_development/baseball_operations/clayton_goodiez/csv/2021_data.csv")
data_2022 <- read_csv("Y:/departments/research_and_development/baseball_operations/clayton_goodiez/csv/2022_data.csv")
data_2023 <- read_csv("Y:/departments/research_and_development/baseball_operations/clayton_goodiez/csv/2023_MLB_Seaspm.csv")
height_df <- read_csv("Y:/departments/research_and_development/baseball_operations/clayton_goodiez/csv/player_heights.csv")

savant <- data %>% 
    mutate(pitch_type_condensed = ifelse(pitch_type == "FF", "FF", 
    ifelse(pitch_type == "FT", "SI",  
    ifelse(pitch_type == "FC", "CT", 
    ifelse(pitch_type == "SL" | pitch_type == "ST" | pitch_type == "SV", "SL", 
    ifelse(pitch_type == "CH" | pitch_type == "FS", "CH", 
    ifelse(pitch_type == "CU" | pitch_type == "KC" | pitch_type == "CS", "CB", 
    ifelse(pitch_type == "SI", "SI", ifelse(pitch_type == "KN", "KN", "FAHCK"))))))))) %>%
    mutate(release_pos_y = 60.5 - release_extension,
    swing = ifelse(description == "swinging_strike" | description == "swinging_strike_blocked" | 
                                                         description == "missed_bunt" | description == "swinging_pitchout" | 
                                                         description == "foul_tip"| description == "foul_bunt"| description == "foul" | 
                                                         description == "foul_pitchout" | description == "hit_into_play" | description == "hit_into_play_no_out" | 
                                                         description == "hit_into_play_score" | description == "pitchout_hit_into_play_score", 1, 0),
            whiff = ifelse(description == 'swinging_strike' | description == 'swinging_strike_blocked' | description == 'missed_bunt', 1, 0)) %>%
    mutate(pitch_id_raw = paste0(game_pk, "_", batter, "_", pitcher, "_", pitch_number, "_", at_bat_number, "_", inning )) %>%
    distinct()

savant_2022 <- data_2022 %>%
    mutate(pitch_type_condensed = ifelse(pitch_type == "FF", "FF", 
    ifelse(pitch_type == "FT", "SI",  
    ifelse(pitch_type == "FC", "CT", 
    ifelse(pitch_type == "SL" | pitch_type == "ST" | pitch_type == "SV", "SL", 
    ifelse(pitch_type == "CH" | pitch_type == "FS", "CH", 
    ifelse(pitch_type == "CU" | pitch_type == "KC" | pitch_type == "CS", "CB", 
    ifelse(pitch_type == "SI", "SI", ifelse(pitch_type == "KN", "KN", "FAHCK"))))))))) %>%
    mutate(release_pos_y = 60.5 - release_extension,
    swing = ifelse(description == "swinging_strike" | description == "swinging_strike_blocked" | 
                                                         description == "missed_bunt" | description == "swinging_pitchout" | 
                                                         description == "foul_tip"| description == "foul_bunt"| description == "foul" | 
                                                         description == "foul_pitchout" | description == "hit_into_play" | description == "hit_into_play_no_out" | 
                                                         description == "hit_into_play_score" | description == "pitchout_hit_into_play_score", 1, 0),
            whiff = ifelse(description == 'swinging_strike' | description == 'swinging_strike_blocked' | description == 'missed_bunt', 1, 0)) %>%
    mutate(pitch_id_raw = paste0(game_pk, "_", batter, "_", pitcher, "_", pitch_number, "_", at_bat_number, "_", inning)) %>%
    distinct()

savant_2023 <- data_2023 %>%
    mutate(pitch_type_condensed = ifelse(pitch_type == "FF", "FF", 
    ifelse(pitch_type == "FT", "SI",  
    ifelse(pitch_type == "FC", "CT", 
    ifelse(pitch_type == "SL" | pitch_type == "ST" | pitch_type == "SV", "SL", 
    ifelse(pitch_type == "CH" | pitch_type == "FS", "CH", 
    ifelse(pitch_type == "CU" | pitch_type == "KC" | pitch_type == "CS", "CB", 
    ifelse(pitch_type == "SI", "SI", ifelse(pitch_type == "KN", "KN", "FAHCK"))))))))) %>%
    mutate(release_pos_y = 60.5 - release_extension,
    swing = ifelse(description == "swinging_strike" | description == "swinging_strike_blocked" | 
                                                         description == "missed_bunt" | description == "swinging_pitchout" | 
                                                         description == "foul_tip"| description == "foul_bunt"| description == "foul" | 
                                                         description == "foul_pitchout" | description == "hit_into_play" | description == "hit_into_play_no_out" | 
                                                         description == "hit_into_play_score" | description == "pitchout_hit_into_play_score", 1, 0),
            whiff = ifelse(description == 'swinging_strike' | description == 'swinging_strike_blocked' | description == 'missed_bunt', 1, 0)) %>%
    mutate(pitch_id_raw = paste0(game_pk, "_", batter, "_", pitcher, "_", pitch_number, "_", at_bat_number, "_", inning)) %>%
    distinct()

savant_clean  <- savant %>%
    filter(pitch_type_condensed %in% c("FF", "SI", "CT") & (swing = 1)) %>%
    select(pitcher, player_name, pitch_id_raw, pitch_type_condensed, p_throws,
    vx0, vy0, vz0, ay, ax, az, spin_axis, release_pos_y, release_speed, release_pos_z, whiff,
    vx0, vy0, vz0, ay, ax, az, spin_axis, release_pos_y, release_speed, 
    release_pos_x, pfx_z, pfx_x, release_spin_rate, effective_speed)

savant_clean_2022 <- savant_2022 %>%
    filter(pitch_type_condensed %in% c("FF", "SI", "CT")) %>%
    select(pitcher, player_name, pitch_id_raw, pitch_type_condensed, p_throws,
    vx0, vy0, vz0, ay, ax, az, spin_axis, release_pos_y, release_speed, release_pos_z, whiff,
    vx0, vy0, vz0, ay, ax, az, spin_axis, release_pos_y, release_speed, 
    release_pos_x, pfx_z, pfx_x, release_spin_rate, effective_speed)

savant_clean_2023 <- savant_2023 %>%
    filter(pitch_type_condensed %in% c("FF", "SI", "CT")) %>%
    select(pitcher, player_name, pitch_id_raw, pitch_type_condensed, p_throws,
    vx0, vy0, vz0, ay, ax, az, spin_axis, release_pos_y, release_speed, release_pos_z, whiff, 
    vx0, vy0, vz0, ay, ax, az, spin_axis, release_pos_y, release_speed, 
    release_pos_x, pfx_z, pfx_x, release_spin_rate, effective_speed)

arm_slot <- savant_clean %>%
left_join(height_df, by = c("pitcher" = "id")) %>%
  mutate(
    height_ratio = release_pos_z / height_numeric,
    # Calculate mean and standard deviation of height_ratio
    mean_height_ratio = mean(height_ratio, na.rm = TRUE),
    sd_height_ratio = sd(height_ratio, na.rm = TRUE),
    # Categorize arm_slot based on standard deviation from the mean
    arm_slot = case_when(
      height_ratio < (mean_height_ratio - sd_height_ratio) ~ 0,
      between(height_ratio, mean_height_ratio - sd_height_ratio, mean_height_ratio + sd_height_ratio) ~ 2,
      height_ratio > (mean_height_ratio + sd_height_ratio) ~ 3,
      TRUE ~ 1
    )
  ) %>%
  select(-p_throws, -mean_height_ratio, -sd_height_ratio, -`...1`, -ay, -vz0,
  -height_numeric, -effective_speed, -release_pos_y) 

arm_slot_2022 <- savant_clean_2022 %>%
left_join(height_df, by = c("pitcher" = "id")) %>%
  mutate(
    height_ratio = release_pos_z / height_numeric,
    # Calculate mean and standard deviation of height_ratio
    mean_height_ratio = mean(height_ratio, na.rm = TRUE),
    sd_height_ratio = sd(height_ratio, na.rm = TRUE),
    # Categorize arm_slot based on standard deviation from the mean
    arm_slot = case_when(
      height_ratio < (mean_height_ratio - sd_height_ratio) ~ 0,
      between(height_ratio, mean_height_ratio - sd_height_ratio, mean_height_ratio + sd_height_ratio) ~ 2,
      height_ratio > (mean_height_ratio + sd_height_ratio) ~ 3,
      TRUE ~ 1
    )
  ) %>%
  select(-p_throws, -mean_height_ratio, -sd_height_ratio, -`...1`, -ay, -vz0,
  -height_numeric, -effective_speed, -release_pos_y)

arm_slot_2023 <- savant_clean_2023 %>%
left_join(height_df, by = c("pitcher" = "id")) %>%
  mutate(
    height_ratio = release_pos_z / height_numeric,
    # Calculate mean and standard deviation of height_ratio
    mean_height_ratio = mean(height_ratio, na.rm = TRUE),
    sd_height_ratio = sd(height_ratio, na.rm = TRUE),
    # Categorize arm_slot based on standard deviation from the mean
    arm_slot = case_when(
      height_ratio < (mean_height_ratio - sd_height_ratio) ~ 0,
      between(height_ratio, mean_height_ratio - sd_height_ratio, mean_height_ratio + sd_height_ratio) ~ 2,
      height_ratio > (mean_height_ratio + sd_height_ratio) ~ 3,
      TRUE ~ 1
    )
  ) %>%
  select(-p_throws, -mean_height_ratio, -sd_height_ratio, -`...1`, -ay, -vz0,
  -height_numeric, -effective_speed, -release_pos_y)

combined_years <- rbind(arm_slot_2022, arm_slot_2023)

# Separate the metadata that you want to keep for the leaderboard
arm_slot_metadata <- arm_slot %>%
    na.omit() %>%
  select(pitcher, player_name, pitch_id_raw, pitch_type_condensed)

arm_slot_metadata_2023 <- combined_years %>%
  na.omit() %>%
  select(pitcher, player_name, pitch_id_raw, pitch_type_condensed)

# Separate the data that will be used for training the model
arm_slot_features <- arm_slot %>%
  na.omit() %>%
  select(-pitcher, -player_name, -pitch_id_raw, -pitch_type_condensed)

arm_slot_features_2023 <- combined_years %>%
  na.omit() %>%
  select(-pitcher, -player_name, -pitch_id_raw, -pitch_type_condensed) 

# Create indices for the training set
set.seed(42) # for reproducibility

# Create the training and test sets
train_data <- arm_slot_features
train_metadata <- arm_slot_metadata
test_data <- arm_slot_features_2023
test_metadata <- arm_slot_metadata_2023
# Prepare matrices for XGBoost
train_matrix <- xgb.DMatrix(data = as.matrix(train_data %>% select(-whiff)), 
                            label = train_data$whiff)
test_matrix <- xgb.DMatrix(data = as.matrix(test_data %>% select(-whiff)), 
                           label = test_data$whiff)

# Define parameters
params <- list(
    eta = .1,
    max_depth = 9,
    gamma = 0,
    colsample_bytree = .8,
    min_child_weight = 1,
    objective = "reg:linear" # For regression objectives
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

# Make predictions
predictions <- predict(xgb_model, as.matrix(test_data %>% select(-whiff)))
test_metadata <- bind_cols(test_metadata, data.frame(xwhiff = predictions))

# Calculate the RMSE
rmse <- sqrt(mean((predictions - test_data$whiff)^2))

# Create a data frame with the predictions and the actual values
predictions_df <- data.frame(predictions, test_data$whiff)

# Rename the columns
colnames(predictions_df) <- c("predictions", "actual")

# Plot the predictions vs. the actual values
predictions_df %>%
  ggplot(aes(x = actual, y = predictions)) +
  geom_point(alpha = .06, size = 4) +
  geom_smooth(method = "lm", se = TRUE, linewidth = 2) + 
  labs(x = "Actual", y = "Predicted", title = "Predicted vs. Actual Spin Direction") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 30, face = "bold"),
    axis.title = element_text(size = 28),
    axis.text = element_text(size = 24)
  )
# Make predictions
test_predictions <- predict(xgb_model, as.matrix(test_data %>% select(-whiff)))

# Combine the predictions with the metadata
test_data_with_predictions <- bind_cols(test_metadata, test_data, data.frame(whiff_predicted = test_predictions))

# Specify file paths for saving CSV files
filepath <- "Y:/departments/research_and_development/baseball_operations/clayton_goodiez/csv/"
filename <- "2022&3_whiff_predictions.csv"

# Save the data frame as a CSV file
write_csv(test_data_with_predictions, paste0(filepath, filename))
