library(tidyverse)
library(baseballr)
library(data.table)
library(xgboost)
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
    mutate(release_pos_y = 60.5 - release_extension) %>%
    mutate(pitch_id_raw = paste0(game_pk, "_", batter, "_", pitcher, "_", pitch_number, "_", at_bat_number, "_", inning)) %>%
    distinct()

savant_2022 <- data_2022 %>%
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

savant_2023 <- data_2023 %>%
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
    select(pitcher, player_name, pitch_id_raw, pitch_type_condensed, p_throws, 
    vx0, vy0, vz0, ay, ax, az, spin_axis, release_pos_y, release_speed, release_pos_z,
    vx0, vy0, vz0, ay, ax, az, spin_axis, release_pos_y, release_speed, 
    release_pos_x, pfx_z, pfx_x, release_spin_rate, effective_speed) %>%
    filter(pitch_type_condensed %in% c("FF", "SI", "CT")) %>%
    filter(!is.na(spin_axis) & !is.na(release_speed))

savant_clean_2022 <- savant_2022 %>%
    select(pitcher, player_name, pitch_id_raw, pitch_type_condensed, p_throws,
    vx0, vy0, vz0, ay, ax, az, spin_axis, release_pos_y, release_speed, release_pos_z,
    vx0, vy0, vz0, ay, ax, az, spin_axis, release_pos_y, release_speed, 
    release_pos_x, pfx_z, pfx_x, release_spin_rate, effective_speed) %>%
    filter(pitch_type_condensed %in% c("FF", "SI", "CT")) %>%
    filter(!is.na(spin_axis) & !is.na(release_speed))


savant_clean_2023 <- savant_2023 %>%
    select(pitcher, player_name, pitch_id_raw, pitch_type_condensed, p_throws, 
    vx0, vy0, vz0, ay, ax, az, spin_axis, release_pos_y, release_speed, release_pos_z,
    release_pos_x, pfx_z, pfx_x, release_spin_rate, effective_speed) %>%
    filter(pitch_type_condensed %in% c("FF", "SI", "CT")) %>%
    filter(!is.na(spin_axis) & !is.na(release_speed))

arm_slot <- savant_clean %>%
left_join(height_df, by = c("pitcher" = "id")) %>%
  mutate(
    height_ratio = release_pos_z / height_numeric,
    pfx_x_cor = if_else(p_throws == 'L', pfx_x * -1, pfx_x),
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
  select(-pfx_x, -p_throws, -mean_height_ratio, -sd_height_ratio) 

arm_slot_2022 <- savant_clean_2023 %>%
left_join(height_df, by = c("pitcher" = "id")) %>%
  mutate(
    height_ratio = release_pos_z / height_numeric,
    pfx_x_cor = if_else(p_throws == 'L', pfx_x * -1, pfx_x),
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
  select(-pfx_x, -p_throws, -mean_height_ratio, -sd_height_ratio) 

arm_slot_2023 <- savant_clean_2023 %>%
left_join(height_df, by = c("pitcher" = "id")) %>%
  mutate(
    height_ratio = release_pos_z / height_numeric,
    pfx_x_cor = if_else(p_throws == 'L', pfx_x * -1, pfx_x),
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
  select(-pfx_x, -p_throws, -mean_height_ratio, -sd_height_ratio) 

armslots <- rbind(arm_slot_2022, arm_slot_2023)

# Separate the metadata that you want to keep for the leaderboard
arm_slot_metadata <- arm_slot %>%
  select(pitcher, player_name, pitch_id_raw, pitch_type_condensed)

arm_slot_metadata_2023 <- armslots %>%
  select(pitcher, player_name, pitch_id_raw, pitch_type_condensed)

# Separate the data that will be used for training the model
arm_slot_features <- arm_slot %>%
  select(-pitcher, -player_name, -pitch_id_raw, -pitch_type_condensed)

arm_slot_features_2023 <- armslots %>%
  select(-pitcher, -player_name, -pitch_id_raw, -pitch_type_condensed)


# Create indices for the training set
set.seed(42) # for reproducibility

# Create the training and test sets
train_data <- arm_slot_features
train_metadata <- arm_slot_metadata
test_data <- arm_slot_features_2023
test_metadata <- arm_slot_metadata_2023

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

# Make predictions
predictions <- predict(xgb_model, as.matrix(test_data %>% select(-pfx_z)))
test_metadata <- bind_cols(test_metadata, data.frame(pfx_z_predictions = predictions))

# Calculate the RMSE
rmse <- sqrt(mean((predictions - test_data$pfx_z)^2))

# Create a data frame with the predictions and the actual values
predictions_df <- data.frame(predictions, test_data$pfx_z)

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
test_predictions <- predict(xgb_model, as.matrix(test_data %>% select(-pfx_z)))

# Combine the predictions with the metadata
test_data_with_predictions <- bind_cols(test_metadata, test_data, data.frame(pfx_z_predicted = test_predictions))

big_data <- test_data_with_predictions %>%
    mutate(
        pfx_z_diff = pfx_z - pfx_z_predicted,
        pfx_z_diff_abs = abs(pfx_z)
    ) %>%
    select(pitch_id_raw, pfx_z, pfx_z_predicted, pfx_z_diff, pfx_z_diff_abs)

# Specify file paths for saving CSV files
filepath <- "Y:/departments/research_and_development/baseball_operations/clayton_goodiez/csv/"
filename <- "2022&3_pfx_z_predictions.csv"

# Save the data frame as a CSV file
write_csv(big_data, paste0(filepath, filename))