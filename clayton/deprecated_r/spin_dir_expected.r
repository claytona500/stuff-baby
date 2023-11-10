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
  select(pitcher, player_name, pitch_id_raw, pitch_type_condensed, arm_slot) %>%
  na.omit()

arm_slot_metadata_2023 <- combined_years %>%
  na.omit() %>%
  select(pitcher, player_name, pitch_id_raw, pitch_type_condensed, arm_slot)


# Separate the data that will be used for training the model
arm_slot_features <- arm_slot %>%
  na.omit() %>%
  select(-pitcher, -player_name, -pitch_id_raw, -pitch_type_condensed, -arm_slot) %>%

arm_slot_features_2023 <- combined_years %>%
  na.omit() %>%
  select(-pitcher, -player_name, -pitch_id_raw, -pitch_type_condensed, -arm_slot) 


# Create indices for the training set
set.seed(42) # for reproducibility

# Create the training and test sets
train_data <- arm_slot_features
train_metadata <- arm_slot_metadata
test_data <- arm_slot_features_2023 
test_metadata <- arm_slot_metadata_2023 

# Separate the features from the target variable for the training data
train_features <- train_data %>% select(-spin_axis) %>% as.matrix()
train_labels <- train_data$spin_axis

# Now create the xgb.DMatrix for the training data
dtrain <- xgb.DMatrix(data = train_features, label = train_labels)

# Assuming you have a similar target variable for your test set
# Separate the features from the target variable for the test data
test_features <- test_data %>% select(-spin_axis) %>% as.matrix()
test_labels <- test_data$spin_axis

# Create the xgb.DMatrix for the test data
dtest <- xgb.DMatrix(data = test_features, label = test_labels)


# Cross-validation to find the optimal number of rounds
cv_nfold <- 3
cv_nrounds <- 100
cv_early_stopping_rounds <- 10

# Define the parameter grid to search over
grid <- expand.grid(
  nrounds = c(100, 150, 200),               # Example values for the number of rounds
  eta = c(0.01, 0.05, 0.1),                 # Learning rate
  max_depth = c(3, 6, 9),                   # Depth of the tree
  gamma = c(0, 0.1, 0.2),                   # Minimum loss reduction required for further partition
  colsample_bytree = c(0.5, 0.8, 1),        # Subsample ratio of columns when constructing each tree
  min_child_weight = c(1, 3, 5)             # Minimum sum of instance weight (hessian) needed in a child
)

# Define the cross-validation method
ctrl <- trainControl(
  method = "cv",
  number = cv_nfold,                        # Number of folds in the cross-validation
  savePredictions = "final",                # Save predictions for the optimal tuning parameter
  allowParallel = TRUE                      # Allow parallel processing
)

# Train the model using the train function from caret package
xgb_model <- train(
  x = as.matrix(training_set[-target_column]), # exclude the target variable
  y = training_set[[target_column]],
  trControl = ctrl,
  tuneGrid = grid,
  method = "xgbTree"                          # This specifies the xgboost model
)

# The resulting xgb_model object will include details on the best parameters found
# and the performance of the model at each combination of parameters.

# Extract the best parameters and the best number of rounds
best_params <- xgb_model$bestTune
best_nrounds <- best_params$nrounds

#      nrounds max_depth eta gamma colsample_bytree min_child_weight subsample
#      150       9   0.1     0          0.8       1         1
# Now, you can proceed to train your final model using these best parameters
final_xgb_model <- xgb.train(
  params = list(
    eta = .1,
    max_depth = 9,
    gamma = 0,
    colsample_bytree = .8,
    min_child_weight = 1,
    objective = "reg:linear" # For regression objectives
  ),
  data = dtrain,
  nrounds = 150,
  watchlist = list(eval = dtest, train = dtrain),
  print_every_n = 10,
  early_stopping_rounds = 10,
  maximize = FALSE
)

# Make predictions on the test set
test_predictions <- predict(final_xgb_model, dtest)

# Assuming 'test_predictions' contains your model predictions
labels <- getinfo(dtest, "label")

# Calculate RMSE
rmse_test <- sqrt(mean((labels - test_predictions)^2))
print(rmse_test)
# Plot Feature Importance
# Get the feature importance matrix from the model
importance_matrix <- xgb.importance(feature_names = colnames(dtrain), model = final_xgb_model)
# Plot feature importance
ggplot(importance_matrix, aes(x = reorder(Feature, Gain), y = Gain)) +
  geom_bar(stat = "identity") +
  coord_flip() +  # Flip the coordinates to make it a horizontal bar plot
  xlab("Features") +
  ylab("Importance Score (Gain)") +
  ggtitle("Feature Importance")



# Create a data frame with the predictions and the actual values
predictions_df <- data.frame(predictions, test_data$spin_axis)
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
test_predictions <- predict(final_xgb_model, as.matrix(test_data %>% select(-spin_axis)))
train_predictions <- predict(final_xgb_model, as.matrix(train_data %>% select(-spin_axis)))

# Combine the predictions with the metadata
test_data_with_predictions <- bind_cols(test_metadata, test_data, data.frame(spin_axis_predicted = test_predictions))
train_data_with_predictions <- bind_cols(train_metadata, train_data, data.frame(spin_axis_predicted = train_predictions))

big_data <- test_data_with_predictions %>%
    mutate(
        spin_axis_diff = spin_axis - spin_axis_predicted,
        spin_axis_diff_abs = abs(spin_axis)
    )

bigger_data <- train_data_with_predictions %>%
    mutate(
        spin_axis_diff = spin_axis - spin_axis_predicted,
        spin_axis_diff_abs = abs(spin_axis)
    )

combined_data <- rbind(big_data, bigger_data)

# Specify file paths for saving CSV files
filepath <- "Y:/departments/research_and_development/baseball_operations/clayton_goodiez/csv/"
filename <- "2022&3_spin_axis_predictions.csv"

# Save the data frame as a CSV file
write_csv(big_data, paste0(filepath, filename))
