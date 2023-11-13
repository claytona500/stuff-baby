import pandas as pd

def process_and_save_files(file_paths, height_df_path, merge_on, height_column, ratio_column):
    # Load the height dataframe
    height_df = pd.read_csv(height_df_path)

    for file_path in file_paths:
        # Load the data
        data = pd.read_csv(file_path)

        # Merge with height_df
        data = data.merge(height_df[['id', height_column]], how='left', left_on=merge_on, right_on='id')

        # Calculate height_ratio
        data['height_ratio'] = data[ratio_column] / data[height_column]

        # Save back to original location
        data.to_csv(file_path, index=False)

        # Clear the dataframe from memory
        del data

# Define file paths
file_paths = [
    "Y:/departments/research_and_development/baseball_operations/clayton_goodiez/csv/2021_data.csv",
    "Y:/departments/research_and_development/baseball_operations/clayton_goodiez/csv/2022_data.csv",
    "Y:/departments/research_and_development/baseball_operations/clayton_goodiez/csv/2023_MLB_Seaspm.csv"
]

height_df_path = "Y:/departments/research_and_development/baseball_operations/clayton_goodiez/csv/player_heights.csv"

# Call the function
process_and_save_files(file_paths, height_df_path, 'pitcher', 'height_numeric', 'release_pos_z')