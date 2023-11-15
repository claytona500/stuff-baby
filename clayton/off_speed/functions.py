import pandas as pd
import numpy as np

def preprocess_data(df, height_df):
    """
    This function takes a DataFrame and applies several preprocessing steps to it.
    """
    df = df.drop(columns = ['spin_rate_deprecated', 'break_angle_deprecated', 'break_length_deprecated', 
    'game_type', 'home_team', 'away_team', 'type', 'bb_type', 'inning_topbot', 
    'hc_x', 'hc_y', 'tfs_deprecated', 'tfs_zulu_deprecated', 'hit_distance_sc', 
    'launch_speed', 'launch_angle', 'estimated_ba_using_speedangle',
    'estimated_woba_using_speedangle', 'woba_value', 'woba_denom', 'babip_value', 
    'iso_value', 'launch_speed_angle', 'pitch_name', 'home_score', 'away_score', 
    'bat_score', 'fld_score', 'post_away_score', 'post_home_score', 'post_bat_score', 
    'post_fld_score', 'if_fielding_alignment', 'of_fielding_alignment', 
    'delta_home_win_exp', 'delta_run_exp', 'hit_location', 'umpire', 
    'sv_id', 'spin_dir', 'pitch_type', 'fielder_2.1', 'pitcher.1', 'fielder_3', 
    'fielder_4', 'fielder_5', 'fielder_6', 'fielder_7', 'fielder_8', 'fielder_9'], axis = 1)

    # Define a function to condense pitch types
    def condense_pitch_type(pitch_type):
        if pitch_type == "FF":
            return "FF"
        elif pitch_type == "FT":
            return "SI"
        elif pitch_type == "FC":
            return "CT"
        elif pitch_type in ["SL", "ST", "SV"]:
            return "SL"
        elif pitch_type in ["CH", "FS"]:
            return "CH"
        elif pitch_type in ["CU", "KC", "CS", "CB"]:
            return "CB"
        elif pitch_type == "SI":
            return "SI"
        elif pitch_type == "KN":
            return "KN"
        else:
            return "FAHCK"
        
    def create_pitch_id(df):
        df['pitch_id_raw'] = (df['game_pk'].astype(str) + "_" +
                          df['batter'].astype(str) + "_" +
                          df['pitcher'].astype(str) + "_" +
                          df['pitch_number'].astype(str) + "_" +
                          df['at_bat_number'].astype(str) + "_" +
                          df['inning'].astype(str))
        return df

    def calculate_pitcher_stats(df):
        # Sort the DataFrame
        df_sorted = df.sort_values(by=['game_pk', 'pitcher', 'pitch_type_condensed', 'pitch_number'])

        # Calculate rolling average of release_speed for the last 5 pitches
        df_sorted['rolling_avg_velo'] = df_sorted.groupby(['game_pk', 'pitcher', 'pitch_type_condensed'])['release_speed'].transform(lambda x: x.rolling(5, min_periods=1).mean())

        # Calculate seasonal average for each pitcher and pitch type
        seasonal_avg = df.groupby(['pitcher', 'pitch_type_condensed'])['release_speed'].mean().reset_index()
        seasonal_avg = seasonal_avg.rename(columns={'release_speed': 'seasonal_avg_velo'})

        # Merge seasonal average back to the original DataFrame
        df_merged = df_sorted.merge(seasonal_avg, on=['pitcher', 'pitch_type_condensed'], how='left')

        # If rolling average is NaN, use the seasonal average
        df_merged['rolling_avg_velo'].fillna(df_merged['seasonal_avg_velo'], inplace=True)

        return df_merged
    def is_contact(description):
        return 1 if description in ["foul_tip", "foul_bunt", "foul", 
                                "foul_pitchout", "hit_into_play", 
                                "hit_into_play_no_out", "hit_into_play_score", 
                                "pitchout_hit_into_play_score", 'hit_into_play'] else 0 
    # Define a function to determine if the pitch resulted in a fly ball

    def is_fly_ball(bb_type):
        return 1 if (bb_type == "fly_ball" | bb_type == 'popup') else 0
    
    def is_ground_ball(bb_type):
        return 1 if bb_type == "ground_ball" else 0

    def is_line_drive(bb_type):
        return 1 if bb_type == "line_drive" else 0
    
    def is_fly_ball(bb_type):
        return 1 if bb_type == "fly_ball" else 0
    
    def is_foul(description):
        return 1 if description in ["foul_tip", "foul_pitchout", 'foul', 'foul_pitchout'] else 0

    def is_swing(description):
        return 1 if description in ["swinging_strike", "swinging_strike_blocked", 
                                "missed_bunt", "swinging_pitchout", 
                                "foul_tip", "foul_bunt", "foul", 
                                "foul_pitchout", "hit_into_play", 
                                "hit_into_play_no_out", "hit_into_play_score", 
                                "pitchout_hit_into_play_score"] else 0
        #create a function for whiffs
    def whiff(description):
        return 1 if description in ["swinging_strike", "swinging_strike_blocked", "missed_bunt", "swinging_pitchout"] else 0
    
    df['pitch_type_condensed'] = df['pitch_type'].apply(condense_pitch_type)

    # Call the new function and store the result
    pitcher_stats = calculate_pitcher_stats(df)

    df = df.merge(pitcher_stats, how='left', on=['pitcher', 'pitch_type_condensed'])

    # Calculate 'release_pos_y'
    df['release_pos_y'] = 60.5 - df['release_extension']

    df['swing'] = df['description'].apply(is_swing)

    df['whiff'] = df['description'].apply(whiff)

    df['contact'] = df['description'].apply(is_contact)

    df['foul'] = df['description'].apply(is_foul)

    df['fly_ball'] = df['bb_type'].apply(is_fly_ball)

    df['ground_ball'] = df['bb_type'].apply(is_ground_ball)

    df['line_drive'] = df['bb_type'].apply(is_line_drive)

    df['total_movement'] = np.sqrt(df['pfx_x']**2 + df['pfx_z']**2)
    # Create 'pitch_id_raw'
    df = create_pitch_id(df)

    dfs = df.merge(height_df[['id', 'height_numeric']], how='left', left_on='pitcher', right_on='id')
        # Return the preprocessed DataFrame
    
    dfs['height_ratio'] = dfs['release_pos_z'] / dfs['height_numeric']
    print(dfs.columns)
    return dfs