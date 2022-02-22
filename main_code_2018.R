# Path 
path = '/Users/duda/Documents/World_Cup/'

# Import the trained model that used World Cups 1960 - 2018 (trained in the R file: model_with_2018.R)
# source('model_with_2018.R')
model_sim <- readRDS(paste0(path,'final_model_without_2018'))
summary(model_sim)

# Clean Elo data that will be used in the test sets (generates 'elo_2021.csv')
source(paste0(path,'elo_2017.R'))

# [ORGANIZE TEST DF] Merge group matches with data from each team 
# generates t est dataframe df_clean and saves it in the df_group_phase_2018.csv
source(paste0(path,'group_matches_2018.R'))
head(df_clean)
dim(df_clean)
max(as.numeric(df_clean$simulation))

# [MODEL] Use results to predict game results
# generates the dataframe winner_pred_df and saves it in the winner_pred_df_2018.csv
source(paste0(path,'group_match_predictions_2018.R'))
head(winner_pred_df)
dim(winner_pred_df)
max(as.numeric(winner_pred_df$simulation))

# [RANKING] Use the match prediction to calculate group points and define winner and runner up of each group
# generates df_clean_final and saves in the csv classification_round_16.csv
source(paste0(path,'classification_round_16_2018.R'))
head(df_clean_final)

# [GET MATCHES] Get round 16 matches for each simulation 
# generates df_matches_final and saves the csv matches_round_of_16.csv
source(paste0(path,'round_of_16_matches_2018.R'))
head(df_matches_final)
dim(df_matches_final)
max(as.numeric(df_matches_final$simulation))

# [ORGANIZE TEST DF] Merge round 16 matches with data from each team
# generates df_clean_round_16 and saves the csv df_test_round_16.csv
source(paste0(path,'round_of_16_test_df_2018.R'))
head(df_clean_round_16)
dim(df_clean_round_16)
max(as.numeric(df_clean_round_16$simulation))

# [MODEL] Use results to predict the result of the matches
# generates winner_pred_df and saves the csv round_16_match_predictions.csv
source(paste0(path,'round_of_16_predict_2018.R'))
head(winner_pred_df_round_of_16)
dim(winner_pred_df_round_of_16)
max(as.numeric(winner_pred_df_round_of_16$simulation))

# [GET MATCHES] Get quarter-final matches for each simulation 
# generates df_matches_quarter and save the csv quarter_matches.csv
source(paste0(path,'quarter_final_matches_2018.R'))
head(df_matches_quarter)
dim(df_matches_quarter)
max(as.numeric(df_matches_quarter$simulation))

# [ORGANIZE TEST DF] Merge quarter-final matches with data from each team (set up test dataframe)
# generates df_clean_quarter_finals and saves the csv df_test_quarter_finals.csv
source(paste0(path,'quarter_finals_test_df_2018.R'))
head(df_clean_quarter_finals)

# [MODEL] Use results to predict teams that are moving to the semi-final
# generates winner_pred_df_quarters and saves the csv quarter_final_match_predictions.csv
source(paste0(path,'quarter_finals_predict_2018.R'))
head(winner_pred_df_quarters)

# [GET MATCHES] Get semi-final matches for each simulation 
# generates df_matches_semi and saves the csv matches_semi.csv
source(paste0(path,'semi_final_matches_2018.R'))
head(df_matches_semi)

# [ORGANIZE TEST DF] Merge semi-final matches with data from each team (set up test dataframe)
# generates df_clean_semi and saves the csv df_test_semi.csv
source(paste0(path,'semi_final_test_df_2018.R'))
head(df_clean_semi)

# [MODEL] Use results to predict teams that are moving to the final
source(paste0(path,'semi_final_predict_2018.R'))
# generates winner_pred_df_semi and the csv semi_final_match_predictions.csv
head(winner_pred_df_semi)

#### FINAL AND THIRD PLACE MATCHES

# [GET MATCHES] Get final and third place matches for each simulation 
# generates df_matches_final and df_matches_third_place and csv final_matches.csv and third_place_matches.csv
source(paste0(path,'final_and_third_matches_2018.R'))
head(df_matches_final)
head(df_matches_third)

# [ORGANIZE TEST DF] Merge final and third place matches with data from each team (set up test dataframe)
# generates df_clean_final and df_clean_third, and saves the csvs df_test_final.csv and df_test_third_place.csv
source(paste0(path,'final_and_third_test_df_2018.R'))
head(df_clean_final)
head(df_clean_third)

# [MODEL] Use results to predict first, second, and third place for each simulation
# generates winner_pred_df_final_and_third and the csv final_and_third_match_predictions.csv
source(paste0(path,'final_and_third_predict_2018.R'))
head(winner_pred_df_final_and_third)

# [GET FINAL RESULTS] Summarize the final winners, second and third places 
# generates raking and saves the csv ranking.csv
source(paste0(path,'ranking.R'))
head(ranking)
prop.table(table(ranking$winners))
prop.table(table(ranking$second_places))
prop.table(table(ranking$third_places))





