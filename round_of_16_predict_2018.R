path <- '/Users/duda/Documents/World_Cup/'

# Getting the model 
model_sim <- readRDS(paste0(path,'final_model_without_2018'))
summary(model_sim)
coeffs <- model_sim$coefficients

# Getting the train dataset (df_clean) for group phase
df_clean_round_16 <- read.csv(paste0(path,'df_test_round_16_2018.csv'))

# Running the predictions for the simulated matches
predictions_sim <- rep(NA,nrow(df_clean_round_16))

for(i in 1:nrow(df_clean_round_16)) {
  predictions_sim[i] <- rpois(1,exp(coeffs[1] + coeffs[2]*df_clean_round_16$year[i] + 
                                      coeffs[3]*df_clean_round_16$diff_elo[i] + 
                                      coeffs[4]*df_clean_round_16$avg_goals_scored[i] + 
                                      coeffs[5]*df_clean_round_16$avg_opp_goals_taken[i] + 
                                      coeffs[6]*df_clean_round_16$stage_num[i] + 
                                      coeffs[7]*df_clean_round_16$team_has_home_advt[i]))
}

# Why rpois only returns integers? Check why this is so!

preds_df <- na.omit(cbind(df_clean_round_16,predictions_sim))

# With this preds_df, define who won and who lost 

pred_for_opp_team <- rep(NA,nrow(preds_df))

for(i in 1:nrow(preds_df)){
  # Need to add the predictions for the opp_team
  team <- preds_df[i,]$team
  opp_team <- preds_df[i,]$opp_team
  stage <- preds_df[i,]$stage_num
  simulation <- preds_df[i,]$simulation
  pred_for_opp_team[i] <- preds_df[preds_df$team == opp_team & 
                                     preds_df$opp_team == team & 
                                     preds_df$stage_num == stage & 
                                     preds_df$simulation == simulation,]$predictions_sim
}

# library(tidyverse)
combined_pred_opp <- cbind(preds_df,pred_for_opp_team)
winner_pred_df <- combined_pred_opp %>% mutate(team_with_opp = gsub(" ","",paste(tolower(team),
                                                                                 tolower(opp_team))))

# Transform the 96 lines into the actual 48 matches 
str_arrange <- function(x){
  x %>%
    stringr::str_split("") %>% # Split string into letters
    purrr::map(~sort(.) %>% paste(collapse = "")) %>% # Sort and re-combine
    as_vector() # Convert list into vector
}

winner_pred_df$sorted_names <- unlist(lapply(winner_pred_df$team_with_opp,str_arrange))

winner_pred_df$sorted_names_add <- gsub(" ","",paste(winner_pred_df$sorted_names,
                                                     winner_pred_df$stage_num,
                                                     winner_pred_df$simulation))

winner_pred_df <- winner_pred_df[!duplicated(winner_pred_df$sorted_names_add),]

winner_pred_df <- winner_pred_df %>% dplyr::select(!c(team_with_opp,sorted_names_add))

winner_pred_df_round_of_16 <- winner_pred_df

# The dimension of winner_pred_df should be 8*n_simulations (checked!)

write.csv(winner_pred_df_round_of_16,paste0(path,'round_16_match_predictions_2018.csv'),row.names = FALSE)

