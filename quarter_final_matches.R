path <- '/Users/duda/Documents/World_Cup/'

# Getting the round of 16 match predictions
match_predictions <- read.csv(paste0(path,'round_16_match_predictions.csv'))

for(i in 1:nrow(match_predictions)){
  
  if(match_predictions$predictions_sim[i] > match_predictions$pred_for_opp_team[i]) match_predictions$winner[i] <- match_predictions$team[i]
  if(match_predictions$predictions_sim[i] < match_predictions$pred_for_opp_team[i]) match_predictions$winner[i] <- match_predictions$opp_team[i]
  
  if(match_predictions$predictions_sim[i] == match_predictions$pred_for_opp_team[i]){
    match_predictions$winner[i] <- ifelse(runif(n=1,min=0,max=1) > 0.5, match_predictions$team[i], match_predictions$opp_team[i])
  }
  
}

winner_df <- match_predictions

# We can use this to get the teams that went to the quarter final with their infos about group and simulation

qualified_for_quarters <- winner_df %>% dplyr::select(winner,simulation,group_team,match_num) %>% distinct %>% 
  arrange(simulation,match_num,group_team)

# dim(qualified_for_quarters) should have 8*n_simulation rows - 1 winner for each match of the round of 16 (checked!)

# Organize the matches that should happen 
 # the result of match_num 1 should play with match_num 2 and so on 

cols <- c('team', 'group_team','opp_team','group_opp_team','match_num_quarter','simulation') 

df_matches_quarter <- data.frame(matrix(NA,
                                nrow = 1,                        
                                ncol = length(cols))) 
names(df_matches_quarter) <- cols

n_matches <- 4
n_simulations <- max(match_predictions$simulation)

for(j in 1:n_simulations){
  
  working_df <- qualified_for_quarters %>% filter(simulation == j)
  
  for(i in 1:n_matches){
    
    match_num_quarter <- i
    simulation <- j
    team <- working_df[working_df$match_num == (2*i-1),]$winner
    group_team <- working_df[working_df$match_num == (2*i-1),]$group_team
    opp_team <- working_df[working_df$match_num == (2*i-1 + 1),]$winner
    group_opp_team <- working_df[working_df$match_num == (2*i-1 + 1),]$group_team
    df_matches_quarter <- rbind(df_matches_quarter,cbind(team,group_team,opp_team,group_opp_team,match_num_quarter,simulation))
    
  }
  
}

df_matches_quarter <- na.omit(df_matches_quarter)

# dim(df_matches_quarter) The number of rows should be 4*n_simulations (4 matches for each simulation) - checked!

# Set this up to send to our clean_df .R chunk

write.csv(df_matches_quarter,paste0(path,'quarter_matches.csv'),row.names = FALSE)