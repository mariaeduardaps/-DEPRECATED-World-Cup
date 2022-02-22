path <- '/Users/duda/Documents/World_Cup/'

# Getting the round of 16 match predictions
match_predictions <- read.csv(paste0(path,'semi_final_match_predictions.csv'))

for(i in 1:nrow(match_predictions)){
  
  if(match_predictions$predictions_sim[i] > match_predictions$pred_for_opp_team[i]) {
    match_predictions$winner[i] <- match_predictions$team[i]
    match_predictions$runnerup[i] <- match_predictions$opp_team[i]
  }
  
  if(match_predictions$predictions_sim[i] < match_predictions$pred_for_opp_team[i]){ 
    match_predictions$winner[i] <- match_predictions$opp_team[i]
    match_predictions$runnerup[i] <- match_predictions$team[i]
    
  }
  
  if(match_predictions$predictions_sim[i] == match_predictions$pred_for_opp_team[i]){
    randomize <- runif(n=1,min=0,max=1) > 0.5
    match_predictions$winner[i] <- ifelse(randomize > 0.5, match_predictions$team[i], match_predictions$opp_team[i])
    match_predictions$runnerup[i] <- ifelse(randomize < 0.5, match_predictions$team[i], match_predictions$opp_team[i])
  }
  
}

# The winners of match 1 plays the winners of match 2
# The runner ups of match 1 plays the runner ups of match 2

match_predictions <- match_predictions %>% mutate(match_num = match_num_semi)

# We can use this to get the teams that went to the quarter final with their infos about group and simulation

qualified_for_final <- match_predictions %>% dplyr::select(winner,simulation,group_team,match_num) %>% distinct %>% 
  arrange(simulation,match_num,group_team)

qualified_for_third_place <- match_predictions %>% dplyr::select(runnerup,simulation,group_team,match_num) %>% distinct %>% 
  arrange(simulation,match_num,group_team)

#### Organize the final matches

# Organize the matches that should happen 
# the result of match_num 1 should play with match_num 2 and so on 

cols <- c('team', 'group_team','opp_team','group_opp_team','match_num_final','simulation') 

df_matches_final <- data.frame(matrix(NA,
                                     nrow = 1,                        
                                     ncol = length(cols))) 
names(df_matches_final) <- cols

n_matches <- 1
n_simulations <- max(match_predictions$simulation)

for(j in 1:n_simulations){
  
  working_df <- qualified_for_final %>% filter(simulation == j)
  
  for(i in 1:n_matches){
    
    match_num_final <- i
    simulation <- j
    team <- working_df[working_df$match_num == (2*i-1),]$winner
    group_team <- working_df[working_df$match_num == (2*i-1),]$group_team
    opp_team <- working_df[working_df$match_num == (2*i-1 + 1),]$winner
    group_opp_team <- working_df[working_df$match_num == (2*i-1 + 1),]$group_team
    df_matches_final <- rbind(df_matches_final,cbind(team,group_team,opp_team,group_opp_team,match_num_final,simulation))
    
  }
  
}

df_matches_final <- na.omit(df_matches_final)

##### Organize the third place matches

cols <- c('team', 'group_team','opp_team','group_opp_team','match_num_third_place','simulation') 

df_matches_third <- data.frame(matrix(NA,
                                      nrow = 1,                        
                                      ncol = length(cols))) 
names(df_matches_third) <- cols

n_matches <- 1
n_simulations <- max(match_predictions$simulation)

for(j in 1:n_simulations){
  
  working_df <- qualified_for_third_place %>% filter(simulation == j)
  
  for(i in 1:n_matches){
    
    match_num_third_place <- i
    simulation <- j
    team <- working_df[working_df$match_num == (2*i-1),]$runnerup
    group_team <- working_df[working_df$match_num == (2*i-1),]$group_team
    opp_team <- working_df[working_df$match_num == (2*i-1 + 1),]$runnerup
    group_opp_team <- working_df[working_df$match_num == (2*i-1 + 1),]$group_team
    df_matches_third <- rbind(df_matches_third,cbind(team,group_team,opp_team,group_opp_team,match_num_third_place,simulation))
    
  }
  
}

df_matches_third <- na.omit(df_matches_third)

write.csv(df_matches_final,paste0(path,'final_matches.csv'),row.names = FALSE)
write.csv(df_matches_third,paste0(path,'third_place_matches.csv'),row.names = FALSE)


