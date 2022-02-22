path <- '/Users/duda/Documents/World_Cup/'

match_predictions <- read.csv(paste0(path,'final_and_third_match_predictions.csv'))

match_predictions$winner <- rep(NA,nrow(match_predictions))
match_predictions$second_place <- rep(NA,nrow(match_predictions))
match_predictions$third_place <- rep(NA,nrow(match_predictions))
match_predictions$fourth_place <- rep(NA,nrow(match_predictions))

for(i in 1:nrow(match_predictions)){
  
  if(match_predictions$predictions_sim[i] > match_predictions$pred_for_opp_team[i]) {
    
    if(match_predictions$stage_num[i] == 6){
      match_predictions[i,]$winner <- match_predictions$team[i]
      match_predictions[i,]$second_place <- match_predictions$opp_team[i]
    } else {
      match_predictions[i,]$third_place <- match_predictions$team[i]
      match_predictions[i,]$fourth_place <- match_predictions$opp_team[i]
    }
    
  }
  
  if(match_predictions$predictions_sim[i] < match_predictions$pred_for_opp_team[i]){ 
    
    if(match_predictions$stage_num[i] == 6){
      match_predictions[i,]$winner <- match_predictions$opp_team[i]
      match_predictions[i,]$second_place <- match_predictions$team[i]
    } else {
      match_predictions[i,]$third_place <- match_predictions$opp_team[i]
      match_predictions[i,]$fourth_place <- match_predictions$team[i]
    }
    
  }
  
  if(match_predictions$predictions_sim[i] == match_predictions$pred_for_opp_team[i]){
    randomize <- runif(n=1,min=0,max=1) > 0.5
    
    if(match_predictions$stage_num[i] == 6){
      match_predictions[i,]$winner <- ifelse(randomize > 0.5, match_predictions$team[i], match_predictions$opp_team[i])
      match_predictions[i,]$second_place <- ifelse(randomize < 0.5, match_predictions$team[i], match_predictions$opp_team[i])
    } else {
      match_predictions[i,]$third_place <- ifelse(randomize > 0.5, match_predictions$team[i], match_predictions$opp_team[i])
      match_predictions[i,]$fourth_place <- ifelse(randomize < 0.5, match_predictions$team[i], match_predictions$opp_team[i])
    }
    
  }
  
}

match_predictions <- match_predictions %>% arrange(as.numeric(simulation))

n_simulations <- max(match_predictions$simulation)
simulation <- seq(1:n_simulations)
winners <- na.omit(match_predictions$winner)
second_places <- na.omit(match_predictions$second_place)
third_places <- na.omit(match_predictions$third_place)
fourth_places <- na.omit(match_predictions$fourth_place)

ranking <- data.frame(simulation,winners,second_places,third_places,fourth_places)

write.csv(ranking,paste0(path,'ranking.csv'),row.names = FALSE)

# Champions dataframe 
champions <- match_predictions %>% filter(stage_num == 6) %>% dplyr::select(simulation,winner)
write.csv(champions,paste0(path,'champions'),row.names = FALSE)