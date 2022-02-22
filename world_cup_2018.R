# To work with 2018 data we need to be able to tell apart when matches with the same team and opp team occur
# For example, England and Belgium played twice (group stage and for third place). We need a new variable to show these are
# two different matches

library(readr)
library(dplyr)
# Read dataset

matches <- read_csv('/Users/duda/Downloads/WorldCupMatches.csv')
cups_info <- read_csv('/Users/duda/Downloads/WorldCups.csv')
country_continent <- read_csv('/Users/duda/Downloads/country_continent.csv')
cup_host <- read_csv('/Users/duda/Downloads/cup_host.csv')

elo_raw <- read_csv('/Users/duda/Downloads/elo_rankings_raw_year_bf.csv')

world_2018 <- read_csv('/Users/duda/Downloads/wc_2018.csv')

selected_cols <- c("Year","Home Team Name","Home Team Goals","Away Team Goals","Away Team Name")

world_2018$Year <- rep(2018,nrow(world_2018))

world_2018 <- world_2018 %>% select('Year','team1','score1','score2','team2')

names(world_2018)[names(world_2018) == 'team1'] <- 'Home Team Name'
names(world_2018)[names(world_2018) == 'team2'] <- 'Away Team Name'
names(world_2018)[names(world_2018) == 'score1'] <- 'Home Team Goals'
names(world_2018)[names(world_2018) == 'score2'] <- 'Away Team Goals'

cols <- c('Year', 'country_name', 'elo_score', 'num_matches', 'goals_for', 
          'goals_against') 

# Initialize our data.frame:
full_df <- data.frame(matrix(NA,
                             nrow = 1,                        
                             ncol = length(cols))) 
names(full_df) <- cols


for(i in 1:ncol(elo_raw)){
  
  cols <- c('Year', 'country_name', 'elo_score', 'num_matches', 'goals_for', 
            'goals_against') 
  
  # Initialize our data.frame:
  z <- data.frame(matrix(NA,
                         nrow = 1,                        
                         ncol = length(cols))) 
  names(z) <- cols
  
  Year <- colnames(elo_raw[,i])
  info <- na.omit(elo_raw[,i])
  n_countries <- (nrow(info) - 14 - 2)/16 # The actual number of countries is one more than that
  
  for(country in 0:n_countries){
    
    country_name <- as.character(info[2 + 16*country,])
    elo_score <- as.numeric(info[3 + 16*country,])
    num_matches <- as.numeric(info[8 + 16*country,])
    goals_for <- as.numeric(info[15 + 16*country,])
    goals_against <- as.numeric(info[16 + 16*country,])
    
    
    z <- na.omit(rbind(z,data.frame(Year,country_name,elo_score,num_matches,goals_for,goals_against)))
    
  }
  
  full_df <- na.omit(rbind(full_df,z))
  
}

# Need to merge with matches

selected_cols <- c("Year","Home Team Name","Home Team Goals","Away Team Goals","Away Team Name")
matches_filter <- matches[,selected_cols] %>% filter(as.numeric(Year) > 1960)

matches_filter <- rbind(matches_filter,world_2018)

matches_filter[matches_filter$`Home Team Name` == 'Germany FR',]$`Home Team Name` <- 'West Germany'
matches_filter[matches_filter$`Home Team Name` == 'Germany DR',]$`Home Team Name` <- 'East Germany'
matches_filter[matches_filter$`Away Team Name` == 'Germany FR',]$`Away Team Name` <- 'West Germany'
matches_filter[matches_filter$`Away Team Name` == 'Germany DR',]$`Away Team Name` <- 'East Germany'
matches_filter[matches_filter$`Home Team Name` == 'German FR',]$`Home Team Name` <- 'West Germany'
matches_filter[matches_filter$`Home Team Name` == 'German DR',]$`Home Team Name` <- 'East Germany'
matches_filter[matches_filter$`Away Team Name` == 'German FR',]$`Away Team Name` <- 'West Germany'
matches_filter[matches_filter$`Away Team Name` == 'German DR',]$`Away Team Name` <- 'East Germany'

matches_filter[matches_filter$`Home Team Name` == 'Korea DPR',]$`Home Team Name` <- 'North Korea'
matches_filter[matches_filter$`Away Team Name` == 'Korea DPR',]$`Away Team Name` <- 'North Korea'

matches_filter[matches_filter$`Home Team Name` == 'Korea Republic',]$`Home Team Name` <- 'South Korea'
matches_filter[matches_filter$`Away Team Name` == 'Korea Republic',]$`Away Team Name` <- 'South Korea'

matches_filter[matches_filter$`Home Team Name` == 'rn">Republic of Ireland',]$`Home Team Name` <- 'Ireland'
matches_filter[matches_filter$`Away Team Name` == 'rn">Republic of Ireland',]$`Away Team Name` <- 'Ireland'

matches_filter[matches_filter$`Home Team Name` == 'rn">United Arab Emirates',]$`Home Team Name` <- 'United Arab Emirates'
matches_filter[matches_filter$`Away Team Name` == 'rn">United Arab Emirates',]$`Away Team Name` <- 'United Arab Emirates'

matches_filter[matches_filter$`Home Team Name` == 'USA',]$`Home Team Name` <- 'United States'
matches_filter[matches_filter$`Away Team Name` == 'USA',]$`Away Team Name` <- 'United States'

# When using the Elo from the previous year remember to use this as > instead of >= 1990
matches_filter[matches_filter$`Home Team Name` %in% c('East Germany','West Germany') & matches_filter$Year > '1990',]$`Home Team Name` <- 'Germany'
matches_filter[matches_filter$`Away Team Name` %in% c('East Germany','West Germany') & matches_filter$Year >'1990',]$`Away Team Name` <- 'Germany'


matches_filter[matches_filter$`Home Team Name` == 'China PR',]$`Home Team Name` <- 'China'
matches_filter[matches_filter$`Away Team Name` == 'China PR',]$`Away Team Name` <- 'China'

matches_filter[matches_filter$`Home Team Name` == 'IR Iran',]$`Home Team Name` <- 'Iran'
matches_filter[matches_filter$`Away Team Name` == 'IR Iran',]$`Away Team Name` <- 'Iran'

matches_filter[matches_filter$`Home Team Name` == 'rn">Bosnia and Herzegovina',]$`Home Team Name` <- 'Bosnia and Herzegovina'
matches_filter[matches_filter$`Away Team Name` == 'rn">Bosnia and Herzegovina',]$`Away Team Name` <- 'Bosnia and Herzegovina'

#matches_filter[matches_filter$`Home Team Name` == 'rn">Serbia and Montenegro',]$`Home Team Name` <- 'Serbia'
#matches_filter[matches_filter$`Away Team Name` == 'rn">Serbia and Montenegro',]$`Away Team Name` <- 'Serbia'

matches_filter[matches_filter$`Home Team Name` == 'rn">Serbia and Montenegro',]$`Home Team Name` <- 'Serbia and Montenegro'
matches_filter[matches_filter$`Away Team Name` == 'rn">Serbia and Montenegro',]$`Away Team Name` <- 'Serbia and Montenegro'

matches_filter[matches_filter$`Home Team Name` %in% c('Czech Republic') & matches_filter$Year >= '2006',]$`Home Team Name` <- 'Czechia'
matches_filter[matches_filter$`Away Team Name` %in% c('Czech Republic') & matches_filter$Year >= '2006',]$`Away Team Name` <- 'Czechia'

matches_filter[matches_filter$`Home Team Name` == 'rn">Trinidad and Tobago',]$`Home Team Name` <- 'Trinidad and Tobago'
matches_filter[matches_filter$`Away Team Name` == 'rn">Trinidad and Tobago',]$`Away Team Name` <- 'Trinidad and Tobago'

matches_filter[matches_filter$`Home Team Name` == "C�te d'Ivoire",]$`Home Team Name` <- 'Ivory Coast'
matches_filter[matches_filter$`Away Team Name` == "C�te d'Ivoire",]$`Away Team Name` <- 'Ivory Coast'

# For each match, get main team, opposing team, elo of the main, opposing, goals for the main team, goals for the opposing team

merged_df <- merge(matches_filter,full_df,by.x = c("Year","Home Team Name"), by.y = c("Year","country_name"), all.x = TRUE)

# Now with this merged df we need to get the opp team data from full_df

merged_df_2 <- merge(merged_df,full_df,by.x = c("Year","Away Team Name"), by.y = c("Year","country_name"), all.x = TRUE)

# Rename all columns

names(merged_df_2)[1] <- 'year'
names(merged_df_2)[2] <- 'opp_team'
names(merged_df_2)[3] <- 'score_team'
names(merged_df_2)[4] <- 'score_team_goals'
names(merged_df_2)[5] <- 'opp_team_goals'
names(merged_df_2)[6] <- 'score_team_elo'
names(merged_df_2)[7] <- 'score_team_num_matches'
names(merged_df_2)[8] <- 'score_team_goals_for'
names(merged_df_2)[9] <- 'score_team_goals_against'
names(merged_df_2)[10] <- 'opp_team_elo'
names(merged_df_2)[11] <- 'opp_team_num_matches'
names(merged_df_2)[12] <- 'opp_team_goals_for'
names(merged_df_2)[13] <- 'opp_team_goals_against'

merged_df_2 <- merged_df_2[!duplicated(merged_df_2), ]

# Organizing df for score teams

df_1_raw <- merged_df_2[,c('opp_team_goals','year','score_team_goals','score_team','opp_team','score_team_elo','opp_team_elo','score_team_goals_for','score_team_num_matches','opp_team_goals_against','opp_team_num_matches')]
df_1 <- df_1_raw %>% mutate(diff_elo = score_team_elo - opp_team_elo,
                            avg_goals_scored = score_team_goals_for/score_team_num_matches,
                            avg_opp_goals_taken = opp_team_goals_against/opp_team_num_matches,
                            goals = score_team_goals,
                            team = score_team,
                            opp_team = opp_team,
                            goals_taken = opp_team_goals) %>%
  dplyr::select(year,goals,diff_elo,avg_goals_scored,avg_opp_goals_taken,team,opp_team,goals_taken)

df_2_raw <- merged_df_2[,c('score_team_goals','year','opp_team_goals','opp_team','score_team','score_team_elo','opp_team_elo','score_team_goals_against','score_team_num_matches','opp_team_goals_for','opp_team_num_matches')]
df_2 <- df_2_raw %>% mutate(diff_elo = opp_team_elo - score_team_elo,
                            avg_goals_scored = opp_team_goals_for/opp_team_num_matches,
                            avg_opp_goals_taken = score_team_goals_against/score_team_num_matches,
                            goals = opp_team_goals,
                            team = opp_team,
                            opp_team = score_team,
                            goals_taken = score_team_goals) %>%
  dplyr::select(year,goals,diff_elo,avg_goals_scored,avg_opp_goals_taken,team,opp_team,goals_taken)

df_final <- rbind(df_1,df_2)

df_country_test <- merge(df_final,country_continent,by.x = 'team',by.y = 'country',all.x = TRUE)

df_host_complete <- merge(df_country_test,cup_host,by.x='year',by.y='year',all.x = TRUE)

df_host_complete <- df_host_complete %>% mutate(in_continent = ifelse(continent.x == continent.y,'1','0'))

# Graph to show how the poisson distribution applies for the y values
#ggplot(df_final,aes(x = as.numeric(goals))) + geom_density(adjust = 2) + facet_wrap(~year)

######### Poisson regression

df_final <- df_host_complete 

train_data <- df_final[df_final$year != '2018',] %>% dplyr::select(year,goals,diff_elo,avg_goals_scored,avg_opp_goals_taken,in_continent)
test_data <- df_final[df_final$year == '2018',] %>% dplyr::select(year,goals,diff_elo,avg_goals_scored,avg_opp_goals_taken,in_continent)

model <- glm(goals ~ ., family = 'poisson', data = train_data)
summary(model)

predictions <- predict(model, newdata = test_data, type = 'response')

predictions_2014 <- cbind(df_final[df_final$year == '2018',],predictions)
predictions_2014 <- predictions_2014[!duplicated(predictions_2014), ]

# write.csv(predictions_2014,'predictions_2014.csv')

######### What percentage of winners did it get right?
pred_for_opp_team <- rep(NA,nrow(predictions_2014))

for(i in 1:nrow(predictions_2014)){
  
  # Need to add the predictions for the opp_team
  print(i)
  team <- predictions_2014[i,]$team
  opp_team <- predictions_2014[i,]$opp_team
  pred_for_opp_team[i] <- predictions_2014[predictions_2014$team == opp_team & predictions_2014$opp_team == team,]$predictions
  print(length(predictions_2014[predictions_2014$team == opp_team & predictions_2014$opp_team == team,]$predictions))
  
}

library(tidyverse)
combined_pred_opp <- cbind(predictions_2014,pred_for_opp_team)
winner_pred_df <- combined_pred_opp %>% mutate(winner = ifelse(goals > goals_taken,team,opp_team)) %>%
  mutate(team_with_opp = gsub(" ","",paste(tolower(team),tolower(opp_team))))

# Create a column of predicted winners (sampling when predicted goals are equal)
winner_pred_df$pred_winner <- rep(NA,nrow(winner_pred_df))

for(i in 1:nrow(winner_pred_df)){
  
  if(winner_pred_df$predictions[i] > winner_pred_df$pred_for_opp_team[i]) winner_pred_df$pred_winner[i] <- winner_pred_df$team[i]
  if(winner_pred_df$predictions[i] < winner_pred_df$pred_for_opp_team[i]) winner_pred_df$pred_winner[i] <- winner_pred_df$opp_team[i]
  
  if(winner_pred_df$predictions[i] == winner_pred_df$pred_for_opp_team[i]){
    winner_pred_df$pred_winner[i] <- ifelse(runif(n=1,min=0,max=1) > 0.5, winner_pred_df$team[i], winner_pred_df$opp_team[i])
  }
  
}

str_arrange <- function(x){
  x %>%
    stringr::str_split("") %>% # Split string into letters
    purrr::map(~sort(.) %>% paste(collapse = "")) %>% # Sort and re-combine
    as_vector() # Convert list into vector
}

winner_pred_df$sorted_names <- unlist(lapply(winner_pred_df$team_with_opp,str_arrange))

winner_pred_df = winner_pred_df[!duplicated(winner_pred_df$sorted_names),]

sum(winner_pred_df$pred_winner == winner_pred_df$winner)/nrow(winner_pred_df) # It got 70% of the 128 games correct!

######### Simulation 

summary(model)
coeffs <- model$coefficients

sim_2014 <- df_final[df_final$year == '2018',]
sim_2014$match <- 1:nrow(sim_2014)
n_simulations <- 1000

cols <- c('match', 'predictions_sim')

# Initialize our data.frame:
preds_df <- data.frame(matrix(NA,
                              nrow = length(files),                        
                              ncol = length(cols))) 
names(preds_df) <- cols

for(i in 1:nrow(sim_2014)){
  
  match <- i
  predictions_sim <- rpois(n_simulations,exp(coeffs[1] + coeffs[2]*sim_2014$year[i] + coeffs[3]*sim_2014$diff_elo[i] + 
                                               coeffs[4]*sim_2014$avg_goals_scored[i] + coeffs[5]*sim_2014$avg_opp_goals_taken[i] + coeffs[6]*(sim_2014$in_continent[i] == 1)))
  preds_df <- na.omit(rbind(preds_df,data.frame(match = rep(match,n_simulations),predictions_sim)))
  
}

df_with_sims <- merge(preds_df,sim_2014,all.x = TRUE)
df_with_sims$predictions_opp <- rep(NA,nrow(df_with_sims))

for(i in 1:nrow(sim_2014)){
  control <- i - 1
  team <- df_with_sims$team[1 + control*n_simulations]
  opp_team <- df_with_sims$opp_team[1 + control*n_simulations]
  pred_opp <- df_with_sims[df_with_sims$team == opp_team & df_with_sims$opp_team == team,]$predictions_sim
  df_with_sims[(1+n_simulations*control):(n_simulations*i),]$predictions_opp <- pred_opp
}

# Remove duplicates

df_with_sims <- df_with_sims %>% mutate(team_with_opp = gsub(" ","",paste(tolower(team),tolower(opp_team))))
df_with_sims$sorted_names <- unlist(lapply(df_with_sims$team_with_opp,str_arrange))

df_with_sims <- df_with_sims %>% mutate(winner = ifelse(goals > goals_taken,team,opp_team))

# Create a column of predicted winners (sampling when predicted goals are equal)
df_with_sims$pred_winner <- rep(NA,nrow(df_with_sims))

for(i in 1:nrow(df_with_sims)){
  
  if(df_with_sims$predictions_sim[i] > df_with_sims$predictions_opp[i]) df_with_sims$pred_winner[i] <- df_with_sims$team[i]
  if(df_with_sims$predictions_sim[i] < df_with_sims$predictions_opp[i]) df_with_sims$pred_winner[i] <- df_with_sims$opp_team[i]
  
  if(df_with_sims$predictions_sim[i] == df_with_sims$predictions_opp[i]){
    df_with_sims$pred_winner[i] <- ifelse(runif(n=1,min=0,max=1) > 0.5, df_with_sims$team[i], df_with_sims$opp_team[i])
  }
  
}


for(i in 1:(nrow(sim_2014)/2)){
  
  match_selected <- i 
  control <- i - 1
  team <- df_with_sims$team[1 + control*n_simulations]
  opp_team <- df_with_sims$opp_team[1 + control*n_simulations]
  
  # Remove all cases when opp_team == team to remove duplicates
  
  df_with_sims <- df_with_sims[!(df_with_sims$team == opp_team & df_with_sims$opp_team == team),]
  
}


# Get a table, for each match, what percentage of the time team1 was the winner and team2 was the winner

cols <- c('match_selected', 'team_selected','opp_team_selected','perc_win_team','perc_win_opp')

# Initialize our data.frame:
sim_results <- data.frame(matrix(NA,
                                 nrow = length(files),                        
                                 ncol = length(cols))) 
names(sim_results) <- cols

for(i in 1:(nrow(sim_2014)/2)){
  match_selected <- i 
  control <- i - 1
  team_selected <- df_with_sims$team[1 + control*n_simulations]
  opp_team_selected <- df_with_sims$opp_team[1 + control*n_simulations]
  
  df_filtered <- df_with_sims %>% filter(team == team_selected & opp_team == opp_team_selected)
  
  perc_win_team <- sum(df_filtered$pred_winner == df_filtered$team)/n_simulations
  perc_win_opp <- sum(df_filtered$pred_winner == df_filtered$opp_team)/n_simulations
  sim_results <- na.omit(rbind(sim_results,data.frame(match_selected,team_selected,opp_team_selected,perc_win_team,perc_win_opp)))
}


