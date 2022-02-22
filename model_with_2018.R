path = '/Users/duda/Documents/World_Cup'

library(readr)
library(dplyr)

# Load clean elo function
source(paste0(path,'/clean_elo.R'))
source(paste0(path,'/world_cup_2018_merge.R'))

# Read files
matches <- read_csv(paste0(path,'/raw_data/WorldCupMatches.csv'))
cups_info <- read_csv(paste0(path,'/raw_data/WorldCups.csv'))
country_continent <- read_csv(paste0(path,'/raw_data/country_continent.csv'))
cup_host <- read_csv(paste0(path,'/raw_data/cup_host.csv'))
elo_raw <- read_csv(paste0(path,'/raw_data/elo_rankings_raw_year_bf.csv'))

world_2018 <- df_2018

selected_cols <- c("Year","Home Team Name","Home Team Goals","Away Team Goals","Away Team Name","Stage")

world_2018$Year <- rep(2018,nrow(world_2018))

world_2018 <- world_2018 %>% dplyr::select('Year','team1','score1','score2','team2',"Stage")

names(world_2018)[names(world_2018) == 'team1'] <- 'Home Team Name'
names(world_2018)[names(world_2018) == 'team2'] <- 'Away Team Name'
names(world_2018)[names(world_2018) == 'score1'] <- 'Home Team Goals'
names(world_2018)[names(world_2018) == 'score2'] <- 'Away Team Goals'

# Clean Elo dataframe
full_df <- clean_elo(elo_raw)

# Organize matches dataframe

selected_cols <- c("Year","Home Team Name","Home Team Goals","Away Team Goals","Away Team Name","Stage")
matches_filter <- matches[,selected_cols] %>% filter(as.numeric(Year) > 1960)

matches_filter <- rbind(matches_filter,world_2018)

# Organize stage variable
  # group phase = 1
  # round of 16 = 2
  # quarter-finals = 3
  # semi-finals = 4
  # third place = 5
  # final = 6

matches_filter$stage_num <- rep(NA,nrow(matches_filter))
matches_filter[grepl("Group",matches_filter$Stage), ]$stage_num <- 1
matches_filter[matches_filter$Stage == 'Round of 16', ]$stage_num <- 2
matches_filter[matches_filter$Stage == 'Quarter-finals', ]$stage_num <- 3
matches_filter[matches_filter$Stage == 'Semi-finals', ]$stage_num <- 4
matches_filter[matches_filter$Stage == 'Third place', ]$stage_num <- 5
matches_filter[matches_filter$Stage == 'Match for third place', ]$stage_num <- 5
matches_filter[matches_filter$Stage == 'Play-off for third place', ]$stage_num <- 5
matches_filter[matches_filter$Stage == 'Final', ]$stage_num <- 6

# Organize country name variable

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
names(merged_df_2)[6] <- 'stage'
names(merged_df_2)[7] <- 'stage_num'
names(merged_df_2)[8] <- 'score_team_elo'
names(merged_df_2)[9] <- 'score_team_num_matches'
names(merged_df_2)[10] <- 'score_team_goals_for'
names(merged_df_2)[11] <- 'score_team_goals_against'
names(merged_df_2)[12] <- 'opp_team_elo'
names(merged_df_2)[13] <- 'opp_team_num_matches'
names(merged_df_2)[14] <- 'opp_team_goals_for'
names(merged_df_2)[15] <- 'opp_team_goals_against'

merged_df_2 <- merged_df_2[!duplicated(merged_df_2), ]

# Organizing df for score teams

df_1_raw <- merged_df_2[,c('stage_num','opp_team_goals','year','score_team_goals','score_team','opp_team','score_team_elo','opp_team_elo','score_team_goals_for','score_team_num_matches','opp_team_goals_against','opp_team_num_matches')]
df_1 <- df_1_raw %>% mutate(diff_elo = score_team_elo - opp_team_elo,
                            avg_goals_scored = score_team_goals_for/score_team_num_matches,
                            avg_opp_goals_taken = opp_team_goals_against/opp_team_num_matches,
                            goals = score_team_goals,
                            team = score_team,
                            opp_team = opp_team,
                            goals_taken = opp_team_goals) %>%
  dplyr::select(year,goals,diff_elo,avg_goals_scored,avg_opp_goals_taken,team,opp_team,goals_taken,stage_num)

df_2_raw <- merged_df_2[,c('stage_num','score_team_goals','year','opp_team_goals','opp_team','score_team','score_team_elo','opp_team_elo','score_team_goals_against','score_team_num_matches','opp_team_goals_for','opp_team_num_matches')]
df_2 <- df_2_raw %>% mutate(diff_elo = opp_team_elo - score_team_elo,
                            avg_goals_scored = opp_team_goals_for/opp_team_num_matches,
                            avg_opp_goals_taken = score_team_goals_against/score_team_num_matches,
                            goals = opp_team_goals,
                            team = opp_team,
                            opp_team = score_team,
                            goals_taken = score_team_goals) %>%
  dplyr::select(year,goals,diff_elo,avg_goals_scored,avg_opp_goals_taken,team,opp_team,goals_taken,stage_num)

df_final <- rbind(df_1,df_2)

df_country_test <- merge(df_final,country_continent,by.x = 'team',by.y = 'country',all.x = TRUE)
df_country_test_opp <- merge(df_country_test,country_continent,by.x = 'opp_team',by.y = 'country',all.x = TRUE)
df_host_complete <- merge(df_country_test_opp,cup_host,by.x='year',by.y='year',all.x = TRUE)

names(df_host_complete)[10] <- 'continent_team'
names(df_host_complete)[11] <- 'continent_opp'
names(df_host_complete)[13] <- 'continent_host'   

df_host_team_opp <- df_host_complete %>% mutate(team_in_continent = ifelse(continent_team == continent_host,'1','0'),
                                                opp_in_continent = ifelse(continent_opp == continent_host,'1','0'))


df_advt_home <- df_host_team_opp %>% mutate(team_has_home_advt = case_when((team_in_continent == 1 & opp_in_continent == 0) ~ 1,
                                                           (team_in_continent == 0 & opp_in_continent == 1) ~ -1,
                                                           TRUE ~ 0))

######### Poisson regression

df_final <- df_advt_home 

train_data <- df_final[df_final$year != '2018',] %>% dplyr::select(year,goals,diff_elo,avg_goals_scored,avg_opp_goals_taken,stage_num,team_has_home_advt)
test_data <- df_final[df_final$year == '2018',] %>% dplyr::select(year,goals,diff_elo,avg_goals_scored,avg_opp_goals_taken,stage_num,team_has_home_advt)

model <- glm(goals ~ ., family = 'poisson', data = train_data)
summary(model)

predictions <- predict(model, newdata = test_data, type = 'response')

predictions_2018 <- cbind(df_final[df_final$year == '2018',],predictions)
predictions_2018 <- predictions_2018[!duplicated(predictions_2018), ]

# We have 128 predictions (goals for team and opp for each of the 64 matches)

######### TESTING THE MODEL: What percentage of winners did it get right?
pred_for_opp_team <- rep(NA,nrow(predictions_2018))

for(i in 1:nrow(predictions_2018)){
  
  # Need to add the predictions for the opp_team
  team <- predictions_2018[i,]$team
  opp_team <- predictions_2018[i,]$opp_team
  stage <- predictions_2018[i,]$stage_num
  pred_for_opp_team[i] <- predictions_2018[predictions_2018$team == opp_team & predictions_2018$opp_team == team & predictions_2018$stage_num == stage,]$predictions
}

# library(tidyverse)
combined_pred_opp <- cbind(predictions_2018,pred_for_opp_team)
winner_pred_df <- combined_pred_opp %>% mutate(winner = ifelse(goals > goals_taken,team,opp_team)) %>%
  mutate(team_with_opp = gsub(" ","",paste(tolower(team),tolower(opp_team),stage_num)))

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
sum(winner_pred_df$pred_winner == winner_pred_df$winner)/nrow(winner_pred_df) # It got 63% of the 64 games correct

######## SAVING THE FINAL MODEL 

final_model <- glm(goals ~ ., family = 'poisson', data = df_final %>% 
                     dplyr::select(year,goals,diff_elo,avg_goals_scored,avg_opp_goals_taken,stage_num,team_has_home_advt))

summary(final_model)

# Save final model 
saveRDS(final_model,'/Users/duda/Documents/World_Cup/final_model')






