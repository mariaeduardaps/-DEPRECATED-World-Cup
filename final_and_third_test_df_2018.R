path <- '/Users/duda/Documents/World_Cup/'

final_matches <- read.csv(paste0(path,'final_matches_2018.csv'))
third_place_matches <- read.csv(paste0(path,'third_place_matches_2018.csv'))

stage_num_selected_final <- 6
stage_num_selected_third <- 5

elo_2021 <- read_csv(paste0(path,'elo_2017.csv'))
elo_2021[elo_2021$country_name == 'Korea Republic',]$country_name <- 'South Korea'

country_continent <- read_csv(paste0(path,'/raw_data/country_continent.csv'))

# Get Host continent
host_continent <- 'Asia'

# Duplicate each match so that we have one line for team + opp_team and another line for opp_team + team 

### FINAL 

for(i in 1:nrow(final_matches)){
  
  team <- final_matches[i,]$opp_team 
  group_team <- final_matches[i,]$group_opp_team
  opp_team <- final_matches[i,]$team 
  group_opp_team <- final_matches[i,]$group_team
  match_num_final <- final_matches[i,]$match_num_final
  simulation <- final_matches[i,]$simulation
  final_matches <- rbind(final_matches,cbind(team,group_team,
                                             opp_team,group_opp_team,match_num_final,simulation))
  
}

# dim(final_matches) here should have 2*n_simulation lines 
# (1 matches duplicated for each simulation) - checked!

# Merge the continents with the country column

df_team <- merge(final_matches,country_continent,by.x = 'team', by.y = 'country',all.x = TRUE) %>%
  rename(team_continent = continent)
df_with_elos <-  merge(df_team,country_continent,by.x = 'opp_team', by.y = 'country',all.x = TRUE) %>%
  rename(opp_continent = continent)

df_with_elos$host_continent <- rep(host_continent,nrow(df_with_elos))

df_with_elos$stage_num <- rep(stage_num_selected_final,nrow(df_with_elos))

# dim(matches_round_16)

merge_elo_team <- merge(df_with_elos,elo_2021,by.x = 'team',by.y = 'country_name',all.x = TRUE)

merge_elo_all <- merge(merge_elo_team,elo_2021,by.x = 'opp_team',by.y ='country_name',all.x = TRUE)

# Organize in a df exactly like the train data that goes into the model 

df_clean_final <- merge_elo_all %>% mutate(year = Year.x,
                                           diff_elo = elo_score.x - elo_score.y,
                                           avg_goals_scored = goals_for.x/num_matches.x,
                                           avg_opp_goals_taken = goals_against.y/num_matches.y,
                                           stage_num = stage_num,
                                           simulation = as.numeric(simulation),
                                           group_team = group_team,
                                           group_opp = group_opp_team,
                                           team_in_continent = ifelse(team_continent == host_continent,1,0),
                                           opp_in_continent = ifelse(opp_continent == host_continent,1,0)
) %>% mutate(team_has_home_advt = case_when((team_in_continent == 1 & opp_in_continent == 0) ~ 1,
                                            (team_in_continent == 0 & opp_in_continent == 1) ~ -1,
                                            TRUE ~ 0)) %>% 
  dplyr::select('team','opp_team','stage_num','year','simulation',
                'diff_elo','avg_goals_scored','avg_opp_goals_taken','team_has_home_advt',
                'group_team','group_opp_team','match_num_final') %>% arrange(simulation)

### Third place matches 

for(i in 1:nrow(third_place_matches)){
  
  team <- third_place_matches[i,]$opp_team 
  group_team <- third_place_matches[i,]$group_opp_team
  opp_team <- third_place_matches[i,]$team 
  group_opp_team <- third_place_matches[i,]$group_team
  match_num_final <- third_place_matches[i,]$match_num_final
  simulation <- third_place_matches[i,]$simulation
  third_place_matches <- rbind(third_place_matches,cbind(team,group_team,
                                                         opp_team,group_opp_team,match_num_third_place,simulation))
  
}

# dim(final_matches) here should have 2*n_simulation lines 
# (1 matches duplicated for each simulation) - checked!

# Merge the continents with the country column

df_team <- merge(third_place_matches,country_continent,by.x = 'team', by.y = 'country',all.x = TRUE) %>%
  rename(team_continent = continent)
df_with_elos <-  merge(df_team,country_continent,by.x = 'opp_team', by.y = 'country',all.x = TRUE) %>%
  rename(opp_continent = continent)

df_with_elos$host_continent <- rep(host_continent,nrow(df_with_elos))

df_with_elos$stage_num <- rep(stage_num_selected_third,nrow(df_with_elos))

# dim(matches_round_16)

merge_elo_team <- merge(df_with_elos,elo_2021,by.x = 'team',by.y = 'country_name',all.x = TRUE)

merge_elo_all <- merge(merge_elo_team,elo_2021,by.x = 'opp_team',by.y ='country_name',all.x = TRUE)

# Organize in a df exactly like the train data that goes into the model 

df_clean_third <- merge_elo_all %>% mutate(year = Year.x,
                                           diff_elo = elo_score.x - elo_score.y,
                                           avg_goals_scored = goals_for.x/num_matches.x,
                                           avg_opp_goals_taken = goals_against.y/num_matches.y,
                                           stage_num = stage_num,
                                           simulation = as.numeric(simulation),
                                           group_team = group_team,
                                           group_opp = group_opp_team,
                                           team_in_continent = ifelse(team_continent == host_continent,1,0),
                                           opp_in_continent = ifelse(opp_continent == host_continent,1,0)
) %>% mutate(team_has_home_advt = case_when((team_in_continent == 1 & opp_in_continent == 0) ~ 1,
                                            (team_in_continent == 0 & opp_in_continent == 1) ~ -1,
                                            TRUE ~ 0)) %>% 
  dplyr::select('team','opp_team','stage_num','year','simulation',
                'diff_elo','avg_goals_scored','avg_opp_goals_taken','team_has_home_advt',
                'group_team','group_opp_team','match_num_third_place') %>% arrange(simulation)


##### Write csvs
write.csv(df_clean_final,paste0(path,'df_test_final_2018.csv'),row.names = FALSE) 
write.csv(df_clean_third,paste0(path,'df_test_third_place_2018.csv'),row.names = FALSE) 

