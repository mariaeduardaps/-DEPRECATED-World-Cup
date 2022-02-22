path <- '/Users/duda/Documents/World_Cup/'

quarter_matches <- read.csv(paste0(path,'quarter_matches.csv'))
stage_num_selected <- 3
elo_2021 <- read_csv(paste0(path,'elo_2021.csv'))
country_continent <- read_csv(paste0(path,'/raw_data/country_continent.csv'))

# Get Host continent
host_continent <- 'Asia'

# Duplicate each match so that we have one line for team + opp_team and another line for opp_team + team 

for(i in 1:nrow(quarter_matches)){
  
  team <- quarter_matches[i,]$opp_team 
  group_team <- quarter_matches[i,]$group_opp_team
  opp_team <- quarter_matches[i,]$team 
  group_opp_team <- quarter_matches[i,]$group_team
  match_num_quarter <- quarter_matches[i,]$match_num_quarter
  simulation <- quarter_matches[i,]$simulation
  quarter_matches <- rbind(quarter_matches,cbind(team,group_team,
                                                   opp_team,group_opp_team,match_num_quarter,simulation))
  
}

# dim(quarter_matches) here should have 8*n_simulation lines 
# (4 matches duplicated for each simulation) - checked!

# Merge the continents with the country column

df_team <- merge(quarter_matches,country_continent,by.x = 'team', by.y = 'country',all.x = TRUE) %>%
  rename(team_continent = continent)
df_with_elos <-  merge(df_team,country_continent,by.x = 'opp_team', by.y = 'country',all.x = TRUE) %>%
  rename(opp_continent = continent)

df_with_elos$host_continent <- rep(host_continent,nrow(df_with_elos))

df_with_elos$stage_num <- rep(stage_num_selected,nrow(df_with_elos))

# dim(matches_round_16)

merge_elo_team <- merge(df_with_elos,elo_2021,by.x = 'team',by.y = 'country_name',all.x = TRUE)

merge_elo_all <- merge(merge_elo_team,elo_2021,by.x = 'opp_team',by.y ='country_name',all.x = TRUE)

# Organize in a df exactly like the train data that goes into the model 

df_clean_quarter_finals <- merge_elo_all %>% mutate(year = Year.x,
                                              diff_elo = elo_score.x - elo_score.y,
                                              avg_goals_scored = goals_for.x/num_matches.x,
                                              avg_opp_goals_taken = goals_against.y/num_matches.y,
                                              stage_num = stage_num,
                                              simulation = simulation,
                                              group_team = group_team,
                                              group_opp = group_opp_team,
                                              team_in_continent = ifelse(team_continent == host_continent,1,0),
                                              opp_in_continent = ifelse(opp_continent == host_continent,1,0)
) %>% mutate(team_has_home_advt = case_when((team_in_continent == 1 & opp_in_continent == 0) ~ 1,
                                            (team_in_continent == 0 & opp_in_continent == 1) ~ -1,
                                            TRUE ~ 0)) %>% 
  dplyr::select('team','opp_team','stage_num','year','simulation',
                'diff_elo','avg_goals_scored','avg_opp_goals_taken','team_has_home_advt',
                'group_team','group_opp_team','match_num_quarter') %>% arrange(simulation)

write.csv(df_clean_quarter_finals,paste0(path,'df_test_quarter_finals.csv'),row.names = FALSE) 