path <- '/Users/duda/Documents/World_Cup/'

matches_round_16 <- read.csv(paste0(path,'matches_round_of_16.csv'))
elo_2021 <- read_csv(paste0(path,'elo_2021.csv'))
country_continent <- read_csv(paste0(path,'/raw_data/country_continent.csv'))
stage_num_selected <- 2

# Get Host continent
host_continent <- 'Asia'

# Duplicate each match so that we have one line for team + opp_team and another line for opp_team + team 

for(i in 1:nrow(matches_round_16)){
  
  team <- matches_round_16[i,]$opp_team 
  group_team <- matches_round_16[i,]$group_opp_team
  rank_team <- matches_round_16[i,]$rank_opp
  opp_team <- matches_round_16[i,]$team 
  group_opp_team <- matches_round_16[i,]$group_team
  rank_opp <- matches_round_16[i,]$rank_team
  match_num <- matches_round_16[i,]$match_num
  simulation <- matches_round_16[i,]$simulation
  matches_round_16 <- rbind(matches_round_16,cbind(team,group_team,rank_team,
                               opp_team,group_opp_team,rank_opp,match_num,simulation))
  
}

# dim(matches_round_16) here should have 16*n_simulation lines 
# (8 matches duplicated for each simulation) - checked!

# Merge the continents with the country column

df_team <- merge(matches_round_16,country_continent,by.x = 'team', by.y = 'country',all.x = TRUE) %>%
  rename(team_continent = continent)
df_with_elos <-  merge(df_team,country_continent,by.x = 'opp_team', by.y = 'country',all.x = TRUE) %>%
  rename(opp_continent = continent)

df_with_elos$host_continent <- rep(host_continent,nrow(df_with_elos))

df_with_elos$stage_num <- rep(stage_num_selected,nrow(df_with_elos))

# dim(matches_round_16)

merge_elo_team <- merge(df_with_elos,elo_2021,by.x = 'team',by.y = 'country_name',all.x = TRUE)

merge_elo_all <- merge(merge_elo_team,elo_2021,by.x = 'opp_team',by.y ='country_name',all.x = TRUE)

# Organize in a df exactly like the train data that goes into the model 

df_clean_round_16 <- merge_elo_all %>% mutate(year = Year.x,
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
                'group_team','group_opp_team','match_num')

write.csv(df_clean_round_16,paste0(path,'df_test_round_16.csv'),row.names = FALSE)


