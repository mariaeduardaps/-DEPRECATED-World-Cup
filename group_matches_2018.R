path = '/Users/duda/Documents/World_Cup/'

elo_2021 <- read_csv(paste0(path,'elo_2017.csv'))
group_df <- read.csv("/Users/duda/Downloads/group_df_2018_right.csv")
country_continent <- read.csv(paste0(path,'raw_data/country_continent.csv'))

elo_2021[elo_2021$country_name == 'Korea Republic', ]$country_name <- 'South Korea'

# Get continent of countries

group_df <- merge(group_df,country_continent,by.x = 'team',by.y = 'country',all.x = TRUE)

# Rename columns 
names(group_df)[1] <- 'country'

# Duplicate to create a thousand simulations 
cols <- c('country', 'group', 'continent','simulation') 

stacked_df <- data.frame(matrix(NA,
                                    nrow = 1,                        
                                    ncol = length(cols))) 
names(stacked_df) <- cols

n_simulations <- 1000

for(i in 1:n_simulations){
  
  simulation <- rep(i,32)
  working_df <- cbind(group_df,simulation)
  stacked_df <- rbind(stacked_df,working_df)
  
}

stacked_df <- na.omit(stacked_df)

# Back to naming it group df
group_df <- stacked_df

# Get Host continent
host_continent <- 'Asia'

df_with_elos <- merge(group_df,elo_2021,by.x = 'country',by.y = 'country_name',all.x = TRUE)
df_with_elos$host_continent <- rep(host_continent,nrow(df_with_elos))
df_with_elos <- df_with_elos %>% filter(simulation == 1) # We just need 1 simulation to get the team elos and match data

# Organize which matches are going to happen in the group phase 12x8 (96 matches)
cols <- c('team', 'opp_team', 'stage_num','group','simulation') 

df_group_stage <- data.frame(matrix(NA,
                                    nrow = 1,                        
                                    ncol = length(cols))) 
names(df_group_stage) <- cols
stage <- 1

for(j in 1:n_simulations){
  
  # print(paste0('Simulation: ', j))
  
  for (i in 1:8) {
    
    # print(paste0('Group: ', i))
    group <- group_df %>% filter(group == i, simulation == j)
    df <- expand.grid(group$country,group$country)
    df <- df[df$Var1 != df$Var2,]
    names(df)[1] <- 'team'
    names(df)[2] <- 'opp_team'
    
    # print(paste0('Nrows df: ',nrow(df)))
    
    df$stage_num <- rep(stage,12)
    df$group <- rep(i,12)
    df$simulation <- rep(j,12)
    df_group_stage <- rbind(df_group_stage,df)
  }
  
}

df_group_stage <- na.omit(df_group_stage)

# dim(df_group_stage) is indeed 96*n_simulations (checked)

df_group_stage$year <- rep(2018,nrow(df_group_stage))
df_intermediary <- merge(df_group_stage,df_with_elos %>% dplyr::select('country','continent','elo_score','num_matches',
                                                                       'goals_for','goals_against','host_continent'),by.x = 'team',by.y = 'country',
                         all.x=TRUE) 


names(df_intermediary)[7] <- 'team_continent'
df_intermediary_with_avg_team <- df_intermediary %>% mutate(avg_goals_team = goals_for/num_matches)

df_with_opp <- merge(df_intermediary_with_avg_team,df_with_elos %>% dplyr::select('country','continent','elo_score','num_matches',
                                                                                  'goals_for','goals_against','host_continent'),
                     by.x = 'opp_team',by.y = 'country',all.x = TRUE)

names(df_with_opp)[14] <- 'opp_continent'

df_with_opp <- df_with_opp %>% mutate(avg_goals_taken_opp = goals_against.y/num_matches.y)

# Organize in a df exactly like the train data that goes into the model 

df_clean <- df_with_opp %>% mutate(year = year,
                                   diff_elo = elo_score.x - elo_score.y,
                                   avg_goals_scored = avg_goals_team,
                                   avg_opp_goals_taken = avg_goals_taken_opp,
                                   stage_num = stage_num,
                                   simulation = simulation,
                                   team_in_continent = ifelse(team_continent == host_continent.x,1,0),
                                   opp_in_continent = ifelse(opp_continent == host_continent.x,1,0)
) %>% mutate(team_has_home_advt = case_when((team_in_continent == 1 & opp_in_continent == 0) ~ 1,
                                            (team_in_continent == 0 & opp_in_continent == 1) ~ -1,
                                            TRUE ~ 0)) %>% dplyr::select('team','opp_team','stage_num','group','year','simulation','diff_elo','avg_goals_scored','avg_opp_goals_taken','team_has_home_advt')

write.csv(df_clean,paste0(path,'df_group_phase_2018.csv'),row.names = FALSE)