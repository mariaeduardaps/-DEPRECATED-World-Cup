# Getting 2022 data
source('/Users/duda/Documents/World_Cup/soccer_shuffling_groups_v2.R')
source('/Users/duda/Documents/World_Cup/clean_elo.R')

n_simulations <- 10
group_df <- get_n_groups(qualified_teams,n_simulations)

# Get datasets 
elo_raw <- read_csv('/Users/duda/Downloads/elo_rankings_raw_year_bf.csv')

# Get Elo scores
elo_df <- clean_elo(elo_raw)
elo_2021 <- elo_df %>% filter(Year == 2021)
elo_2021[elo_2021$country_name == 'South Korea',]$country_name <- "Korea Republic"

# Get Host continent
host_continent <- 'Asia'

df_with_elos <- merge(group_df,elo_2021,by.x = 'country',by.y = 'country_name')
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
  
  for (i in 1:8) {
    # print(i)
    group <- group_df %>% filter(group == i, simulation == j)
    df <- expand.grid(group$country,group$country)
    df <- df[df$Var1 != df$Var2,]
    names(df)[1] <- 'team'
    names(df)[2] <- 'opp_team'
    df$stage_num <- rep(stage,12)
    df$group <- rep(i,12)
    df$simulation <- rep(j,12)
    df_group_stage <- rbind(df_group_stage,df)
  }
  
}

df_group_stage <- na.omit(df_group_stage)

# dim(df_group_stage) is indeed 96*n_simulations (checked)

df_group_stage$year <- rep(2022,nrow(df_group_stage))
df_intermediary <- merge(df_group_stage,df_with_elos %>% dplyr::select('country','score','continent','elo_score','num_matches',
                                                                       'goals_for','goals_against','host_continent'),by.x = 'team',by.y = 'country',
                         all.x=TRUE) 


names(df_intermediary)[8] <- 'team_continent'
df_intermediary_with_avg_team <- df_intermediary %>% mutate(avg_goals_team = goals_for/num_matches)

df_with_opp <- merge(df_intermediary_with_avg_team,df_with_elos %>% dplyr::select('country','score','continent','elo_score','num_matches',
                                                                                  'goals_for','goals_against','host_continent'),
                     by.x = 'opp_team',by.y = 'country',all.x = TRUE)

names(df_with_opp)[16] <- 'opp_continent'

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

# Getting the model 
model_sim <- readRDS('/Users/duda/final_model')
summary(model_sim)
coeffs <- model_sim$coefficients

# Running the predictions for the simulated matches
predictions_sim <- rep(NA,nrow(df_clean))

for(i in 1:nrow(df_clean)) {
  predictions_sim[i] <- rpois(1,exp(coeffs[1] + coeffs[2]*df_clean$year[i] + coeffs[3]*df_clean$diff_elo[i] + 
                                      coeffs[4]*df_clean$avg_goals_scored[i] + coeffs[5]*df_clean$avg_opp_goals_taken[i] + 
                                      coeffs[6]*df_clean$stage_num[i] + coeffs[7]*df_clean$team_has_home_advt[i]))
}

# Why rpois only returns integers? Check why this is so!

preds_df <- na.omit(cbind(df_clean,predictions_sim))

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
winner_pred_df <- combined_pred_opp %>% mutate(team_with_opp = gsub(" ","",paste(tolower(team),tolower(opp_team),stage_num,simulation)))

# Transform the 96 lines into the actual 48 matches 
str_arrange <- function(x){
  x %>%
    stringr::str_split("") %>% # Split string into letters
    purrr::map(~sort(.) %>% paste(collapse = "")) %>% # Sort and re-combine
    as_vector() # Convert list into vector
}

winner_pred_df$sorted_names <- unlist(lapply(winner_pred_df$team_with_opp,str_arrange))
winner_pred_df <- winner_pred_df[!duplicated(winner_pred_df$sorted_names),]

# The dim here should be 48*n_simulation lines (checked!)

# With preds_df winner and loser, calculate team points 
# Win is worth 3 points, draw is worth 1 point, loss is worth 0 points
team_df <- group_df %>% dplyr::select('country','group','simulation') %>% arrange(simulation,group)
team_df$points <- rep(0,nrow(team_df))
team_df$goals_scored <- rep(0,nrow(team_df))
team_df$goals_taken <- rep(0,nrow(team_df))

for(j in 1:n_simulations){
  
  # NEED TO CREATE A NEW DF HERE FILTERING FOR SIMULATION
  winner_pred_df_filtered <- winner_pred_df %>% filter(simulation == j)
  
  for(i in 1:nrow(winner_pred_df_filtered)){
   
    goals_team <- winner_pred_df_filtered[i,]$predictions_sim
    goals_opp <- winner_pred_df_filtered[i,]$pred_for_opp_team
    team <- winner_pred_df_filtered[i,]$team
    opp <- winner_pred_df_filtered[i,]$opp_team
    selected_simulation <- j
    
    if(goals_team > goals_opp) {
      team_df[team_df$country == team & team_df$simulation == selected_simulation,]$points <- team_df[team_df$country == team & team_df$simulation == selected_simulation,]$points + 3
      team_df[team_df$country == opp & team_df$simulation == selected_simulation,]$points <- team_df[team_df$country == opp & team_df$simulation == selected_simulation,]$points + 0
    }
    
    if(goals_team < goals_opp) {
      team_df[team_df$country == opp & team_df$simulation == selected_simulation,]$points <- team_df[team_df$country == opp & team_df$simulation == selected_simulation,]$points + 3
      team_df[team_df$country == team & team_df$simulation == selected_simulation,]$points <- team_df[team_df$country == team & team_df$simulation == selected_simulation,]$points + 0
    } 
    
    if(goals_team == goals_opp) {
      team_df[team_df$country == opp & team_df$simulation == selected_simulation,]$points <- team_df[team_df$country == opp & team_df$simulation == selected_simulation,]$points + 1
      team_df[team_df$country == team & team_df$simulation == selected_simulation,]$points <- team_df[team_df$country == team & team_df$simulation == selected_simulation,]$points + 1
    }
    
    team_df[team_df$country == team & team_df$simulation == selected_simulation,]$goals_scored <- team_df[team_df$country == team & team_df$simulation == selected_simulation,]$goals_scored + goals_team
    team_df[team_df$country == team & team_df$simulation == selected_simulation,]$goals_taken <- team_df[team_df$country == team & team_df$simulation == selected_simulation,]$goals_taken + goals_opp
    
    team_df[team_df$country == opp & team_df$simulation == selected_simulation,]$goals_scored <- team_df[team_df$country == opp & team_df$simulation == selected_simulation,]$goals_scored + goals_opp
    team_df[team_df$country == opp & team_df$simulation == selected_simulation,]$goals_taken <- team_df[team_df$country == opp & team_df$simulation == selected_simulation,]$goals_taken + goals_team
    
  }
  
  
}

# With group points, define which are the two teams going to the next round

rank_df <- team_df %>% arrange(simulation,group,-points) %>% group_by(simulation,group) %>% 
  mutate(rank = min_rank(-points)) %>%
  mutate(diff_goals = goals_scored - goals_taken) %>%
  filter(rank <= 2) %>%
  # Check for ties
  group_by(simulation,group) %>%
  mutate(count_candidates = n(),
         max_rank = max(rank))

# Ready to go
rank_df_without_ties <- rank_df %>% filter(count_candidates == 2 & max_rank == 2)

# In the case of ties, we need to check the difference between goals scored and goals taken

rank_df_tie_treatment <- rank_df %>% 
  filter(!(count_candidates == 2 & max_rank == 2)) %>%
  group_by(simulation,group) %>%
  mutate(n_distinct_ranks = n_distinct(rank)) %>%
  mutate(rank_per_goals = case_when(n_distinct_ranks == 1 ~ as.integer(min_rank(-diff_goals)),
                                    TRUE ~ as.integer(ifelse(rank == 1,1,min_rank(-diff_goals)+1)))) %>%
  mutate(old_rank = rank) %>%
  mutate(rank = dense_rank(rank_per_goals)) %>%
  ungroup %>%
  filter(rank %in% c(1,2)) %>%
  dplyr::select(country,group,simulation,points,goals_scored,goals_taken,rank,diff_goals)


# If we have situations of two first places and one second place, we want to filter to get only the two first places
rank_df_tie_treatment_flag <- rank_df_tie_treatment %>%
  group_by(simulation,group) %>%
  mutate(rank_1_count = sum(rank == 1),
         rank_2_count = sum(rank == 2)) %>%
  mutate(remove_flag = ifelse(rank_1_count >= 2 & rank_2_count != 0 & rank_1_count > rank_2_count & rank == 2,1,0)) %>% 
  filter(remove_flag == 0) %>%
  dplyr::select(country,group,simulation,points,goals_scored,goals_taken,rank,diff_goals)
  
  
new_df_rank <- rbind(rank_df_without_ties,rank_df_tie_treatment_flag) %>% dplyr::select(country,group,points,
                                                                                   goals_scored,goals_taken,rank,diff_goals,simulation)

# Check if there are still ties, if so, we are checking for the total number of goals scored 
candidates_left_check <- new_df_rank %>% group_by(simulation,group) %>% 
  mutate(count_candidates_left = n(),
         distinct_ranks = n_distinct(rank)) %>%
  filter(count_candidates_left > 2 | distinct_ranks == 1) # Either we have more than 2 candidates or the candidates are all from the same rank
# for example, two candidates for #1 

# Continue fix from here

if(nrow(candidates_left_check) != 0) {
  
  candidates_ready <- new_df_rank %>% group_by(simulation,group) %>% 
    mutate(count_candidates_left = n(),
           distinct_ranks = n_distinct(rank)) %>%
    filter(count_candidates_left == 2 & distinct_ranks == 2)
  
  candidates_left_df <- candidates_left_check %>% filter(!(count_candidates_left == 2 & distinct_ranks == 2)) %>%
    group_by(simulation,group) %>%
    mutate(rank_per_goals_scored = case_when(distinct_ranks == 1 ~ as.integer(min_rank(-goals_scored)),
                                      TRUE ~ as.integer(ifelse(rank == 1,1,min_rank(-goals_scored) + 1)))) %>%
    mutate(rank = dense_rank(rank_per_goals_scored)) %>%
    filter(rank %in% c(1,2))
  
  final_df <- rbind(candidates_ready,candidates_left_df) %>% dplyr::select(country,simulation,group,points,
                                                                           goals_scored,goals_taken,diff_goals,rank)
  
} else {
  final_df <- new_df_rank
}

# If final df STILL has ties, the rules are too subtle (e.g. Fair play) so we are just sampling and getting one of the teams

candidates_left_final_check <- final_df %>% group_by(simulation,group) %>% 
  mutate(count_candidates_left = n(),
  distinct_ranks = n_distinct(rank)) %>%
  filter(count_candidates_left > 2 | distinct_ranks == 1)

if(nrow(candidates_left_final_check) != 0) {
  
  candidates_ready_final <- final_df %>% group_by(simulation,group) %>% 
    mutate(count_candidates_left = n(),
           distinct_ranks = n_distinct(rank)) %>%
    filter(count_candidates_left == 2 & distinct_ranks == 2)
  
  candidates_left_df_row_order <- candidates_left_final_check %>% 
    filter(!(count_candidates_left == 2 & distinct_ranks == 2)) %>%
    group_by(simulation,group) %>% 
    arrange(simulation,group,country) %>% 
    mutate(rank_per_row_order = case_when(distinct_ranks == 1 ~ row_number(),
                                             TRUE ~ as.integer(ifelse(rank == 1,1,row_number() + 1)))) %>%
    mutate(rank = dense_rank(rank_per_row_order)) %>%
    filter(rank %in% c(1,2))
  
  df_clean_final <- rbind(candidates_ready_final,candidates_left_df_row_order) %>% dplyr::select(country,simulation,group,points,
                                                                           goals_scored,goals_taken,diff_goals,rank)
  
} else {
  df_clean_final <- final_df
}

# Stop this .R code maybe here and start a new one for the following phases

# Check dim - this final df should have 16*n_simulation lines! Which are the 16 countries in each simulation that are going to the next round

write.csv(df_clean_final,'/Users/duda/Documents/World_Cup/classification_round_16.csv',row.names = FALSE)






