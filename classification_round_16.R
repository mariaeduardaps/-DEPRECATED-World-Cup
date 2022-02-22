path <- '/Users/duda/Documents/World_Cup/'

# We need group_df and winner_pred_df
group_df <- read.csv(paste0(path,'group_df.csv'))
winner_pred_df <- read.csv(paste0(path,'winner_pred_df.csv'))

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

# Need to fix cases when we have two first places (count candidates > 2 and max_rank is 2)
final_df_fixed_ready <- final_df %>% group_by(simulation,group) %>% 
  mutate(count_candidates_left = n(),
         max_rank = max(rank)) %>%
  filter(!(count_candidates_left > 2 & max_rank == 2))

final_df_fixed_flag <- final_df %>% group_by(simulation,group) %>% 
  mutate(count_candidates_left = n(),
         max_rank = max(rank)) %>%
  filter(count_candidates_left > 2 & max_rank == 2) %>%
  group_by(simulation,group) %>%
  mutate(rank_1_count = sum(rank == 1),
         rank_2_count = sum(rank == 2)) %>%
  mutate(remove_flag = ifelse(rank_1_count >= 2 & rank_2_count != 0 & rank_1_count >= rank_2_count & rank == 2,1,0)) %>% 
  ungroup %>% 
  filter(remove_flag == 0) 

final_df_fixed <- rbind(final_df_fixed_ready,final_df_fixed_flag) %>% dplyr::select(country,group,points,
                                                                                           goals_scored,goals_taken,rank,diff_goals,simulation)

# If final df STILL has ties, the rules are too subtle (e.g. Fair play) so we are just sampling and getting one of the teams

candidates_left_final_check <- final_df_fixed %>% group_by(simulation,group) %>% 
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

write.csv(df_clean_final,paste0(path,'classification_round_16.csv'),row.names = FALSE)