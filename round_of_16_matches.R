path <- '/Users/duda/Documents/World_Cup/'

countries_on_round_16 <- read.csv(paste0(path,'classification_round_16.csv'))

# Rank 1 is top, rank 2 is runner up 

# Having the teams going to the next ground, make a table filled with the round of 16 matches 

# Group 1 winner plays group 2 runner up (1,4)
# Group 3 winner plays group 4 runner up (5,8)
# Group 5 winner plays group 6 runner up (9,12)
# Group 7 winner plays group 8 runner up (13,16)
# 
# Group 1 runner up plays group 2 winner (2,3)
# Group 3 runner up plays group 4 winner (6,7)
# Group 5 runner up plays group 6 winner (10,11)
# Group 7 runner up plays group 8 winner (14,15)

# Organize which matches are going to happen in the group phase 12x8 (96 matches)
cols <- c('team', 'group_team','rank_team', 'opp_team','group_opp_team','rank_opp','match_num','simulation') 

df_matches <- data.frame(matrix(NA,
                                    nrow = 1,                        
                                    ncol = length(cols))) 
names(df_matches) <- cols

df_matches_final <- data.frame(matrix(NA,
                                nrow = 1,                        
                                ncol = length(cols))) 
names(df_matches_final) <- cols

n_matches <- 8 
seq_first_teams <- c(seq(1,13,4),seq(2,14,4))
seq_second_teams <- c(seq(4,16,4),seq(3,15,4))
n_simulations <- max(countries_on_round_16$simulation)

for(j in 1:n_simulations) {

  matches_simulation <- countries_on_round_16 %>% arrange(simulation,group,rank) %>% filter(simulation == j)

  for(i in 1:n_matches){
  
    df_matches[i,]$team <- matches_simulation[seq_first_teams[i],]$country
    df_matches[i,]$group_team <- matches_simulation[seq_first_teams[i],]$group
    df_matches[i,]$rank_team <- matches_simulation[seq_first_teams[i],]$rank
    
    df_matches[i,]$opp_team <- matches_simulation[seq_second_teams[i],]$country
    df_matches[i,]$group_opp_team <- matches_simulation[seq_second_teams[i],]$group
    df_matches[i,]$rank_opp <- matches_simulation[seq_second_teams[i],]$rank
    
    df_matches[i,]$match_num <- i
    df_matches[i,]$simulation <- j
    
  }

  df_matches_final <- rbind(df_matches_final,df_matches)

}

df_matches_final <- na.omit(df_matches_final)

write.csv(df_matches_final,paste0(path,'matches_round_of_16.csv'),row.names = FALSE)

# Table + rpois again 
# We can use df_clean since we already did source(simulation.R), but remember to change stage_num to 2

# Finish loop for round of 16 phase 

# (...)