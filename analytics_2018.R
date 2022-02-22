path <- '/Users/duda/Documents/World_Cup/'

group_df <- read.csv("/Users/duda/Downloads/group_df_2018_right.csv")

all_teams <- group_df$team
teams_in_round_of_16 <- read.csv(paste0(path,'classification_round_16_2018.csv'))
teams_in_quarter <- read.csv(paste0(path,'quarter_matches_2018.csv'))
teams_in_semi <- read.csv(paste0(path,'matches_semi_2018.csv'))
teams_in_final <- read.csv(paste0(path,'final_matches_2018.csv'))
champions <- read.csv(paste0(path,'champions_2018.csv'))

cols <- c('team', 'round_16','quarter','semi','final','champion') 

df_teams_analytics <- data.frame(matrix(0,
                                        nrow = 32,                        
                                        ncol = length(cols))) 
names(df_teams_analytics) <- cols
df_teams_analytics$team <- all_teams


n_teams <- 32

n_simulations <- max(teams_in_round_of_16$simulation)


for(j in 1:n_simulations){
  
  working_df <- teams_in_round_of_16 %>% filter(simulation == j)
  teams_in_round <- unique(working_df$country)
  
  for(i in 1:n_teams){
    
    team_selected <- df_teams_analytics[i,]$team
    if(team_selected %in% teams_in_round){
      df_teams_analytics[i,]$round_16 <- df_teams_analytics[i,]$round_16 + 1
    }
    
  }
  
}

# Quarter finals

for(j in 1:n_simulations){
  
  working_df <- teams_in_quarter %>% filter(simulation == j)
  teams_in_round <- unique(c(working_df$team,working_df$opp_team))
  
  for(i in 1:n_teams){
    
    team_selected <- df_teams_analytics[i,]$team
    if(team_selected %in% teams_in_round){
      df_teams_analytics[i,]$quarter <- df_teams_analytics[i,]$quarter + 1
    }
    
  }
  
}

# Semi 

for(j in 1:n_simulations){
  
  working_df <- teams_in_semi %>% filter(simulation == j)
  teams_in_round <- unique(c(working_df$team,working_df$opp_team))
  
  for(i in 1:n_teams){
    
    team_selected <- df_teams_analytics[i,]$team
    if(team_selected %in% teams_in_round){
      df_teams_analytics[i,]$semi <- df_teams_analytics[i,]$semi + 1
    }
    
  }
  
}

# Final

for(j in 1:n_simulations){
  
  working_df <- teams_in_final %>% filter(simulation == j)
  teams_in_round <- unique(c(working_df$team,working_df$opp_team))
  
  for(i in 1:n_teams){
    
    team_selected <- df_teams_analytics[i,]$team
    if(team_selected %in% teams_in_round){
      df_teams_analytics[i,]$final <- df_teams_analytics[i,]$final + 1
    }
    
  }
  
}

# Champion 
for(j in 1:n_simulations){
  
  working_df <- champions %>% filter(simulation == j)
  teams_in_round <- unique(working_df$winner)
  
  for(i in 1:n_teams){
    
    team_selected <- df_teams_analytics[i,]$team
    if(team_selected %in% teams_in_round){
      df_teams_analytics[i,]$champion <- df_teams_analytics[i,]$champion + 1
    }
    
  }
  
}

# Organizing df_teams_analytics 
df_teams_analytics_prop <- cbind(df_teams_analytics$team,
                                 df_teams_analytics[,-1]/n_simulations)

ordered_df <- df_teams_analytics_prop %>% arrange(-round_16)

write.csv(ordered_df,paste0(path,'ordered_df_2018_fixed.csv'),row.names = FALSE)

elo_2021 <- read.csv(paste0(path,'elo_2021.csv'))


