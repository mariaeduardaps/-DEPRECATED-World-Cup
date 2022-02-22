library(readr)
library(dplyr)

# Run code to get qualified teams
source('/Users/duda/Documents/World_Cup/qualifiers.R')

world_2018 <- read_csv('/Users/duda/Downloads/wc_2018.csv')
all_teams <- data.frame(country_name = unique(world_2018$team1))

# Getting elo scores
elo_raw <- read_csv('/Users/duda/Downloads/elo_rankings_raw_year_bf.csv')

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

elo_scores_2018 <- full_df %>% filter(Year == '2018')

df_merged <- merge(all_teams,elo_scores_2018,by = 'country_name',all.x = TRUE) %>% select(country_name,elo_score)

# Putting teams in their respective pots

# Host goes to pot 1
host <- 'Russia'
    
df_merged$pot <- rep(NA,32)

# Give the host some advantage so it is on the top
df_merged[df_merged$country_name == host,]$elo_score <- df_merged[df_merged$country_name == host,]$elo_score + 2000
df_merged <- df_merged %>% arrange(-elo_score)

df_merged[1:8,]$pot <- 1
df_merged[9:16,]$pot <- 2
df_merged[17:24,]$pot <- 3
df_merged[25:32,]$pot <- 4

# Need to add the confederations of each team before moving to the group draw 
country_continent <- read_csv('/Users/duda/Downloads/country_continent.csv')
df_with_continent <- merge(df_merged,country_continent,by.x = 'country_name',by.y = 'country',all.x = TRUE) 
df_with_continent$group <- rep(NA,32)

# Starting with pot 1

# Allocating Russia necessarily on group A
df_with_continent[df_with_continent$country_name == host,]$group <- 1
df_with_continent <- df_with_continent %>% arrange(-elo_score)

# Sampling who is going to group B - H 
order_allocation <- sample(2:8,size = 7, rep = FALSE)

for(i in 2:8){
  df_with_continent[i,]$group <- order_allocation[i-1]
}

# Moving to pot 2

for(pot in 1:3){
  
order_allocation <- sample(1:8,size = 8, rep = FALSE)

  for(i in 1:8){
    
    group_selected <- order_allocation[i]
    
    # Check where which team would be added to this group
    continent_team_selected <- df_with_continent[i + 8*pot,]$continent
    
    # Check how many teams we already have from that continent in this group
    count <- nrow(df_with_continent %>% filter(group == group_selected & continent == continent_team_selected))
    
    # If the continent is Europe and the count is 2, we can't put that team in that group
    if(continent_team_selected == 'Europe' & count == 2){
      
      
      
    }
    
    if(continent_team_selected != 'Europe' & count == 1){
      
      
      
    } 
    
    # If the continent is not Europe and the count is 1, we can't put that team in that group 
    
    
    # If everything is ok, we can run the line below and allocate
    df_with_continent[i + 8*pot,]$group <- order_allocation[i]
    
    # We need to check for the conditions here!
    
    
  }

}

# Sampling who is going to group A, checking if that team will increase the count of same continent to 2 and 
# the count to Europe to 3. If so, check if that team can go to the group that comes after, until finding one. When finding one
# going back to the group it stopped and sampling again


    