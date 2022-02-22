library(readr)
library(dplyr)

# Run code to get qualified teams
# source('/Users/duda/Documents/World_Cup/qualifiers.R')

# Putting teams in their respective pots

# Host goes to pot 1
host <- 'Qatar'

qualified_teams$pot <- rep(NA,32)

# Give the host some advantage so it is on the top
qualified_teams[qualified_teams$country == host,]$score <- qualified_teams[qualified_teams$country == host,]$score + 2000
qualified_teams <- qualified_teams %>% arrange(-score)

qualified_teams[1:8,]$pot <- 1
qualified_teams[9:16,]$pot <- 2
qualified_teams[17:24,]$pot <- 3
qualified_teams[25:32,]$pot <- 4

qualified_teams$group <- rep(NA,32)

draw_group <- function(qualified_teams){
  
  # Starting with pot 1
  
  # Allocating Russia necessarily on group A
  qualified_teams[qualified_teams$country == host,]$group <- 1
  qualified_teams <- qualified_teams %>% arrange(-score)
  
  # Sampling who is going to group B - H 
  order_allocation <- sample(2:8,size = 7, rep = FALSE)
  
  for(i in 2:8){
    qualified_teams[i,]$group <- order_allocation[i-1]
  }
  
  # Moving to pot 2
  
  for(selected_pot in 2:4){
    
    order_allocation <- sample(1:8,size = 8, rep = FALSE)
    
    for(i in 1:8){
      
      cols <- c('j', 'count_countries_from_pot_x_in_group') 
      
      # Initialize our data.frame:
      df_cnt <- data.frame(matrix(NA,
                                   nrow = 1,                        
                                   ncol = length(cols))) 
      names(df_cnt) <- cols
      
      for(j in 1:8){
      
        list_count <- (qualified_teams %>% filter(pot == selected_pot) %>% .$group) == j
        list_count[is.na(list_count)] <- FALSE
        count_countries_from_pot_x_in_group <- sum(list_count)
        df_cnt <- rbind(df_cnt,cbind(j,count_countries_from_pot_x_in_group))
      
      }
      
      df_cnt <- na.omit(df_cnt)
      
      # The goal is to always select the min group that has cnt = 0 (e.g. still doesn't have countries from the selected pot)
      group_selected <- min(df_cnt %>% filter(count_countries_from_pot_x_in_group == 0) %>% .$j)
      
      # if(jumps == 0) {
      #   group_selected <- i
      # } else {
      #   group_selected <- i - jumps
      # }
      
      team_selected <- order_allocation[i]
      
      # Check where which team would be added to this group
      team_selected_df <- qualified_teams %>% filter(pot == selected_pot)
      team_selected_line <- team_selected_df[team_selected,]
      
      # Check how many teams we already have from that confederation in this group
      count <- nrow(qualified_teams %>% filter(group == group_selected & confederation == team_selected_line$confederation))
      
      # If the confederation is UEFA and the count is 2, we can't put that team in that group
      if(team_selected_line$confederation == 'UEFA' & count == 2){
        
        next_group <- group_selected
        
        # We need to keep searching for either when group is full or when the conditions are not met
        while(nrow(qualified_teams %>% filter(group == next_group)) == 4 | count >= 2){
          next_group <- next_group + 1
          
          if(next_group > 8) break
          
          count <- nrow(qualified_teams %>% filter(group == next_group & confederation == team_selected_line$confederation))
        }
        
        # We then know the team should go to the variable next group
        qualified_teams[qualified_teams$country == team_selected_line$country,]$group <- next_group
        
        
      }
      
      # If the continent is not Europe and the count is 1, we can't put that team in that group 
      if(team_selected_line$confederation != 'UEFA' & count == 1){
        
        next_group <- group_selected
          
        # We need to keep searching for either when group is full or when the conditions are not met
        while(nrow(qualified_teams %>% filter(group == next_group)) == 4 | count >= 1){
          next_group <- next_group + 1
          
          if(next_group > 8) break
          
          count <- nrow(qualified_teams %>% filter(group == next_group & confederation == team_selected_line$confederation))
        }
          
        # Need to create condition for when next group > 8
          
        # We then know the team should go to the variable next group
        qualified_teams[qualified_teams$country == team_selected_line$country,]$group <- next_group
        
        
      } else {
        
      # If everything is ok, we can run the line below and allocate
      qualified_teams[qualified_teams$country == team_selected_line$country,]$group <- group_selected
    
      
      }
      
      
    }
    
  }
  
  # Fix the advantage we had previously added to the host 
  qualified_teams[qualified_teams$country == host,]$score <- qualified_teams[qualified_teams$country == host,]$score - 2000
  
  return(qualified_teams)

}


get_n_groups <- function(qualified_teams,n){
  n_groups <- 0
  cols <- c('country', 'score','continent','confederation','pot','group','simulation') 
  
  # Initialize our data.frame:
  df_final <- data.frame(matrix(NA,
                              nrow = 1,                        
                              ncol = length(cols))) 
  names(df_final) <- cols
  
  while(n_groups < n){
    
    df <- draw_group(qualified_teams)
    
    check <- df %>% group_by(group) %>% summarize(cnt = n()) %>% 
      mutate(cnt_check = ifelse(cnt == 4,1,0)) %>% ungroup %>% 
      summarize(sum_cnt = sum(as.numeric(cnt_check))) %>% .$sum_cnt
    
    print(check)
    
    if((sum(df$group == 9) != 0) | (check != 8)) {
      n_groups <- n_groups
    } else {
      n_groups <- n_groups + 1
      df$simulation <- rep(n_groups,32)
      df_final <- rbind(df_final,df)
    }
    
  }
  
  df_final <- na.omit(df_final)
  return(df_final)
}

teams <- get_n_groups(qualified_teams,150)


