library(readr)
library(dplyr)

elo_raw <- read_csv('/Users/duda/Downloads/elo_rankings_raw_year_bf.csv')

clean_elo <- function(elo_raw) {
  
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
  
  return(full_df)

}

clean_elo(elo_raw)


