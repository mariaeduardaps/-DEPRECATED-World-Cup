path <- '/Users/duda/Documents/World_Cup/'
# Get the FIFA 2021 scores from the teams with the confederations
library(dplyr)

fifa_data <- read.csv(paste0(path,'fifa_rankings_2021.csv'))

# Filter for the best teams of each confederation

uefa <- fifa_data %>% filter(confederation == 'UEFA') %>% head(13)
conmebol <- fifa_data %>% filter(confederation == 'CONMEBOL') %>% head(5)
afc <- fifa_data %>% filter(confederation == 'AFC') %>% head(6)
caf <- fifa_data %>% filter(confederation == 'CAF') %>% head(5)
concacaf <- fifa_data %>% filter(confederation == 'CONCACAF') %>% head(4)
ofc <- fifa_data %>% filter(confederation == 'OFC') %>% head(1)

# Get the qualified teams and think about intercontinental playoffs
# 1 team from CONMEBOL
# 1 team from AFC
# 1 team from OFC
# 1 team from CONCACAF
intercontinental_playoffs <- rbind(tail(conmebol,1),
                                   tail(afc,1),
                                   tail(ofc,1),
                                   tail(concacaf,1)) %>% arrange(-score)

selected_playoffs <- head(intercontinental_playoffs,2)

qualified_teams <- rbind(
  uefa,
  conmebol[1:4,],
  afc[1:5,],
  caf,
  concacaf[1:3,],
  selected_playoffs
)

write.csv(qualified_teams,paste0(path,'qualified_teams.csv'),row.names = FALSE)


