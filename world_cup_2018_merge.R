stages <- read_csv('/Users/duda/Downloads/team1_team2_v2.csv')
matches <- read_csv('/Users/duda/Downloads/stages_fixed_2018.csv')

# Change date format in matches

matches[matches$team1 == 'Korea Republic',]$team1 <- 'South Korea'
matches[matches$team2 == 'Korea Republic',]$team2 <- 'South Korea'

matches$date <- strptime(as.character(matches$date), "%d/%m/%Y")
stages$date <- strptime(as.character(stages$date), "%Y-%m-%d")

df_2018 <- merge(matches,stages,by = c('date','team1','team2'),all.x = TRUE)

