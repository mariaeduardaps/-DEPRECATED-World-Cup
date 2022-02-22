path <- '/Users/duda/Documents/World_Cup/'

source(paste0(path,'clean_elo.R'))
elo_raw <- read_csv(paste0(path,'raw_data/elo_rankings_raw_year_bf.csv'))

# Get Elo scores
elo_df <- clean_elo(elo_raw)
elo_2017 <- elo_df %>% filter(Year == 2018)
elo_2017[elo_2017$country_name == 'South Korea',]$country_name <- "Korea Republic"

write.csv(elo_2017,paste0(path,'elo_2017.csv'),row.names = FALSE)