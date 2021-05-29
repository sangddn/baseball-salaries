salaries <- read.csv('../raw_data/Salaries.csv', header = T, stringsAsFactors = F) %>%
  dplyr::select(yearID, playerID, teamID, salary) %>% filter_dat()
inflation <- read.csv('../raw_data/Inflation.csv', header = T, stringsAsFactors = F)
salaries <- adjust_salaries(salaries, inflation)

pitch <- read.csv('../raw_data/Pitching.csv', header = T, stringsAsFactors = F) %>%
  dplyr::select(yearID, playerID, teamID, ERA) %>% filter_dat()
pitch <- adjust_ERAs(pitch)

processed <- dplyr::left_join(pitch, salaries, by = c('year' = 'year', 'player' = 'player', 'team' = 'team'))
write.csv(drop_na(processed), '../processed_data/processed.csv', row.names = F)