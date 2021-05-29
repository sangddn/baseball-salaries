#Pay for Pitcher at qth tile over Time
qtile_pitcher <- function(dat, duration, qtile) {
  mp <- dat %>% filter(year %in% duration, between(adjERA, qtile - 0.01, qtile + 0.01)) %>%
    group_by(year) %>% summarize (adjSal = mean(adjSal), ERA = mean(ERA))
  
  out <- ggplot(data = mp, aes(x = year, y = adjSal)) +
    geom_point() + 
    geom_line(aes(x = year, y = ERA * 1000000, color = 'Mean ERA')) +
    geom_smooth(method = lm, se = F, aes(color = 'Salary Trendline')) +
    labs(
      title = paste(qtile, 'quantile Pitchers', duration[1], '-', dplyr::last(duration)),
      x = 'Year',
      y = 'Salary (2015 dollar)',
      color = ''
    ) + theme_minimal() +
    scale_y_continuous(
      labels = scales::comma,
      sec.axis = sec_axis(trans = ~./1000000, name = 'Mean ERA')) +
    scale_color_viridis(discrete = T)
  
  return(out)
}

#Salary and ERA
sal_era <- function(dat, duration) {
  d <- filter(dat, year %in% duration)
  highest_paid <- d %>% group_by(year) %>% filter(row_number(desc(adjSal)) == 1)
  highest_paid$year[1] <- paste('Highest-Paid Player in', highest_paid$year[1])
  
  out <- ggplot(data = d, aes(x = adjERA, y = adjSal)) +
    geom_point(aes(color = factor(year))) + 
    labs(
      title = paste('Pay and Performance during', duration[1], '-', dplyr::last(duration)),
      x = 'ERA Quantile (0 = Best, 1 = Worst)', 
      y = 'Salary (2015 dollar)',
      color = ''
    ) + theme_minimal() +
    scale_y_continuous(labels = scales::comma) +
    scale_color_viridis(discrete = T) +
    geom_point(data = highest_paid, size = 3, shape = 1) +
    ggrepel::geom_label_repel(data = highest_paid, aes(label = year)) +
    geom_smooth(method = lm, se = F)
  
  return(out)
}

#Pitcher Salaries over Time
salera_over_time <- function(dat, salera) {
  d <- mean_salera_by_year(dat, 1985:2014, salera)
  
  out <- ggplot(data = d, aes(x = yr, y = meanSE)) +
    geom_line(aes(color = paste('Mean', salera))) + theme_minimal() +
    labs(
      subtitle = paste("Pitchers' Mean", salera, '(1985—2014)'),
      x = 'Year',
      y = paste('Mean', salera),
      color = ''
    ) +
  geom_smooth(se = F, size = 0.5, aes(color = 'Trendline')) +
    scale_color_viridis(discrete = T) +
    scale_y_continuous(labels = scales::comma_format(accuracy = 0.5))
  
  return(out)
}

mean_salera_by_year <- function(dat, duration, salera) {
  SE <- paste('filter(dat, year == i)$', ifelse(salera == 'Salary', 'adjSal', 'ERA'))
  d <- data.frame(yr = duration, meanSE = rep(NA, length(duration)), stringsAsFactors = F)
  for(i in duration)
    d$meanSE[d$yr == i] <- mean(eval(parse(text = SE)))
  return(d)
}

#Data Munging Functions
filter_dat <- function(dat) {
  return(dplyr::filter(dat, yearID %in% 1985:2014))
}

adjust_salaries <- function(salaries, inflation) {
  for(i in 1985:2014) {
    thesePlayers <- which(salaries$yearID %in% i)
    salaries$salary[thesePlayers] <- salaries$salary[thesePlayers] * 
      inflation$inflation2015[inflation$year == i]
  }
  names(salaries) <- c('year', 'player', 'team', 'adjSal')
  return(salaries)
}

adjust_ERAs <- function(pitch) {
  pitch$adjERA <- NA
  for(i in 1985:2014) {
    thisYear <- pitch$ERA[pitch$yearID == i]
    pitch$adjERA[pitch$yearID == i] <- thisYear %>% 
      dplyr::cume_dist() #Cumulative distribution transformation
  }
  names(pitch) <- c('year', 'player', 'team', 'ERA', 'adjERA')
  return(pitch)
}