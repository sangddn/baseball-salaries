get_processed_data <- function() {
  d <- read.csv('../processed_data/processed.csv', header = T, stringsAsFactors = F)
  return(d)
}