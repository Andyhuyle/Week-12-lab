library(dplyr)
library(readr)
library(lubridate)

load_and_clean_data <- function(filepath) {
  data <- read_csv(filepath)
  data <- data %>%
    mutate(
      user_type = customer_type, # rename via new column
      start_time = as.POSIXct(start_time),
      end_time = as.POSIXct(end_time),
      start_station = as.character(start_station),
      end_station = as.character(end_station)
    ) %>%
    select(-customer_type) %>% # drop old name
    filter(!is.na(start_time) & !is.na(end_time))
  return(data)
}