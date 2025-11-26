library(dplyr)
library(tidyr)

estimate_rates <- function(data) {
  # average trips per hour by OD
  demand_hourly <- data %>%
    mutate(
      hour = hour(start_time),
      day = date(start_time)
    ) %>%
    filter(start_station != "R", end_station != "R") %>%
    group_by(start_station, end_station, hour) %>%
    summarize(avg_rides = n() / n_distinct(day), .groups = "drop")
  return(demand_hourly)
}
