#' Allocate Initial Bikes to Stations
#'
#' Distributes a total number of bikes across all stations evenly (round-robin fashion).
#'
#' @param bike_data A data.frame containing at least `start_station` and `end_station` columns.
#' @param total_bikes Integer. Total number of bikes to allocate across stations.
#'
#' @return A data.frame with two columns:
#' \describe{
#'   \item{station_id}{The unique ID of each bike station.}
#'   \item{bikes_available}{The number of bikes initially allocated to each station.}
#' }
#' @examples
#' sample_data <- data.frame(
#'   start_station = c(1,2,3),
#'   end_station = c(2,3,1)
#' )
#' allocate_initial(sample_data, total_bikes = 10)
#' @export
allocate_initial <- function(bike_data, total_bikes) {
  all_stations <- unique(c(
    bike_data$start_station,
    bike_data$end_station
  ))
  
  num_stations <- length(all_stations)
  
  # initializes inventory according to stations
  inventory <- rep(0, num_stations)
  names(inventory) <- as.character(all_stations)
  
  bikes_remaining <- total_bikes
  station_index <- 1
  
  # while there are bikes still left, adds bikes to each station one after another
  while (bikes_remaining > 0) {
    
    station_id_to_receive <- names(inventory)[station_index]
    
    inventory[station_id_to_receive] <- inventory[station_id_to_receive] + 1
    
    bikes_remaining <- bikes_remaining - 1
    
    station_index <- station_index + 1
    # station index wraps over to start allocation evenly
    if (station_index > num_stations) {
      station_index <- 1 
    }
  }
  
  initial_bike_allocations <- data.frame(
    station_id = names(inventory),
    bikes_available = as.numeric(inventory)
  )
  
  return(initial_bike_allocations)
}


#' Allocate Final Bikes Based on Demand
#'
#' Allocates bikes to stations according to simulated demand while ensuring a minimum number of bikes per station. 
#' Also saves demand df to csv in results directory.
#'
#' @param df A data.frame containing station information and a `denied_trips_proportional` column representing relative unmet demand.
#' @param total_bikes Integer. Total number of bikes to allocate across all stations.
#' @param min_bikes Integer. Minimum number of bikes guaranteed at each station (default is 3).
#'
#' @return A data.frame with the final bike allocations per station:
#' \describe{
#'   \item{station_id}{The ID of each station.}
#'   \item{denied_trips_proportional}{Relative demand weight used for allocation.}
#'   \item{allocated_bikes}{Final number of bikes allocated to the station.}
#' }
#' @examples
#' demand_df <- data.frame(
#'   station_id = 1:3,
#'   denied_trips_proportional = c(0.5, 0.3, 0.2),
#'   bikes_available = c(3,3,3)
#' )
#' allocate_final(demand_df, total_bikes = 10, min_bikes = 2)
#' @export
allocate_final <- function(df, total_bikes, min_bikes = 3) {
  n <- nrow(df)
  
  # start by giving each station min num of bikes
  base_alloc <- rep(min_bikes, n)
  remaining_bikes <- total_bikes - sum(base_alloc)
  
  # get relative demand for each station
  weights <- df$denied_trips_proportional
  
  # math to allocate bikes proportionally
  prop <- weights / sum(weights)
  raw_alloc <- prop * remaining_bikes
  extra_alloc <- floor(raw_alloc)
  leftover <- remaining_bikes - sum(extra_alloc)
  
  # computes fractional parts to decide which stations get leftover bikes
  frac <- raw_alloc - extra_alloc
  idx <- order(frac, decreasing = TRUE)[1:leftover]
  extra_alloc[idx] <- extra_alloc[idx] + 1
  
  # combine base and extra bike allocations
  df$allocated_bikes <- base_alloc + extra_alloc
  
  
  df <- subset(df, select = -bikes_available)
  return(df)
}