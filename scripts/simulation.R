library(dplyr)

#' Simulate Bike Arrival Events
#'
#' Generates a simulated dataset of bike trips between stations based on estimated hourly arrival rates.
#' The simulation uses a thinning algorithm for non-homogeneous Poisson processes.
#'
#' @param arrival_rates A data.frame containing the following columns:
#' \describe{
#'   \item{start_station}{ID of the station where the trip starts.}
#'   \item{end_station}{ID of the station where the trip ends.}
#'   \item{hour}{Hour of the day (0-23).}
#'   \item{mu_hat}{Estimated arrival rate for that station pair at that hour.}
#' }
#'
#' @return A data.frame of simulated bike arrival events with columns:
#' \describe{
#'   \item{start_station}{Start station ID for each simulated trip.}
#'   \item{end_station}{End station ID for each simulated trip.}
#'   \item{time}{Simulated hour of the trip (integer 0-23).}
#' }
#' @examples
#'   sample_arrival_rates <- data.frame(
#'   start_station = c(1,1,2),
#'   end_station = c(2,3,3),
#'   hour = c(8,9,8),
#'   mu_hat = c(0.5, 0.7, 0.3)
#' )
#' simulate_rates(sample_arrival_rates)
#' @export
simulate_rates <- function(arrival_rates) {
  # creates an empty simulation df and also splits the historical data into 
  # smaller df's grouped by pairs of stations
  simulation_data <- data.frame()
  historical_data_list <- split(
    arrival_rates, 
    f = list(arrival_rates$start_station, arrival_rates$end_station),
    drop=TRUE
  )
  
  # loops through each pairs of stations
  for (i in 1:length(historical_data_list)) {
    current_df <- historical_data_list[[i]]
    curr_time = 0
    lambda_max <- max(current_df$mu_hat)
    
    # loops through until day ends
    while (curr_time < 23) {
      # randomly advances time 
      curr_time = floor(curr_time + rexp(1, rate= lambda_max))
      
      if (curr_time >= 23) {
        break 
      }
      
      # gets corresponding mu_hat
      mu_hat <- current_df %>%
        filter(hour == curr_time) %>%
        pull(mu_hat)
      
      if (length(mu_hat) == 0) {
        break
      }
      
      # acceptance prob, if accepted then adds new trip to sim data
      if (runif(1) < (mu_hat / lambda_max)) {
        new_arrival <- data.frame(
          start_station = current_df$start_station[1],
          end_station = current_df$end_station[1],
          time = floor(curr_time)
        )
        simulation_data <- rbind(simulation_data, new_arrival)
      }
    }
  }
  
  return(simulation_data)
}


#' Simulate Bike Demand Across Multiple Trials
#'
#' Simulates bike trips for multiple days/trials based on estimated arrival rates and initial station inventories.
#' Tracks denied trips and updates station inventory for each trial.
#'
#' @param arrival_rates A data.frame of estimated arrival rates (see \code{simulate_rates}).
#' @param initial_placements A data.frame with initial bike inventory at each station:
#' \describe{
#'   \item{station_id}{Station ID.}
#'   \item{bikes_available}{Number of bikes initially at the station.}
#' }
#' @param trials Integer. Number of simulation trials to run.
#'
#' @return A data.frame containing for each station:
#' \describe{
#'   \item{station_id}{Station ID.}
#'   \item{bikes_available}{Final number of bikes at the station after all trials.}
#'   \item{denied_trips_proportional}{Proportion of trips denied at the station across trials.}
#' }
#' @examples
#' initial <- data.frame(
#'   station_id = c(1,2,3),
#'   bikes_available = c(5,5,5)
#' )
#' simulate_demand(sample_arrival_rates, initial, trials = 10)
#' @export
simulate_demand <- function(arrival_rates, initial_placements, trials) {
  
  # initialize station inventory as a named vector
  station_inventory <- initial_placements$bikes_available
  names(station_inventory) <- as.character(initial_placements$station_id)
  
  # track cumulative denied trips
  demand_counts <- rep(0, length(station_inventory))
  names(demand_counts) <- names(station_inventory)
  
  # run trials
  for (t in 1:trials) {
    simulation_data <- simulate_rates(arrival_rates)
    simulation_data <- simulation_data %>%
      arrange(time)
    
    # for each event in the simulation
    for (i in 1:nrow(simulation_data)) {
      event <- simulation_data[i, ]
      start_id <- as.character(event$start_station)
      end_id <- as.character(event$end_station)
      
      bikes_at_start <- station_inventory[start_id]
      
      if (bikes_at_start > 0) {
        # trip succeeds -> move bike
        station_inventory[start_id] <- station_inventory[start_id] - 1
        station_inventory[end_id] <- station_inventory[end_id] + 1
      } else {
        # trip denied -> add one to the demand for the station
        demand_counts[start_id] <- demand_counts[start_id] + 1
      }
    }
  }
  
  # transform vectors to df and combine into final output
  final_inventory <- data.frame(
    station_id = names(station_inventory),
    bikes_available = as.numeric(station_inventory),
    stringsAsFactors = FALSE
  )
  
  demand <- data.frame(
    station_id = names(demand_counts),
    denied_trips_proportional = as.numeric(demand_counts) / trials,
    stringsAsFactors = FALSE
  )
  
  final_placements <- merge(
    final_inventory,
    demand,
    by = "station_id"
  )
  
  return(final_placements)
}
