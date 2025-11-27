#' Visualize Final Bike Allocation and Denied Trips
#'
#' Creates a scatter plot showing the number of bikes allocated to each station
#' and the proportion of denied trips. The plot uses point size to indicate the 
#' number of allocated bikes and a color gradient (green to red) to indicate 
#' the proportion of denied trips.
#'
#' @param final_placements A data.frame containing at least the following columns:
#' \describe{
#'   \item{station_id}{ID of each bike station.}
#'   \item{allocated_bikes}{Number of bikes allocated to the station.}
#'   \item{denied_trips_proportional}{Proportion of trips denied at the station.}
#' }
#'
#' @return A \code{ggplot} object representing the visualization.
#' @details
#' The function also saves the plot as a PNG file at \code{"results/optimal_bike_placement.png"}.
#'
#' @examples
#' sample_final <- data.frame(
#'   station_id = 1:5,
#'   allocated_bikes = c(5, 10, 7, 3, 8),
#'   denied_trips_proportional = c(0.1, 0.3, 0.05, 0.0, 0.2)
#' )
#' visualize_demand(sample_final)
#' @export
visualize_demand <- function(final_placements) {
  p <- ggplot(final_placements, aes(x = factor(station_id), 
                                    y = allocated_bikes, 
                                    size = allocated_bikes, 
                                    color = denied_trips_proportional)) +
    geom_point(alpha = 0.7) +
    scale_color_gradient(low = "green", high = "red") +
    labs(x = "Station ID", y = "Allocated Bikes", color = "Denied Trips Proportion",
         title = "Bike Allocation and Denied Trips by Station") +
    theme_minimal()
  return(p)
}