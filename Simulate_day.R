day_simulation <- function(demand_tbl, starting_counts) {
  
  if (nrow(demand_tbl) == 0) return(0L)
  inv <- starting_counts
  missed <- 0L
  # make sure all used stations exist in inventory
  
  stns <- union(demand_tbl$start_station, demand_tbl$end_station)
  miss <- setdiff(stns, names(inv))
  
  if (length(miss)) inv[miss] <- 0L
  
  ord <- order(demand_tbl$sim_time_mins)
  s_vec <- demand_tbl$start_station[ord]
  e_vec <- demand_tbl$end_station[ord]
  
  for (i in seq_along(s_vec)) {
    s <- s_vec[i]; e <- e_vec[i]
    
    if (is.null(inv[[s]])) inv[[s]] <- 0L
    if (is.null(inv[[e]])) inv[[e]] <- 0L
    if (inv[[s]] > 0L) {
      inv[[s]] <- inv[[s]] - 1L
      inv[[e]] <- inv[[e]] + 1L
    } else {
      missed <- missed + 1L
    }
  }
  return(missed)
}
