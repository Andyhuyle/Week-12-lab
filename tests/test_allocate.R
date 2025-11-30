# Test for allocate_initial()

test_that("allocate_initial distributes bikes round-robin across all stations", {
  sample_data <- data.frame(
    start_station = c(1, 2, 3),
    end_station   = c(2, 3, 1)
  )
  
  total_bikes <- 10
  
  alloc <- allocate_initial(sample_data, total_bikes)
  
  # all unique stations are present
  expected_stations <- sort(unique(c(sample_data$start_station, sample_data$end_station)))
  expect_equal(sort(as.numeric(alloc$station_id)), expected_stations)
  
  # total bikes equals requested total
  expect_equal(sum(alloc$bikes_available), total_bikes)
  
  # bikes are distributed as evenly as possible (difference <= 1)
  diff_range <- max(alloc$bikes_available) - min(alloc$bikes_available)
  expect_lte(diff_range, 1)
})

# Test for allocate_final()

test_that("allocate_final respects min bikes and allocates proportionally to demand", {
  demand_df <- data.frame(
    station_id = 1:3,
    denied_trips_proportional = c(0.5, 0.3, 0.2),
    bikes_available = c(3, 3, 3)
  )
  
  total_bikes <- 15
  min_bikes   <- 3
  
  result <- allocate_final(demand_df, total_bikes = total_bikes, min_bikes = min_bikes)
  
  # column bikes_available should be removed
  expect_false("bikes_available" %in% names(result))
  
  # all stations preserved
  expect_equal(sort(result$station_id), demand_df$station_id)
  
  # each station has at least min_bikes
  expect_true(all(result$allocated_bikes >= min_bikes))
  
  # total matches requested
  expect_equal(sum(result$allocated_bikes), total_bikes)
  
  # in this simple monotone-demand example: higher demand → >= bikes
  # demand: 0.5 >= 0.3 >= 0.2  → allocated should follow same ordering
  # sort by demand and check allocated_bikes is non-increasing
  ord <- order(demand_df$denied_trips_proportional, decreasing = TRUE)
  allocated_sorted <- result$allocated_bikes[match(demand_df$station_id[ord], result$station_id)]
  expect_gte(allocated_sorted[1], allocated_sorted[2])
  expect_gte(allocated_sorted[2], allocated_sorted[3])
})

