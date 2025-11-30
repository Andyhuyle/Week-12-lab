# Test for visualize_demand()

test_that("visualize_demand returns a ggplot with correct data", {
  # minimal example input
  sample_final <- data.frame(
    station_id = 1:3,
    allocated_bikes = c(5, 10, 7),
    denied_trips_proportional = c(0.1, 0.3, 0.05)
  )
  
  p <- visualize_demand(sample_final)
  
  # check it's a ggplot object
  expect_s3_class(p, "ggplot")
  
  # check the data attached to the plot is the same as the input
  expect_equal(p$data, sample_final)
})
