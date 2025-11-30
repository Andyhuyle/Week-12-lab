# Test for simulate_rates()

test_that("simulate_rates returns trips with valid structure and times", {
  set.seed(123)  # for reproducibility of the random process
  
  arrival_rates <- data.frame(
    start_station = c(1, 1, 2),
    end_station   = c(2, 3, 3),
    hour          = c(8, 9, 10),
    mu_hat        = c(0.5, 1.0, 0.3)
  )
  
  sim <- simulate_rates(arrival_rates)
  
  # If any trips are generated, they should respect the input structure
  if (nrow(sim) > 0) {
    # station IDs should come from arrival_rates
    expect_true(all(sim$start_station %in% arrival_rates$start_station))
    expect_true(all(sim$end_station   %in% arrival_rates$end_station))
    
    # time should be integer hours between 0 and 22 (since curr_time < 23)
    expect_true(all(sim$time == floor(sim$time)))
    expect_true(all(sim$time >= 0 & sim$time <= 22))
  }
})


# Test for simulate_demand()

test_that("simulate_demand moves bikes and counts denied trips correctly", {
  # fake arrival_rates object (contents don't matter because we mock simulate_rates)
  arrival_rates <- data.frame(
    start_station = 1,
    end_station   = 2,
    hour          = 8,
    mu_hat        = 1
  )
  
  initial_placements <- data.frame(
    station_id      = c(1, 2),
    bikes_available = c(2, 0)
  )
  
  # deterministic simulated trips: three attempts from 1 -> 2
  fake_simulated_trips <- data.frame(
    start_station = c(1, 1, 1),
    end_station   = c(2, 2, 2),
    time          = c(1, 2, 3)
  )
  
  result <- with_mocked_bindings(
    {
      simulate_demand(
        arrival_rates       = arrival_rates,
        initial_placements  = initial_placements,
        trials              = 1
      )
    },
    simulate_rates = function(arrival_rates) fake_simulated_trips
  )
  
  # Check station 1
  res1 <- result[result$station_id == "1", ]
  expect_equal(res1$bikes_available, 0)          # both bikes moved out
  expect_equal(res1$denied_trips_proportional, 1)  # third trip from 1 is denied
  
  # Check station 2
  res2 <- result[result$station_id == "2", ]
  expect_equal(res2$bikes_available, 2)          # received two bikes
  expect_equal(res2$denied_trips_proportional, 0)  # no denied trips at station 2
})

