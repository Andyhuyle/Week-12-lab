test_that("Arrival rates is correctly computed", {
  df_prime <- tibble(
    start_station = c("R", "A"),
    end_station   = c("A", "B"),
    start_time    = as.POSIXct(c("2022-01-01 08:00:00",
                                 "2022-01-01 08:59:00")),
    end_time      = as.POSIXct(c("2022-01-01 08:00:00",
                                 "2022-01-01 08:59:00")),
    user_type     = c(NA, "Subscriber")
  )
  
  # Compute arrival rates
  rates <- estimate_arrival_rates(df_prime)
  
  # Extract the rate for A -> B in hour 8
  rate_ab <- rates %>%
    filter(start_station == "A",
           end_station   == "B",
           hour          == 8) %>%
    pull(mu_hat)
  
  expect_false(is.na(rate_ab))
  expect_gt(rate_ab, 0)
})
