# Week-12-lab
Bike Allocation Optimization
Authors: Andy & Ren

Optimize bike placement across stations using historical demand data. This workflow estimates arrival rates, simulates demand, and determines optimal allocations.
Quickstart

Install packages

install.packages(c("dplyr", "ggplot2"))
library(dplyr)
library(ggplot2)

Source scripts
source("scripts/estimate_arrival_rates.R")
source("scripts/simulation.R")
source("scripts/allocations.R")
source("scripts/visualize_demand.R")

Run workflow
# Load data
historical_bike_data <- read.csv("data/sample_bike.csv")

# Estimate hourly arrival rates
arrival_rates <- estimate_arrival_rates(historical_bike_data)

# Simulate bike demand
starting_bikes <- allocate_initial(historical_bike_data, total_bikes = 100)
demand_data <- simulate_demand(arrival_rates, starting_bikes, trials = 100)

# Allocate bikes based on demand
final_placements <- allocate_final(demand_data, total_bikes = 100, min_bikes = 3)

# Save and visualize results
write.csv(final_placements, "results/final_placements.csv", row.names = FALSE)
visualize_demand(final_placements)

Project Structure
scripts/          # Functions for estimation, simulation, allocation, visualization
data/             # Historical bike data
results/          # Output final placements
run_optimization.Rmd

Notes
Adjust total_bikes and min_bikes as needed.
Simulation uses multiple trials to capture demand variability.
Output CSV contains optimized bike placements for each station.