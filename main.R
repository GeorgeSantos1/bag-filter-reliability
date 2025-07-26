##%######################################################%##
#                                                          #
####      MAIN ANALYSIS SCRIPT FOR DEGRADATION MODEL    ####
#                                                          #
##%######################################################%##

# ---
# Author: George Anderson
# Date Created: 2025-07-26
#
# Description:
# This script executes the full workflow for the degradation analysis project.
# It is divided into three main sections:
#   1. Validation: Simulates data with known parameters and verifies that the
#      estimation functions can recover them.
#   2. Application: Processes real-world data from a bag filter and estimates
#      its degradation parameters.
#   3. Prediction: Uses the estimated parameters to generate and plot future
#      reliability curves.
# ---


##%######################################################%##
#                                                          #
####          SECTION 0: SETUP AND FUNCTIONS            ####
#                                                          #
##%######################################################%##

# Load all custom functions from the 'functions.R' file.
# This script must be in the same directory, or you must provide the full path.
source("functions.R")


##%######################################################%##
#                                                          #
####    SECTION 1: VALIDATION WITH SIMULATED DATA       ####
#                                                          #
##%######################################################%##

# This section validates that our estimation functions work correctly.
# We generate data with known parameters and check if the estimators can recover them.

# Set a seed for reproducibility of the simulation.
set.seed(123)

# Generate a simulated dataset for 5 systems ("paths").
# determined by the length of the 'repair_factors' vector.
simulated_fleet_data <- generate_dataset_with_variable_repairs(
  num_paths = 5,
  max_time = 20,
  num_observations = 20,
  drift = 2.0,
  variance = 4.0,
  repair_factor = c(0.3, 0.5, 0.7),
  num_maintenances = 3
)

estimated_drift_sim <- mle_drift_y(simulated_fleet_data)
estimated_variance_sim <- mle_sigma2_y(simulated_fleet_data)
estimated_rho_sim <- estimate_repair_factor(simulated_fleet_data)

cat("--- Validation Results ---\n")
cat("True Drift: 2.0 | Estimated Drift:", round(estimated_drift_sim, 2), "\n")
cat("True Variance: 4.0 | Estimated Variance:", round(estimated_variance_sim, 2), "\n")
cat("True Maintenance Effect: 0.3, 0.5, 0.7 | Estimated Maintenance Effect:",
    estimated_rho_sim, "\n\n")


##%######################################################%##
#                                                          #
####   SECTION 2: APPLICATION WITH REAL-WORLD DATA      ####
#                                                          #
##%######################################################%##

# This section applies the methodology to the real bag filter dataset.

# Pre-process the raw Excel data into a standardized format.
processed_data <- preprocess_bag_filter_data(
  file_path = "dataset/bagfilter_dataset.xlsx",
  start_time = ymd_hms("2024-05-04 14:40:08"),
  end_time = ymd_hms("2024-05-05 00:00:00"),
  thinning_step = 30,
  maintenance_indices = c(1, 391, 392, 781, 783, 1171, 1173),
  object_id = "TEST_SYSTEM_01"
)

# Estimate the degradation parameters from the processed real data.
mu <- mle_drift_y(processed_data)
variance <- mle_sigma2_y(processed_data)
maintenance_effect <- estimate_repair_factor(processed_data)

cat("--- Real Data Parameter Estimates ---\n")
cat("Estimated Drift (mu):", mu, "\n")
cat("Estimated Variance (sigma^2):", variance, "\n")
cat("Estimated Repair Factor (rho):", maintenance_effect, "\n\n")

#%######################################################%##
#                                                          #
####       SECTION 3: RELIABILITY PREDICTION            ####
#                                                          #
##%######################################################%##

# This section uses the estimated parameters to predict future reliability.

# Determine the initial conditions (t0, x0) for the prediction.
# t0 is the time of the last maintenance action.
t0 <- processed_data  |> 
  filter(duplicated(time) | duplicated(time, fromLast = TRUE))  |> 
  summarise(last_maint_time = max(time))  |> 
  pull(last_maint_time)

# x0 is the degradation level immediately AFTER the last maintenance action.
x0 <- processed_data  |> 
  filter(time == t0) |> 
  summarise(post_maint_value = min(Y)) |> 
  pull(post_maint_value)

# Generate the reliability plot and the underlying data.
# We'll assume the function is called `generate_reliability_analysis`
# and it returns a list with the plot and data.
reliability_results <- create_reliability(
  mu = mu,
  variance = variance,
  thresholds = 150,
  t0 = t0,
  x0 = x0,
  t_max = 155)

# To display the results, you can now access them from the returned list:
# Display the plot
print(reliability_results$reliability_plot)

# To view the reliability data itself
reliability_results$reliability_data |> head()
