
##%######################################################%##
#                                                          #
####      COLLECTION OF FUNCTIONS FOR DEGRADATION       ####
#               ANALYSIS AND SIMULATION                  #
#                                                          #
##%######################################################%##

# ---
# Author: George Anderson
# Date Created: 2025-07-26
# Last Modified: 2025-07-26
# R Version: 4.3.1 or superior
#
# Description:
# This script contains a set of custom functions for simulating, processing,
# analyzing, and visualizing degradation data based on Wiener processes
# with maintenance interventions.
# ---


##%######################################################%##
#                                                          #
####                SETUP: LOAD PACKAGES                ####
#                                                          #
##%######################################################%##

# This section checks for required packages and installs them if missing.
# It then loads them into the session.

required_packages <- c(
  "readxl",     # For reading Excel files
  "dplyr",      # For data manipulation
  "lubridate",  # For handling dates and times
  "tidyr",      # For tidying data (crossing, unnest)
  "purrr",      # For functional programming (map)
  "ggplot2",    # For plotting
  "statmod",    # For the Inverse Gaussian distribution (pinvgauss)
  "ggtext",     # For advanced text rendering in ggplot
  "tayloRswift" # For specific color palettes
)

for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    message(paste("Package", pkg, "not found. Installing..."))
    install.packages(pkg)
  }
  library(pkg, character.only = TRUE)
}

##%######################################################%##
#                                                          #
####        SECTION 1: DATA PREPROCESSING FUNCTION      ####
#                                                          #
##%######################################################%##

#' Prepare and Sample Bag Filter Process Data
#'
#' This function reads raw data from an Excel file, filters it by a time range,
#' applies subsampling ("thinning"), and finally structures the data into a
#' standardized format for degradation analysis, including `Objeto` (Object),
#' `Time` (0-indexed), and `Y` (degradation value) columns.
#'
#' @param file_path The path to the Excel file containing the raw data.
#' @param start_time A datetime object (e.g., from `lubridate::ymd_hms`) representing
#'   the start of the period of interest for filtering.
#' @param end_time A datetime object representing the end of the period of interest.
#' @param thinning_step An integer that defines the subsampling step. For example,
#'   a `thinning_step` of 30 will select one out of every 30 rows.
#' @param maintenance_indices A numeric vector with the exact row indices (based on
#'   the time-filtered data) that must be kept, corresponding to moments
#'   before and after maintenance actions.
#' @param datetime_col_name The name of the column in the Excel file that contains
#'   the date and time. Default: `"Data Hora"`.
#' @param value_col_name The name of the column containing the degradation data
#'   (e.g., differential pressure). Default: `"Diferencial_mmCa_800dPT8102"`.
#' @param object_id A string to identify the system or object in the final
#'   dataset. Default: `"OBJ_001"`.
#'
#' @return A clean and processed `tibble` (data frame) with the columns `Objeto`,
#'   `Time`, `Y`, and the original datetime column.
#'
#' @export
#'
#' @examples
#' # For this example to work, we need to create a test Excel file.
#' # install.packages(c("writexl", "dplyr", "lubridate"))
#' library(readxl)
#' library(dplyr)
#' library(lubridate)
#'
#' # Creating example data
#' dummy_data <- tibble(
#'   `Data Hora` = seq(ymd_hms("2024-05-04 14:00:00"), by = "10 secs", length.out = 1500),
#'   `Diferencial_mmCa_800dPT8102` = sin(seq_len(1500) / 100) + seq_len(1500) / 500,
#'   `Other Column` = rnorm(1500)
#' )
#' # Adding an extra row at the top to simulate the df[-1,] operation
#' dummy_data_with_header <- bind_rows(
#'    tibble(`Data Hora` = NA, `Diferencial_mmCa_800dPT8102` = NA, `Other Column` = NA),
#'    dummy_data
#' )
#'
#' # Saving to a temporary file
#' temp_file <- tempfile(fileext = ".xlsx")
#' write_xlsx(dummy_data_with_header, temp_file)
#'
#' # Using the function with the example data
#' processed_data <- preprocess_bag_filter_data(
#'   file_path = temp_file,
#'   start_time = ymd_hms("2024-05-04 14:40:08"),
#'   end_time = ymd_hms("2024-05-05 00:00:00"),
#'   thinning_step = 30,
#'   maintenance_indices = c(1, 391, 392, 781, 783, 1171, 1173),
#'   object_id = "TEST_SYSTEM_01"
#' )
#'
#' print(head(processed_data))
#' # plot(processed_data$Time, processed_data$Y, type = 'l')
#'
preprocess_bag_filter_data <- function(file_path,
                                       start_time,
                                       end_time,
                                       thinning_step,
                                       maintenance_indices,
                                       datetime_col_name = "Data Hora",
                                       value_col_name = "Diferencial_mmCa_800dPT8102",
                                       object_id = "OBJ_001") {
  
  # --- 1. Parameter Validation ---
  if (!requireNamespace("readxl", quietly = TRUE) ||
      !requireNamespace("dplyr", quietly = TRUE) ||
      !requireNamespace("lubridate", quietly = TRUE)) {
    stop("The 'readxl', 'dplyr', and 'lubridate' packages are required. Please install them.")
  }
  if (!file.exists(file_path)) {
    stop("File not found at the specified path: ", file_path)
  }
  
  # --- 2. Read and Initial Filtering ---
  df <- suppressWarnings({readxl::read_excel(file_path) })
  
  # Remove the first data row, as in the original script
  df <- df[-1, ]
  
  # Convert the datetime column to the correct type, if necessary
  df[[datetime_col_name]] <- lubridate::as_datetime(df[[datetime_col_name]])
  
  # Filter the data frame by the specified time range
  df_filtered <- df %>%
    dplyr::filter(
      .data[[datetime_col_name]] >= start_time & .data[[datetime_col_name]] < end_time
    )
  
  # --- Generate and Combine Indices for Sampling ---
  thinned_indices <- seq(1, nrow(df_filtered), by = thinning_step)
  final_indices <- sort(unique(c(thinned_indices, maintenance_indices)))
  final_indices <- final_indices[final_indices <= nrow(df_filtered)]
  df_sampled <- df_filtered[final_indices, ]
  
  
  s1 <- seq(1,14)
  for (i in 1:3) {
    s2 <- seq(max(s1),max(s1) + 13)
    s1 <- c(s1,s2)
  }
  time_sequence <- s1[1:nrow(df_sampled)] -1
  
  # --- 4. Final Data Frame Formatting ---
  
  # --- Final Data Frame Formatting ---
  df_final <- df_sampled %>%
    dplyr::rename(Y = dplyr::all_of(value_col_name)) %>%
    dplyr::mutate(
      # Assign the correctly generated sequence (and make it 0-indexed)
      time = time_sequence ,
      id = object_id
    ) %>%
    dplyr::select(id, time, Y, dplyr::all_of(datetime_col_name))
  
  return(df_final)
}


##%######################################################%##
#                                                          #
####     SECTION 2: DEGRADATION PATH SIMULATION         ####
#                                                          #
##%######################################################%##

#' Generate a Wiener Process Path
#'
#' Simulates a single trajectory of a Wiener process (Brownian motion with drift).
#' This is often used to model degradation paths or asset prices over time.
#'
#' @param max_time The final time point for the simulation (T_max).
#' @param num_observations The total number of equally spaced time points to generate.
#' @param drift The drift parameter, mu. Represents the deterministic trend.
#' @param variance The variance parameter, sigma^2. Represents the volatility or noise level.
#' @param id A unique identifier for the generated path (e.g., a unit or object ID). Default is 1.
#'
#' @return A data frame with three columns: `id`, `time`, and `value`.
#'
#' @export
#'
#' @examples
#' # Generate a single degradation path
#' set.seed(42) # for reproducible results
#' path_data <- generate_wiener_path(max_time = 100, num_observations = 51,
#'                                   drift = 0.5, variance = 2.25, id = 1)
#' head(path_data)
#'
#' # Plot the generated path
#' # plot(path_data$time, path_data$value, type = 'l',
#' #      main = "Simulated Wiener Process Path", xlab = "Time", ylab = "Degradation")
#'
generate_wiener_path <- function(max_time, num_observations, drift, variance, id = 1) {
  # Generate a sequence of equally spaced time points from 0 to max_time
  num_observations <- num_observations + 1
  time_points <- seq(0, max_time, length.out = num_observations)
  
  # Calculate the time step size (delta_t)
  delta_t <- max_time / (num_observations - 1)
  
  # Generate standard Brownian motion (B_t) increments
  # B_t = sqrt(delta_t) * cumulative_sum_of_standard_normal_draws
  brownian_increments <- rnorm(n = num_observations - 1, mean = 0, sd = 1)
  brownian_path <- c(0, cumsum(brownian_increments)) * sqrt(delta_t)
  
  # Construct the Wiener process path: W_t = mu*t + sigma*B_t
  # Note: sqrt(variance) is sigma (the standard deviation)
  wiener_path <- drift * time_points + sqrt(variance) * brownian_path
  
  # Create the output data frame
  df <- data.frame(
    id    = paste0("OBJ_", id),
    time  = time_points,
    value = wiener_path
  )
  
  return(df)
}


#' Generate a Dataset of Multiple Wiener Process Paths
#'
#' This is a wrapper function that simulates multiple degradation paths by repeatedly
#' calling `generate_wiener_path`. It's designed to create a complete dataset for a
#' fleet of units or objects.
#'
#' @param num_paths The total number of unique paths (objects/units) to simulate.
#' @param ... Additional arguments to be passed directly to `generate_wiener_path()`,
#'            such as `max_time`, `num_observations`, `drift`, and `variance`.
#'
#' @return A single data frame containing the combined data for all simulated paths.
#'         Each path is identified by a unique `id`.
#'
#' @seealso [generate_wiener_path()] for simulating a single path.
#'
#' @export
#'
#' @examples
#' # For this example to work, make sure the 'generate_wiener_path' function is loaded.
#' # You also need the 'dplyr' and 'ggplot2' packages.
#' # install.packages(c("dplyr", "ggplot2"))
#'
#' library(dplyr)
#' library(ggplot2)
#'
#' # Generate a dataset for 5 units
# set.seed(111) # for reproducibility
# fleet_data <- generate_wiener_dataset(num_paths = 1,
#                                       max_time = 20,
#                                       num_observations = 20,
#                                       drift = 2,
#                                       variance = 2)
#' print(head(fleet_data))
#' print(tail(fleet_data))
#'
#' # Plot all paths together
#' fleet_data %>%
#'   ggplot(aes(x = time, y = value, group = id, color = id)) +
#'   geom_line(alpha = 0.8) +
#'   labs(
#'     title = "Simulated Paths for a Fleet of 5 Units",
#'     x = "Time",
#'     y = "Degradation Value"
#'    ) +
#'   theme_minimal()
#'
generate_wiener_dataset <- function(num_paths, ...){
  # Use lapply to create a list of data frames, one for each path.
  # This avoids the slow rbind() in a loop.
  list_of_paths <- lapply(1:num_paths, function(current_id) {
    generate_wiener_path(id = current_id, ...)
  })
  
  # Efficiently bind all data frames in the list into a single data frame.
  # dplyr::bind_rows() is generally faster and more robust than do.call(rbind, ...).
  dplyr::bind_rows(list_of_paths)
}



#' Generate a Wiener Process Path with Maintenance Interventions
#'
#' Simulates a degradation path that undergoes one or more maintenance events.
#' At each maintenance, the accumulated degradation is reduced by a specified
#' factor (rho).
#'
#' @param max_time The final time point for the simulation.
#' @param num_observations The total number of equally spaced time points in the base path.
#' @param drift The drift parameter (mu) of the underlying Wiener process.
#' @param variance The variance parameter (sigma^2) of the diffusion term.
#' @param repair_factor The maintenance effect parameter (rho), a value between 0 and 1.
#'                      A value of 1.0 means perfect repair (degradation resets to 0
#'                      relative to that point), 0 means no repair.
#' @param num_maintenances The number of equally spaced maintenance events to simulate.
#'
#' @return A data frame containing the simulated path. At each maintenance time, two
#'         rows are present to show the degradation value immediately before and
#'         after the repair, allowing for easy plotting of the instantaneous drop.
#'
#' @seealso [generate_wiener_path()] for simulating a simple path without maintenance.
#'
#' @export
#'
#' @examples
#' # For this example to work, the 'generate_wiener_path' function must be loaded,
#' library(dplyr)
#'
# set.seed(99) # For reproducible results
# maintenance_data <- generate_path_with_maintenance(
#   max_time = 20,
#   num_observations = 20,
#   drift = 0.8,
#   variance = 6,
#   repair_factor = 0.7, # 70% of degradation is repaired
#   num_maintenances = 3
# )
#'
generate_path_with_maintenance <- function(max_time, num_observations, drift, variance,
                                           repair_factor, num_maintenances) {
  
  # Generate a single, uninterrupted base path
  base_path <- generate_wiener_path(
    max_time = max_time,
    num_observations = num_observations,
    drift = drift,
    variance = variance,
    id = 1
  )
  
  # Determine the times for maintenance events
  # Events are equally spaced between time 0 and max_time
  maintenance_times <- seq(from = 0, to = max_time,
                           length.out = max_time/(num_maintenances +1))
  maintenance_times <- maintenance_times[-c(1, length(maintenance_times))] # Remove t=0 and t=max
  
  
  yj <- (num_observations + 1 + num_maintenances) / (num_maintenances + 1)
  xj <- num_observations / (num_maintenances + 1)
  
  # Underlying degradation path
  X <- base_path$value
  
  # degradation path under maintenance effects
  Y <- numeric(num_observations + 1 + num_maintenances)
  
  for (k in 1:num_maintenances) {
    Y[1:yj] <- X[1:(xj + 1)]
    Y[(k * yj + 1):((k + 1) * yj)] <- X[(xj * k + 1):((k + 1) * xj + 1)] - repair_factor * X[(xj * k + 1)]
  }
  
  base_path <- rbind(base_path,base_path %>%
                       filter(round(time,6) %in% round(maintenance_times,6))) %>%
    arrange(time)
  
  base_path$Y <- Y
  
  return(base_path)
}

#' Generate a Path with Variable Maintenance Repair Effects
#'
#' Simulates a degradation path subject to multiple maintenance events, where each
#' event can have a different repair factor (rho).
#'
#' @param max_time The final time point for the simulation.
#' @param num_observations The total number of equally spaced time points in the base path.
#' @param drift The drift parameter (mu) of the underlying Wiener process.
#' @param variance The variance parameter (sigma^2) of the diffusion term.
#' @param repair_factors A numeric vector where each element is the repair factor (rho)
#'                       for a sequential maintenance event. The number of events is
#'                       determined by the length of this vector.
#' @param id A unique identifier for the generated path. Default is 1.
#'
#' @return A data frame containing the simulated path. At each maintenance time, two
#'         rows are present to show the degradation value immediately before and
#'         after the repair, allowing for easy plotting.
#'
#' @seealso [generate_path_with_maintenance()] for a version with a single, constant repair factor.
#'
#' @export
#'
#' @examples
#' # This example requires 'dplyr' and 'ggplot2'.
#' library(dplyr)
#' library(ggplot2)
#'
# set.seed(42) # For reproducible results
# path_with_variable_repairs <- generate_path_with_variable_repairs(
#   max_time = 20,
#   num_observations = 20,
#   drift = 0.5,
#   variance = 4,
#   repair_factor = c(0.2, 0.2,0.7),
#   num_maintenances = 3
# )
#'
generate_path_with_variable_repairs <- function(max_time,
                                                num_observations,
                                                drift, variance,
                                                repair_factor,
                                                num_maintenances,
                                                id=1) {
  # Generate a single, uninterrupted base path
  base_path <- generate_wiener_path(
    max_time = max_time,
    num_observations = num_observations,
    drift = drift,
    variance = variance,
    id = id
  )
  
  # Determine the times for maintenance events
  # Events are equally spaced between time 0 and max_time
  maintenance_times <- seq(from = 0, to = max_time,
                           length.out = max_time/(num_maintenances +1))
  maintenance_times <- maintenance_times[-c(1, length(maintenance_times))] # Remove t=0 and t=max
  
  
  yj <- (num_observations + 1 + num_maintenances) / (num_maintenances + 1)
  xj <- num_observations / (num_maintenances + 1)
  
  # Underlying degradation path
  X <- base_path$value
  
  # degradation path under maintenance effects
  Y <- numeric(num_observations + 1 + num_maintenances)
  
  Y[1:yj] <- X[1:(xj + 1)]
  
  for (k in 1:num_maintenances) {
    if (k == 1){
      Y[(k * yj + 1)] <- (1 - repair_factor[k]) * X[(k * xj + 1)]
      Y[(k * yj + 2):((k + 1) * yj)] <- (1 - repair_factor[k]) * X[(k * xj + 1)] + X[(xj * k + 2):((k + 1) * xj + 1)] - X[(k * xj + 1)]
    }
    if (k > 1){
      aux_w <- NA
      for (l in 1:k) {
        aux_w[l] <- repair_factor[l]*(X[(l * xj + 1)]-X[((l-1) * xj + 1)]) # Somatório dos Rho's
      }
      Y[(k * yj + 1)] <- X[(k * xj + 1)] - sum(aux_w)
      Y[(k * yj + 2):((k + 1) * yj)] <- X[(k * xj + 1)] - sum(aux_w) + X[(xj * k + 2):((k + 1) * xj + 1)] - X[(k * xj + 1)]
    }
  }
  
  base_path <- rbind(base_path,base_path %>%
                       filter(round(time,6) %in% round(maintenance_times,6))) %>%
    arrange(time)
  
  base_path$Y <- Y
  
  return(base_path)
}


#' Generate a Dataset of Multiple Paths with Variable Repairs
#'
#' This is a high-level wrapper function to simulate a "fleet" of systems.
#' It repeatedly calls `generate_path_with_variable_repairs` to create a complete
#' dataset where each system/path has its own stochastic trajectory but follows the
#' same maintenance schedule with variable repair effects.
#'
#' @param num_paths The total number of unique paths (systems/units) to simulate.
#' @param ... Arguments passed directly on to `generate_path_with_variable_repairs()`.
#'            This must include `max_time`, `num_observations`, `drift`, `variance`,
#'            and the `repair_factor` vector.
#'
#' @return A single data frame containing the combined data for all simulated paths,
#'         with each path identified by a unique `id`.
#'
#' @seealso [generate_path_with_variable_repairs()] for the function that simulates a single path.
#'
#' @export
#'
#' @examples
#' # This example requires 'dplyr' and 'ggplot2'.
#' library(dplyr)
#' library(ggplot2)
#'
#' # Ensure the single-path generation functions are loaded first.
#'
#' set.seed(101) # For reproducible results
#' fleet_data_variable_repairs <- generate_dataset_with_variable_repairs(
#'   num_paths = 5,
#'   max_time = 20,
#'   num_observations = 20,
#'   drift = 0.5,
#'   variance = 4,
#'    repair_factor = c(0.2, 0.2,0.7),
#'   num_maintenances = 3
#' )
#'
#'
generate_dataset_with_variable_repairs <- function(num_paths, ...){
  # Use lapply to iterate from 1 to num_paths, generating a data frame for each.
  # The 'id' is passed to the underlying function, and so are all other arguments via '...'.
  list_of_paths <- lapply(1:num_paths, function(current_id) {
    generate_path_with_variable_repairs(id = current_id, ...)
  })
  
  # Efficiently row-bind the list of data frames into a single data frame.
  dplyr::bind_rows(list_of_paths)
}

##%######################################################%##
#                                                          #
####         SECTION 3: PARAMETER ESTIMATION (MLE)      ####
#                                                          #
##%######################################################%##


#' Estimate Drift from Repaired Degradation Data (MLE)
#'
#' Estimates the drift parameter (mu) of a Wiener process using the Maximum
#' Likelihood Estimator (MLE). This function is designed for data that has
#' undergone maintenance interventions.
#' 
#' @param data A data frame containing degradation paths for one or more systems.
#' It must include columns for system ID, time, and the degradation value.
#'
#' @return estimate
#'
#' @examples
#' # This example requires 'dplyr' and 'ggplot2'.
#' library(dplyr)
#'
#' # Ensure the single-path generation functions are loaded first.
#'
#' set.seed(101) # For reproducible results
#' fleet_data_variable_repairs <- generate_dataset_with_variable_repairs(
#'   num_paths = 5,
#'   max_time = 20,
#'   num_observations = 20,
#'   drift = 0.5,
#'   variance = 4,
#'    repair_factor = c(0.2, 0.2,0.7),
#'   num_maintenances = 3
#' )
#'
#'
#' @export
mle_drift_y <- function(data){
  num_path <- unique(data$id)
  num_maintenances <- data %>% filter(id == num_path[1],duplicated(time)) %>% 
    select(time) %>% pull()
  delta_t <- max(data$time)/(length(unique(data$time))-1)
  max_time <- max(data$time)
  Zj <- NA

  ytau_zlj <- lapply(1:length(num_path), function(current_id) {
    y_tau <- data %>% filter(num_path[current_id] == id,time == max(time)) %>% select(Y) %>%
      pull()
  
    for (i in 1:length(num_maintenances)) {
      Zj[i] <- diff(data %>% filter(num_path[current_id] == id,time == num_maintenances[i]) %>% select(Y) %>% pull())
    }
  
    return(as_tibble(y_tau-sum(Zj)))
  })

  suppressMessages({
    (dplyr::bind_cols(ytau_zlj)%>% t() %>% colSums())/(length(num_path)*max_time)
  })
}


#' Estimate Variance/Diffusion from Repaired Degradation Data (MLE)
#'
#' Estimates the variance parameter (sigma^2) of a Wiener process using the
#' Maximum Likelihood Estimator (MLE). This function is designed for data that
#' has undergone maintenance.
#'
#' @param data A data frame containing degradation paths. It must include columns
#'   for system ID, time, and the degradation value.
#'
#' @return A single numeric value representing the unbiased estimate of the variance (sigma^2).
#'
#' @seealso [estimate_drift_from_repaired_data()] for the drift estimation.
#'
#' @export
#'
#' @examples
#' # This example requires 'dplyr' and other previously defined functions.
#' library(dplyr)
#'
#' # 1. First, generate a dataset with a known drift and variance
#' set.seed(101)
#' fleet_data <- generate_dataset_with_variable_repairs(
#'   num_paths = 10,
#'   max_time = 20,
#'   num_observations = 20,
#'   drift = 0.5,
#'   variance = 4.0, # This is the true value we want to estimate
#'   repair_factors = c(0.2, 0.9)
#' )
#' 
#' @export
mle_sigma2_y <- function(data){
  num_path <- unique(data$id)
  num_maintenances <- data %>% filter(id == num_path[1],duplicated(time)) %>% 
    select(time) %>% pull()
  
  mu_hat <- mle_drift_y(data)
  nj <- data %>% 
    filter(id == num_path[1],time > num_maintenances[1],time<num_maintenances[2]) %>% 
    nrow()
  N<- nj*(length(num_maintenances)+1)
  y_aux <- matrix(NA,nrow=length(num_path),ncol=(length(num_maintenances)+1))
  yji<-NA
  tji<-NA
  for (l in 1:length(num_path)) {
    aux_data <- data %>% filter(id == num_path[l])
    for (j in 1:(length(num_maintenances)+1)) {
      if (j == 1){
        yji <- aux_data %>% filter(time <= num_maintenances[j]) %>% 
          filter(row_number() <= n()-1) %>% 
          select(Y) %>% pull() %>% diff()
        tji <- aux_data %>% filter(time <= num_maintenances[j]) %>% 
          filter(row_number() <= n()-1) %>% 
          select(time) %>% pull() %>% diff()
        y_aux[l,j] <- sum(((yji-mu_hat*tji)^2)/tji)
      }
      if (j>=2 & j<(length(num_maintenances)+1) ){
        yji <- aux_data %>% 
          filter(time >= num_maintenances[j-1],time <= num_maintenances[j]) %>% 
          slice(3:n()-1) %>% 
          select(Y) %>% pull() %>% diff()
        tji <- aux_data %>% 
          filter(time >= num_maintenances[j-1],time <= num_maintenances[j]) %>% 
          slice(3:n()-1) %>% 
          select(time) %>% pull() %>% diff()
        y_aux[l,j] <- sum(((yji-mu_hat*tji)^2)/tji)
      }
      if (j==(length(num_maintenances)+1)){
        yji <- aux_data %>% 
          filter(time >= num_maintenances[j-1]) %>% 
          slice(2:n()) %>% 
          select(Y) %>% pull() %>% diff()
        tji <- aux_data %>% 
          filter(time >= num_maintenances[j-1]) %>% 
          slice(2:n()) %>% 
          select(time) %>% pull() %>% diff()
        y_aux[l,j] <- sum(((yji-mu_hat*tji)^2)/tji)
      }
    }
  }
  sigma2_hat_biased <- sum(y_aux)/(length(num_path)*(N+length(num_maintenances)+1))
  sigma2_hat_unbiased <- sigma2_hat_biased*(length(num_path)*
          (N+length(num_maintenances)+1))/(length(num_path)*
          (N+length(num_maintenances)+1)-1)
  
  return(sigma2_hat_unbiased)
}

#' Estimate the Maintenance Repair Factor (rho)
#'
#' Estimates the repair factor (rho) from a degradation dataset that contains
#' maintenance interventions. The function assumes that maintenance events are
#' represented by duplicated time points in the data.
#'
#' @param data1 A data frame containing the degradation path. It must contain:
#'   - A `time` column.
#'   - A `Y` column for the maintenanced degradation process.
#'
#' @return A single numeric value representing the estimated mean repair factor (rho).
#'
#' @export
#'
#' @examples
#' # First, generate some data using our previously defined function.
#' # Ensure 'generate_path_with_maintenance' is loaded and dplyr is installed.
#' library(dplyr)
#'
# set.seed(99)
# maintenance_data <- generate_path_with_maintenance(
#   max_time = 20,
#   num_observations = 20,
#   drift = 2,
#   variance = 2,
#   repair_factor = 0.9, # The true value we want to estimate
#   num_maintenances = 3
# )
#'
#' # Now, estimate rho from the generated data
#' estimated_rho <- estimate_repair_factor(maintenance_data)
#'
#' cat("True rho:", 0.7, "\n")
#' cat("Estimated rho:", estimated_rho, "\n")
#'
estimate_repair_factor <- function(data1){
  
  num_paths <- unique(data1$id)
  
  zlj <- lapply(1:length(num_paths), function(current_id) {
    data <- data1 %>% filter(num_paths[current_id] == id)
    
    maintenance_times <- data$time[duplicated(data$time)]
    delta_t <- max(data$time)/(length(unique(data$time))-1)
    
    Zj <- NA
    yji <- NA
    increments <- -diff(data$Y)
    
    Zj[1] <- -diff(data$Y[data$time == maintenance_times[1]])
    yji[1] <- sum(increments[1:(maintenance_times[1]/delta_t)])
    
    for (i in 2:length(maintenance_times)) {
      Zj[i] <- -diff(data$Y[data$time == maintenance_times[i]])
      yji[i] <- sum(increments[(maintenance_times[i - 1]/delta_t + i):(maintenance_times[i]/delta_t + (i - 1))]) 
    }
    
    return(as_tibble(-Zj / yji))
    
  })
  
  suppressMessages({
    dplyr::bind_cols(zlj) %>% t() %>% colMeans() %>% as.vector()
  })
}

##%######################################################%##
#                                                          #
####         SECTION 4: RELIABILITY PLOTTING            ####
#                                                          #
##%######################################################%##


#' Plot Reliability (Survival) Curves
#'
#' Calculates and plots reliability curves based on a Wiener process degradation model.
#' The reliability is the probability that the degradation level has not yet
#' crossed a failure threshold. It is calculated using the first passage time
#' distribution (Inverse Gaussian).
#'
#' @param mu The estimated drift parameter (μ) of the Wiener process.
#' @param variance The estimated variance parameter (σ²) of the Wiener process.
#' @param thresholds A numeric vector of one or more degradation failure thresholds (α).
#' @param t0 The starting time for the prediction (e.g., time of last maintenance).
#' @param x0 The starting degradation level at t0.
#' @param t_max The maximum time to plot on the x-axis.
#' @param palette The color palette to use for the curves, passed to
#'   `tayloRswift::scale_color_taylor`. Default is "taylor1989".
#'
#' @return A `ggplot` object containing the reliability plot.
#'
#' @export
#'
#' @examples
#' # Required packages for the example
#' # install.packages(c("ggplot2", "tidyr", "purrr", "statmod", "scales", "tayloRswift"))
#'
#' # Plot reliability for multiple failure thresholds
#' plot_object <- plot_reliability_curves(
#'   mu = 0.5,
#'   variance = 2.25,
#'   thresholds = c(40, 50, 60),
#'   t0 = 50,
#'   x0 = 5,
#'   t_max = 150
#' )
#'
#' print(plot_object)
#'
create_reliability <- function(mu, variance, thresholds, t0, x0, t_max, palette = "taylor1989") {
  
  # --- 1. Data Generation  ---
  
  # Define the time sequence relative to t0
  time_relative <- seq(0, t_max - t0, by = 0.1)
  
  # Use tidyr::crossing and purrr::map to generate all curves at once
  reliability_data <- tidyr::crossing(
    threshold = thresholds,
    tau = time_relative
  ) %>%
    dplyr::mutate(
      alpha_minus_x0 = threshold - x0,
      # Calculate reliability P(T > tau) for each threshold and time point
      reliability = purrr::map2_dbl(alpha_minus_x0, tau, function(dist_to_threshold, time_point) {
        if (dist_to_threshold <= 0) return(0) # Already failed
        
        mean_fpt <- dist_to_threshold / mu
        shape_fpt <- (dist_to_threshold^2) / variance
        
        statmod::pinvgauss(time_point, mean = mean_fpt, shape = shape_fpt, lower.tail = FALSE)
      }),
      # Convert relative time back to absolute time for plotting
      time_absolute = tau + t0
    )
  
  # --- 2. Create the Plot ---
  
  p <- reliability_data %>%
    ggplot2::ggplot(ggplot2::aes(x = time_absolute, y = reliability, colour = as.factor(threshold))) +
    ggplot2::geom_line(linewidth = 1, alpha = 0.8) +
    ggplot2::geom_vline(xintercept = t0-2) +
    ggplot2::geom_vline(xintercept = t0, colour = "black", linetype = "longdash") +
    ggplot2::scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
    tayloRswift::scale_color_taylor(palette = palette, reverse = FALSE) +
    ggplot2::coord_cartesian(expand = FALSE) +
    ggplot2::labs(
      x = "Time",
      y = "Reliability (Survival Probability)",
      color = "Failure Threshold"
    ) +
    ggplot2::theme_classic() +
    ggplot2::theme(plot.title = element_blank(),
                   legend.position = "none") +
    ggplot2::annotate("text",x=(t0 + 2.5), y= 0.08,
                      label=paste("t=",t0),size = 3,colour="black")
  
  return(list(
    reliability_data = reliability_data, 
    reliability_plot = p
    )
  )
}








