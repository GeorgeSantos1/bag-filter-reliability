# ==============================================================================
# Monte Carlo Simulation Study: Wiener Degradation with Imperfect Maintenance
#
# Reference: Master's Dissertation / Article Study
#
# NOTE: Executing runSimulation() across the complete factorial design with
#       replications = 1000 is computationally intensive and takes several days.
#       Pre-computed simulation results are stored in 'SimDesign4.rds'.
# ==============================================================================

# Required libraries
if (requireNamespace("WienerRS", quietly = TRUE)) {
  library(WienerRS)
} else if (requireNamespace("devtools", quietly = TRUE)) {
  devtools::load_all(".", quiet = TRUE)
} else {
  source("R/utils.R")
}

if (!requireNamespace("SimDesign", quietly = TRUE)) {
  stop("Package 'SimDesign' is required to execute or analyze this simulation.")
}
library(SimDesign)
library(dplyr)
library(ggplot2)

# ------------------------------------------------------------------------------
# 1. Factorial Simulation Design Matrix
# ------------------------------------------------------------------------------
# n_system: Number of monitored systems / units (1, 10, 20, 50)
# mu:       True drift coefficient (4, 16)
# sigma2:   True diffusion variance (1, 25)
# n_main:   Number of maintenance actions k (3, 4, 5)
# n_intra:  Number of intermediate measurements between maintenances (0, 2, 4)
# tau:      Total observation horizon (20)

Design <- SimDesign::createDesign(
  n_system = c(1, 10, 20, 50),
  mu       = c(4, 16),
  sigma2   = c(1, 25),
  n_main   = c(3, 4, 5),
  n_intra  = c(0, 2, 4),
  tau      = 20
)

# ------------------------------------------------------------------------------
# 2. Data Generating Function (Generate)
# ------------------------------------------------------------------------------
Generate <- function(condition, fixed_objects = NULL) {
  n_med <- (condition$n_main + 1) * (condition$n_intra + 2) - (condition$n_main + 1)

  # Maintenance efficiency factors for each scenario
  if (condition$n_main == 3) {
    rho <- c(0.1, 0.3, 0.5)
  } else if (condition$n_main == 4) {
    rho <- c(0.1, 0.3, 0.5, 0.7)
  } else if (condition$n_main == 5) {
    rho <- c(0.1, 0.3, 0.5, 0.7, 0.9)
  }

  dat <- sim_wiener_maintenance_paths(
    n_units = condition$n_system,
    t_max   = condition$tau,
    n_steps = n_med,
    drift   = condition$mu,
    sigma2  = condition$sigma2,
    rho     = rho,
    n_maint = condition$n_main
  )

  dat
}

# ------------------------------------------------------------------------------
# 3. Parameter Estimation & Metric Analysis (Analyse)
# ------------------------------------------------------------------------------
Analyse <- function(condition, dat, fixed_objects = NULL) {
  n_med <- (condition$n_main + 1) * (condition$n_intra + 2) - (condition$n_main + 1)

  # Maximum likelihood estimators
  mu_hat     <- mle_drift_standard(dat)
  sigma2_hat <- mle_sigma2_standard(dat)

  # Confidence intervals & Empirical Coverage Rate (ECR) for mu
  erro_padrao <- sqrt(sigma2_hat) / sqrt(condition$n_system * condition$tau)
  t_crit      <- qt(1 - 0.05 / 2, df = condition$n_system * (n_med + condition$n_main + 1) - 1)
  IC_mu_hat   <- c(mu_hat - t_crit * erro_padrao, mu_hat + t_crit * erro_padrao)
  CP_mu_hat   <- SimDesign::ECR(IC_mu_hat, condition$mu)

  # Confidence intervals & ECR for sigma^2
  id_col <- if ("Object" %in% names(dat)) "Object" else if ("Objeto" %in% names(dat)) "Objeto" else names(dat)[1]
  s      <- unique(dat[[id_col]])
  first_unit <- dat[dat[[id_col]] == s[1], ]
  k      <- first_unit$Time[duplicated(first_unit$Time)]
  nj     <- nrow(first_unit[first_unit$Time > k[1] & first_unit$Time < k[2], ])
  N      <- nj * (length(k) + 1)
  df     <- length(s) * (N + length(k) + 1) - 1

  chi_low      <- qchisq(1 - 0.05 / 2, df)
  chi_up       <- qchisq(0.05 / 2, df)
  IC_sigma_hat <- c((df * sigma2_hat / chi_low), (df * sigma2_hat / chi_up))
  CP_sigma_hat <- SimDesign::ECR(IC_sigma_hat, condition$sigma2)

  # Model-based asymptotic variances
  mod_var_mu     <- erro_padrao^2
  mod_var_sigma2 <- (2 * sigma2_hat^2) / df

  ret <- c(
    mu_hat         = mu_hat,
    sigma_hat      = sigma2_hat,
    cp_mu_hat      = CP_mu_hat,
    cp_sigma_hat   = CP_sigma_hat,
    mod_var_mu     = mod_var_mu,
    mod_var_sigma2 = mod_var_sigma2
  )

  return(ret)
}

# ------------------------------------------------------------------------------
# 4. Results Aggregation (Summarise)
# ------------------------------------------------------------------------------
Summarise <- function(condition, results, fixed_objects = NULL) {
  obs_bias <- SimDesign::bias(
    results[, c("mu_hat", "sigma_hat")],
    parameter = c(condition$mu, condition$sigma2)
  )
  obs_RMSE <- SimDesign::RMSE(
    results[, c("mu_hat", "sigma_hat")],
    parameter = c(condition$mu, condition$sigma2)
  )
  obs_MAE  <- SimDesign::MAE(
    results[, c("mu_hat", "sigma_hat")],
    parameter = c(condition$mu, condition$sigma2)
  )

  obs_CP_mu_hat    <- mean(results$cp_mu_hat)
  obs_cp_sigma_hat <- mean(results$cp_sigma_hat)

  obs_EmpVar_mu     <- var(results$mu_hat)
  obs_EmpVar_sigma2 <- var(results$sigma_hat)

  obs_ModVar_mu     <- mean(results$mod_var_mu)
  obs_ModVar_sigma2 <- mean(results$mod_var_sigma2)

  ret <- c(
    bias              = obs_bias,
    RMSE              = obs_RMSE,
    MAE               = obs_MAE,
    CP_mu_hat         = obs_CP_mu_hat,
    CP_sigma2_hat     = obs_cp_sigma_hat,
    obs_EmpVar_mu     = obs_EmpVar_mu,
    obs_EmpVar_sigma2 = obs_EmpVar_sigma2,
    obs_ModVar_mu     = obs_ModVar_mu,
    obs_ModVar_sigma2 = obs_ModVar_sigma2
  )

  ret
}

# ------------------------------------------------------------------------------
# 5. Execution (DO NOT RUN INTERACTIVELY - Requires ~4 days)
# ------------------------------------------------------------------------------
# To execute from scratch on a compute cluster or high-performance machine:
#
# resultados <- SimDesign::runSimulation(
#   design       = Design,
#   replications = 1000,
#   generate     = Generate,
#   analyse      = Analyse,
#   summarise    = Summarise,
#   parallel     = TRUE
# )
# saveRDS(resultados, file = "simulations/SimDesign4.rds")

# ------------------------------------------------------------------------------
# 6. Load Pre-computed Results & Generate Figures
# ------------------------------------------------------------------------------
# Locate precomputed RDS in simulations directory:
rds_file <- if (file.exists("simulations/SimDesign4.rds")) {
  "simulations/SimDesign4.rds"
} else if (file.exists("SimDesign4.rds")) {
  "SimDesign4.rds"
} else {
  NULL
}

if (!is.null(rds_file)) {
  message("Loading pre-computed simulation results from: ", rds_file)
  sim_results <- readRDS(rds_file)

  # 1. Root Mean Squared Error (RMSE) plot
  p_rmse <- plot_simulation_rmse(sim_results)

  # 2. Estimation Bias plot
  p_bias <- plot_simulation_bias(sim_results)

  # 3. Empirical Coverage Probability (CP 95%) plot
  p_cp <- plot_simulation_coverage(sim_results)

  # 4. Variance Ratio (Model / Empirical) plot
  p_ratio <- plot_simulation_variance_ratio(sim_results)

  message("Simulation plots successfully generated.")
}
