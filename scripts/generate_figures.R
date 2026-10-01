# ==============================================================================
# Reproduction Script: Generation of All Article & Dissertation Figures
#
# Package: WienerRS
# Author: George Anderson A. dos Santos
# Description: Generates and exports all conceptual, empirical, and
#              Monte Carlo simulation figures to 'figures/' directory.
# ==============================================================================

# 1. Package Loading
if (requireNamespace("WienerRS", quietly = TRUE)) {
  library(WienerRS)
} else if (requireNamespace("devtools", quietly = TRUE)) {
  devtools::load_all(".", quiet = TRUE)
} else {
  source("R/utils.R")
}

library(ggplot2)
library(dplyr)

# Ensure output directory exists
if (!dir.exists("figures")) {
  dir.create("figures")
}

# Helper function to save figures in multiple formats (.svg, .pdf, .eps)
save_plot <- function(plot_obj, base_name, width, height, formats = c("svg", "pdf")) {
  # 1. SVG
  if ("svg" %in% formats) {
    svg_path <- file.path("figures", paste0(base_name, ".svg"))
    ggplot2::ggsave(
      filename = svg_path,
      plot     = plot_obj,
      width    = width,
      height   = height,
      units    = "in",
      dpi      = 300,
      device   = grDevices::svg
    )
    message("Saved figure: ", svg_path)
  }

  # 2. PDF
  if ("pdf" %in% formats) {
    pdf_path <- file.path("figures", paste0(base_name, ".pdf"))
    ggplot2::ggsave(
      filename = pdf_path,
      plot     = plot_obj,
      width    = width,
      height   = height,
      units    = "in",
      dpi      = 300,
      device   = grDevices::cairo_pdf
    )
    message("  -> Saved as PDF: ", pdf_path)
  }

  # 3. EPS
  if ("eps" %in% formats) {
    eps_path <- file.path("figures", paste0(base_name, ".eps"))
    ggplot2::ggsave(
      filename = eps_path,
      plot     = plot_obj,
      width    = width,
      height   = height,
      units    = "in",
      dpi      = 300,
      device   = grDevices::cairo_ps
    )
    message("  -> Saved as EPS: ", eps_path)
  }
}

# ==============================================================================
# PART 1: Theoretical Reliability and Survival Models
# ==============================================================================

message("\n[1/4] Generating theoretical reliability figures...")

labs_density     <- c("Density", "Time", "f(t)")
labs_hazard      <- c("Hazard Rate", "Time", "\u03bb(t)")
labs_reliability <- c("Reliability", "Time", "R(t)")

# 1.1 Exponential Distribution
p_exp <- plot_exponential(
  labs_density     = labs_density,
  labs_reliability = labs_reliability,
  labs_hazard      = labs_hazard
)
save_plot(p_exp, "PLOT_EXP", width = 9, height = 6, formats = c("svg", "pdf"))

# 1.2 Weibull Distribution
p_weibull <- plot_weibull(
  labs_density     = labs_density,
  labs_reliability = labs_reliability,
  labs_hazard      = labs_hazard
)
save_plot(p_weibull, "PLOT_WEIBULL", width = 9, height = 6, formats = c("svg", "pdf"))

# 1.3 Lognormal Distribution
p_lognormal <- plot_lognormal(
  labs_density     = labs_density,
  labs_reliability = labs_reliability,
  labs_hazard      = labs_hazard
)
save_plot(p_lognormal, "PLOT_LOGNORMAL", width = 9, height = 6, formats = c("svg", "pdf"))

# 1.4 Censoring Schemes
p_censura <- plot_censoring()
save_plot(p_censura, "PLOT_CENSURA", width = 10, height = 6, formats = c("svg", "pdf"))

# 1.5 Bathtub Curve
p_banheira <- plot_bathtub_curve(
  infant_mortality_label = "Infant\nMortality",
  useful_life_label      = "Useful Life",
  wear_out_label         = "Wear-out",
  x_label                = "Time"
)
save_plot(p_banheira, "PLOT_BANHEIRA", width = 6, height = 3.5, formats = c("svg", "pdf"))

# 1.6 Degradation Path and Critical Failure Threshold
labs_degradacao <- c(
  "Failure Threshold",
  "Degradation Path",
  "Failure Time",
  "Time",
  "Degradation Level"
)
p_degrada001 <- plot_degradation(labs_degradacao = labs_degradacao)
save_plot(p_degrada001, "PLOT_DEGRADA001", width = 8, height = 4, formats = c("svg", "pdf"))

# 1.7 Wiener Process Paths with Different Drifts
labs_wiener <- c("Degradation", "Time")
p_wiener <- plot_wiener_drift(labs_wiener = labs_wiener)
save_plot(p_wiener, "PLOT_WIENER", width = 6, height = 3.5, formats = c("svg", "pdf"))

# 1.8 Repair Types Comparison (Perfect, Minimal, Imperfect)
labs_reparos <- c("Time", "Degradation", "(a)", "(b)", "(c)")
p_reparos <- plot_repair_types(labs_reparos = labs_reparos)
save_plot(p_reparos, "PLOT_REPARO", width = 9, height = 6, formats = c("svg", "pdf", "eps"))

# 1.9 Observation Scheme with Imperfect Maintenance
labs_scheme <- c("Time", "Degradation")
p_scheme <- plot_maintenance_scheme(labs_scheme = labs_scheme)
save_plot(p_scheme, "PLOT_SCHEMA", width = 8, height = 4, formats = c("svg", "pdf", "eps"))
save_plot(p_scheme, "PLOT_SCHEMA_R1", width = 8, height = 4, formats = c("svg", "pdf", "eps"))

# 1.10 Exponential Degradation Paths
p_degrada01 <- plot_exponential_degradation(labs_degrada01 = c("Time", "Degradation"))
save_plot(p_degrada01, "PLOT_DEGRADA01", width = 8, height = 4, formats = c("svg", "pdf"))

# 1.11 Exponential Reliability with Median Lifetime
p_conf_exp <- plot_exponential_reliability(x_label = "Time", y_label = "R(t)")
save_plot(p_conf_exp, "CONFIABILIDADE_001", width = 6, height = 3.5, formats = c("svg", "pdf"))

# ==============================================================================
# PART 2: Monte Carlo Simulation Study (SimDesign4.rds)
# ==============================================================================

message("\n[2/4] Generating simulation study figures...")

sim_file <- if (file.exists("simulations/SimDesign4.rds")) {
  "simulations/SimDesign4.rds"
} else if (file.exists("SimDesign4.rds")) {
  "SimDesign4.rds"
} else {
  NULL
}

if (!is.null(sim_file)) {
  resultados_sim <- readRDS(sim_file)

  # 2.1 Root Mean Squared Error (RMSE)
  p_rmse <- plot_simulation_rmse(
    data      = resultados_sim,
    labs_rmse = c("Number of Systems", "RMSE")
  )
  save_plot(p_rmse, "PLOT_RMSE", width = 11, height = 6, formats = c("svg", "pdf", "eps"))

  # 2.2 Parameter Estimation Bias
  p_bias <- plot_simulation_bias(
    data      = resultados_sim,
    labs_bias = c("Number of Systems", "Bias")
  )
  save_plot(p_bias, "PLOT_BIAS", width = 11, height = 6, formats = c("svg", "pdf", "eps"))

  # 2.3 Coverage Probability (CP 95%)
  p_cp <- plot_simulation_coverage(
    data          = resultados_sim,
    labs_coverage = c("Number of Systems", "Coverage Probability (95%)")
  )
  save_plot(p_cp, "PLOT_CP", width = 11, height = 6, formats = c("svg", "pdf"))

  # 2.4 Variance Ratio (Model / Empirical)
  p_ratiovar <- plot_simulation_variance_ratio(
    data          = resultados_sim,
    labs_ratiovar = c("Number of Systems", "Variance Ratio")
  )
  save_plot(p_ratiovar, "PLOT_RATIOVAR", width = 11, height = 6, formats = c("svg", "pdf"))
} else {
  warning("File 'simulations/SimDesign4.rds' not found. Skipping simulation plots.")
}

# ==============================================================================
# PART 3: Empirical Application - Bag Filter (Dataset 'bagfilter')
# ==============================================================================

message("\n[3/4] Generating empirical application figures (Bag Filter)...")

# Load empirical dataset from WienerRS package
if (requireNamespace("WienerRS", quietly = TRUE)) {
  data("bagfilter", package = "WienerRS", envir = environment())
}
if (!exists("bagfilter")) {
  if (file.exists("data/bagfilter.rda")) {
    load("data/bagfilter.rda")
  }
}

# Parameter estimation for empirical process
mu_est     <- mle_drift_maintenance(bagfilter)
sigma2_est <- mle_sigma2_maintenance(bagfilter)
rho_est    <- calc_rho(bagfilter)

t0_eval <- 39
x0_eval <- min(bagfilter$Y[bagfilter$Time == t0_eval])

# 3.1 Empirical Degradation Trajectory of the Bag Filter
p_result001 <- plot_maintenance(
  data      = bagfilter,
  ylab      = "Differential [mmWC]",
  xlab      = "Time",
  show_time = TRUE
)
save_plot(p_result001, "RESULT_001", width = 8, height = 4, formats = c("svg", "pdf", "eps"))

# 3.2 Comparison between Observed Process Y(t) and Natural Trajectory X(t)
labs_xtyt <- c(
  "Y(t) - Degradation process with maintenance actions",
  "X(t) - Natural degradation process",
  "Time",
  "Degradation"
)
p_xtyt <- plot_wiener_maintenance_comparison(labs_xtyt = labs_xtyt)
save_plot(p_xtyt, "PLOT_XTYT", width = 8, height = 4, formats = c("svg", "pdf"))

# 3.3 Merit Functions: First Hitting Time (FHT) Inverse Gaussian PDF and CDF
p_merito <- plot_merit_functions(
  drift         = mu_est,
  sigma2        = sigma2_est,
  threshold     = 150,
  t0            = t0_eval,
  x0            = x0_eval,
  t_max         = 155,
  labs_merito01 = c("Density", "Time", "f(t)"),
  labs_merito02 = c("Cumulative Distribution", "Time", "F(t)")
)
save_plot(p_merito, "PLOT_MERITO", width = 8, height = 4, formats = c("svg", "pdf"))

# 3.4 Goodness-of-Fit Diagnostic: P-P Plot and Q-Q Plot with Anderson-Darling Test
labs_qq01 <- c("Theoretical Cumulative Distribution", "Empirical Cumulative Distribution", "P-P Plot")
labs_qq02 <- c("Theoretical Quantiles", "Empirical Quantiles", "Q-Q Plot")
p_qqplot <- plot_diagnostic_qq(
  data          = bagfilter,
  labs_qqplot01 = labs_qq01,
  labs_qqplot02 = labs_qq02
)
save_plot(p_qqplot, "PLOT_QQPLOT", width = 8, height = 4, formats = c("svg", "pdf", "eps"))

# 3.5 Theoretical Inverse Gaussian Reliability Curve
p_result002 <- plot_reliability(
  mu        = mu_est,
  sigma2    = sigma2_est,
  alpha     = 150,
  t0        = t0_eval,
  x0        = x0_eval,
  t_max     = 155,
  xlab      = "Time",
  ylab      = "Reliability (%)",
  palette   = "taylor1989"
)
save_plot(p_result002, "RESULT_002", width = 11, height = 5, formats = c("svg", "pdf"))

# 3.6 Reliability Curve with Asymptotic Confidence Interval (Delta Method)
df_est      <- 38 + 3 + 1 - 1
var_mu_est  <- (sqrt(sigma2_est) / sqrt(42))^2
var_sig_est <- (sqrt((sigma2_est^2) * 2 / df_est))^2

res_ci <- plot_reliability_ci(
  drift       = mu_est,
  sigma2      = sigma2_est,
  var_drift   = var_mu_est,
  var_sigma2  = var_sig_est,
  threshold   = 150,
  t0          = t0_eval,
  x0          = x0_eval,
  t_max       = 155,
  x_label     = "Time",
  y_label     = "Reliability (%)",
  palette     = "taylor1989"
)
save_plot(res_ci$plot, "RELIABILITY_IC_001", width = 6, height = 3, formats = c("svg", "pdf", "eps"))

message("\n[4/4] All figures successfully generated and saved to 'figures/'!")
