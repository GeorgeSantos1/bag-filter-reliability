# ==============================================================================
# Empirical Case Study: Bag Filter Degradation with Imperfect Maintenance
#
# Reference: Master's Dissertation / Article Study
# Dataset: Industrial bag filter differential pressure (data/bagfilter.rda)
# ==============================================================================

# 1. Package Loading
if (requireNamespace("WienerRS", quietly = TRUE)) {
  library(WienerRS)
  data("bagfilter", package = "WienerRS")
} else if (requireNamespace("devtools", quietly = TRUE)) {
  devtools::load_all(".", quiet = TRUE)
  if (file.exists("data/bagfilter.rda")) load("data/bagfilter.rda")
} else {
  source("R/utils.R")
  if (file.exists("data/bagfilter.rda")) load("data/bagfilter.rda")
}

library(dplyr)
library(tidyr)
library(grid)
library(gridExtra)

# 2. Assign Empirical Dataset
subset_bagfilter <- bagfilter

message("=== Bag Filter Dataset Loaded ===")
cat("Observations:", nrow(subset_bagfilter), "\n")
cat("Time horizon:", min(subset_bagfilter$Time), "to", max(subset_bagfilter$Time), "\n")
cat("Maintenance epochs (duplicated inspection times):",
    unique(subset_bagfilter$Time[duplicated(subset_bagfilter$Time)]), "\n\n")

# ------------------------------------------------------------------------------
# 3. Parameter Estimation (MLE) under Imperfect Maintenance
# ------------------------------------------------------------------------------
mu_hat <- mle_drift_maintenance(subset_bagfilter)
sigma2_hat <- mle_sigma2_maintenance(subset_bagfilter)
rho_hat <- calc_rho(subset_bagfilter)

mu <- mu_hat
sigma2 <- sigma2_hat

cat("--- Parameter Estimates ---\n")
cat("Drift (mu):", round(mu_hat, 4), "\n")
cat("Diffusion variance (sigma^2):", round(sigma2_hat, 4), "\n")
cat("Maintenance efficiencies (rho_j):", round(rho_hat, 4), "\n\n")

# Asymptotic Standard Errors & Confidence Intervals (95%)
dat <- subset_bagfilter
s <- 1
k <- dat %>% filter(duplicated(Time)) %>% pull(Time)
nj <- dat %>% filter(Time > k[1], Time < k[2]) %>% nrow()
N <- 38
df <- length(s) * (N + length(k) + 1) - 1

# Drift confidence interval
erro_padrao_mu <- sqrt(sigma2_hat) / sqrt(42)
t_crit <- qt(1 - 0.05 / 2, df = df)
IC_mu_hat <- c(mu_hat - t_crit * erro_padrao_mu, mu_hat + t_crit * erro_padrao_mu)

# Diffusion variance confidence interval
chi_low <- qchisq(1 - 0.05 / 2, df)
chi_up  <- qchisq(0.05 / 2, df)
IC_sigma_hat <- c((df * sigma2_hat / chi_low), (df * sigma2_hat / chi_up))

var_mu <- (sqrt(sigma2_hat) / sqrt(42))^2
var_sigma2 <- (sqrt((sigma2_hat^2) * 2 / df))^2

cat("--- Asymptotic Confidence Intervals (95%) ---\n")
cat("IC mu: [", round(IC_mu_hat[1], 4), ",", round(IC_mu_hat[2], 4), "]\n")
cat("IC sigma^2: [", round(IC_sigma_hat[1], 4), ",", round(IC_sigma_hat[2], 4), "]\n\n")

# ------------------------------------------------------------------------------
# 4. Model Comparison: Complete vs. Reduced Maintenance Models
# ------------------------------------------------------------------------------
# Complete model: specific rho_j per maintenance event
rho_comp <- calc_rho(subset_bagfilter)
complete_y <- c()
for (j in 1:(length(k) + 1)) {
  if (j == 1) {
    ti <- subset_bagfilter %>% filter(Time <= k[j]) %>%
      filter(row_number() <= n() - 1) %>% select(Time) %>% pull()
    complete_y <- mu * ti
  } else if (j == 2) {
    ti <- subset_bagfilter %>% filter(Time >= k[j - 1], Time <= k[j]) %>% slice(3:n() - 1) %>%
      select(Time) %>% pull()
    complete_y <- c(complete_y, mu * ti - rho_comp[1] * mu * 13)
  } else if (j == 3) {
    ti <- subset_bagfilter %>% filter(Time >= k[j - 1], Time <= k[j]) %>% slice(3:n() - 1) %>%
      select(Time) %>% pull()
    complete_y <- c(complete_y, mu * ti - rho_comp[1] * mu * 13 - rho_comp[2] * (mu * 26 - mu * 13))
  } else if (j == 4) {
    ti <- subset_bagfilter %>% filter(Time >= k[j - 1]) %>% slice(2:n()) %>%
      select(Time) %>% pull()
    complete_y <- c(complete_y, mu * ti - rho_comp[1] * mu * 13 - rho_comp[2] * (mu * 26 - mu * 13) - rho_comp[3] * (mu * 39 - mu * 26))
  }
}
modelo_completo <- complete_y

# Reduced model: fixed / average rho across all maintenance events
rho_red <- rep(mean(calc_rho(subset_bagfilter)), 3)
simple_y <- c()
for (j in 1:(length(k) + 1)) {
  if (j == 1) {
    ti <- subset_bagfilter %>% filter(Time <= k[j]) %>%
      filter(row_number() <= n() - 1) %>% select(Time) %>% pull()
    simple_y <- mu * ti
  } else if (j == 2) {
    ti <- subset_bagfilter %>% filter(Time >= k[j - 1], Time <= k[j]) %>% slice(3:n() - 1) %>%
      select(Time) %>% pull()
    simple_y <- c(simple_y, mu * ti - rho_red[1] * mu * 13)
  } else if (j == 3) {
    ti <- subset_bagfilter %>% filter(Time >= k[j - 1], Time <= k[j]) %>% slice(3:n() - 1) %>%
      select(Time) %>% pull()
    simple_y <- c(simple_y, mu * ti - rho_red[1] * mu * 13 - rho_red[2] * (mu * 26 - mu * 13))
  } else if (j == 4) {
    ti <- subset_bagfilter %>% filter(Time >= k[j - 1]) %>% slice(2:n()) %>%
      select(Time) %>% pull()
    simple_y <- c(simple_y, mu * ti - rho_red[1] * (mu * 13) - rho_red[2] * (mu * 26 - mu * 13) - rho_red[3] * (mu * 39 - mu * 26))
  }
}
modelo_simples <- simple_y

subset_bagfilter$modelo_completo <- modelo_completo
subset_bagfilter$modelo_simples  <- modelo_simples

subset_bagfilter <- subset_bagfilter %>%
  mutate(
    erro_completo = Y - modelo_completo,
    erro_simples  = Y - modelo_simples
  )

calcular_criterios <- function(sse, n_obs, n_params) {
  logLik <- -n_obs / 2 * (log(2 * pi) + log(sse / n_obs) + 1)
  aic <- -2 * logLik + 2 * n_params
  bic <- -2 * logLik + n_params * log(n_obs)
  list(logLik = round(logLik, 2), AIC = round(aic, 2), BIC = round(bic, 2))
}

n_observacoes <- nrow(subset_bagfilter)
p_completo <- 5  # mu, sigma^2, e 3 rhos
p_reduzido <- 3  # mu, sigma^2, e 1 rho fixo

crit_red  <- calcular_criterios(sum(subset_bagfilter$erro_simples^2), n_observacoes, p_reduzido)
crit_comp <- calcular_criterios(sum(subset_bagfilter$erro_completo^2), n_observacoes, p_completo)

tabela_comparacao <- tibble(
  Modelo         = c("Completo (rho_j variavel)", "Reduzido (rho fixo)"),
  Num_Parametros = c(p_completo, p_reduzido),
  LogLik         = c(crit_comp$logLik, crit_red$logLik),
  AIC            = c(crit_comp$AIC, crit_red$AIC),
  BIC            = c(crit_comp$BIC, crit_red$BIC)
)

cat("--- Model Comparison (Lowest AIC/BIC is preferred) ---\n")
print(tabela_comparacao)

# Likelihood Ratio Test
LR <- -2 * (crit_red$logLik - crit_comp$logLik)
p_value <- pchisq(LR, df = 2, lower.tail = FALSE)
cat("\nLikelihood Ratio Statistic (LR):", round(LR, 4), "\n")
cat("p-value (df = 2):", round(p_value, 4), "\n\n")

# ------------------------------------------------------------------------------
# 5. Reliability Evaluation at t0 = 39 (Threshold alpha = 150)
# ------------------------------------------------------------------------------
t0 <- 39
x0 <- subset_bagfilter %>%
  filter(Time == 39) %>%
  filter(Y == min(Y)) %>%
  select(Y) %>%
  pull()

res_ci <- plot_reliability_ci(
  drift = mu,
  sigma2 = sigma2,
  var_drift = var_mu,
  var_sigma2 = var_sigma2,
  threshold = 150,
  t0 = t0,
  x0 = x0,
  t_max = 155,
  x_label = "Time",
  y_label = "Reliability (%)",
  palette = "taylor1989"
)

# Format reliability evaluation table for target inspection epochs
df_visu <- res_ci$data
reliability_table <- df_visu %>%
  mutate(r_mean = paste0(round(r_mean, digits = 4) * 100, "%")) %>%
  spread(key = "Threshold", value = "r_mean") %>%
  filter(time %in% c(39, 50, 70, 90, 110, 130, 150, 170))

cat("--- Reliability Evaluation Table ---\n")
print(reliability_table)

# 80% Confidence Interval table for reliability
df_ic <- df_visu %>%
  mutate(
    lower = round(lower * 100, 2),
    upper = round(upper * 100, 2),
    reliability = round(reliability * 100, 2)
  ) %>%
  filter(time %in% c(39, 50, 70, 90, 110, 130, 150, 170)) %>%
  select(time, reliability, lower, upper)

cat("\n--- Reliability 80% Confidence Intervals ---\n")
print(df_ic)

# Render formatted table in plot window if interactive
if (interactive()) {
  grid.newpage()
  grid.draw(tableGrob(reliability_table, rows = NULL))
}
