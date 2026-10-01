library(testthat)

# Ensure functions are loaded
if (!exists("sim_wiener_path")) {
  source("../../utils.r")
}

test_that("sim_wiener_path generates a valid single degradation path", {
  set.seed(42)
  t_max <- 10
  n_steps <- 20
  drift <- 2
  sigma2 <- 0.5
  
  df <- sim_wiener_path(t_max = t_max, n_steps = n_steps, drift = drift, sigma2 = sigma2, obj_id = 1)
  
  # Structural checks
  expect_s3_class(df, "data.frame")
  expect_named(df, c("Object", "Wt", "Time"))
  expect_equal(nrow(df), n_steps + 1)
  
  # Initial conditions
  expect_equal(df$Time[1], 0)
  expect_equal(df$Wt[1], 0)
  expect_equal(df$Time[nrow(df)], t_max)
  expect_equal(unique(df$Object), "OBJ_01")
  
  # Time sequence monotonicity
  expect_true(all(diff(df$Time) > 0))
})

test_that("sim_wiener_paths generates valid multiple paths", {
  set.seed(42)
  n_units <- 5
  t_max <- 15
  n_steps <- 30
  drift <- 1.5
  sigma2 <- 1.0
  
  df <- sim_wiener_paths(n_units = n_units, t_max = t_max, n_steps = n_steps, drift = drift, sigma2 = sigma2)
  
  # Structural checks
  expect_s3_class(df, "data.frame")
  expect_named(df, c("Object", "Wt", "Time"))
  expect_equal(nrow(df), n_units * (n_steps + 1))
  
  # Unique objects
  objects <- unique(df$Object)
  expect_equal(length(objects), n_units)
  expect_equal(objects, paste0("OBJ_", sprintf("%02d", 1:n_units)))
  
  # All paths start at t=0, Wt=0
  starts <- df[df$Time == 0, ]
  expect_equal(nrow(starts), n_units)
  expect_equal(starts$Wt, rep(0, n_units))
})

test_that("plot_degradation_paths returns a ggplot object", {
  set.seed(42)
  df <- sim_wiener_paths(n_units = 3, t_max = 10, n_steps = 20, drift = 2, sigma2 = 1)
  
  p <- plot_degradation_paths(df, title = "Test Plot")
  
  expect_s3_class(p, "ggplot")
  expect_equal(p$labels$title, "Test Plot")
  expect_equal(p$labels$x, "Time")
  expect_equal(p$labels$y, "Degradation (Wt)")
})

test_that("sim_wiener_maintenance generates paths with maintenance effect", {
  set.seed(42)
  t_max <- 20
  n_steps <- 20
  drift <- 2
  sigma2 <- 1
  rho <- 0.6
  n_maint <- 3

  df <- sim_wiener_maintenance(
    t_max = t_max, n_steps = n_steps, drift = drift,
    sigma2 = sigma2, rho = rho, n_maint = n_maint
  )

  expect_s3_class(df, "data.frame")
  expect_true(all(c("Object", "Wt", "Time", "Y") %in% names(df)))
  expect_equal(df$Time[1], 0)
  expect_equal(df$Y[1], 0)

  # Maintenance points create duplicate time stamps for the jump
  expect_equal(sum(duplicated(df$Time)), n_maint)

  # With positive rho and positive drift, maintained process Y is generally <= unmaintained Wt
  expect_true(tail(df$Y, 1) < tail(df$Wt, 1))
})

test_that("calc_rho analytically recovers maintenance effect parameters", {
  set.seed(42)
  # Test with rho = 0.5 across 3 interventions
  df1 <- sim_wiener_maintenance(t_max = 20, n_steps = 20, drift = 2, sigma2 = 1, rho = 0.5, n_maint = 3)
  rho_est1 <- calc_rho(df1)
  expect_equal(length(rho_est1), 3)
  expect_equal(rho_est1, rep(0.5, 3), tolerance = 1e-6)

  # Test with rho = 1.0 (perfect maintenance)
  df2 <- sim_wiener_maintenance(t_max = 20, n_steps = 20, drift = 2, sigma2 = 1, rho = 1.0, n_maint = 3)
  rho_est2 <- calc_rho(df2)
  expect_equal(rho_est2, rep(1.0, 3), tolerance = 1e-6)

  # Test with single maintenance intervention
  df3 <- sim_wiener_maintenance(t_max = 20, n_steps = 20, drift = 2, sigma2 = 1, rho = 0.75, n_maint = 1)
  rho_est3 <- calc_rho(df3)
  expect_equal(length(rho_est3), 1)
  expect_equal(rho_est3, 0.75, tolerance = 1e-6)
})

test_that("plot_wiener_maintenance returns a valid ggplot object", {
  set.seed(42)
  df <- sim_wiener_maintenance(t_max = 20, n_steps = 20, drift = 2, sigma2 = 1, rho = 0.5, n_maint = 3)
  p <- plot_wiener_maintenance(df, title = "Maintenance Test Plot")

  expect_s3_class(p, "ggplot")
  expect_equal(p$labels$title, "Maintenance Test Plot")
  expect_equal(p$labels$x, "Time")
  expect_equal(p$labels$y, "Degradation")
})

test_that("sim_wiener_maintenance_path supports vector rho with individual effects", {
  set.seed(42)
  rho_vec <- c(0.2, 0.5, 0.9)
  df <- sim_wiener_maintenance_path(
    t_max = 20, n_steps = 20, drift = 2, sigma2 = 1,
    rho = rho_vec, n_maint = 3, obj_id = 1
  )

  expect_s3_class(df, "data.frame")
  expect_true(all(c("Object", "Wt", "Time", "Y") %in% names(df)))
  expect_equal(unique(df$Object), "OBJ_01")

  rho_est <- calc_rho(df)
  expect_equal(rho_est, rho_vec, tolerance = 1e-6)
})

test_that("sim_wiener_maintenance_paths simulates multiple maintained units", {
  set.seed(42)
  n_units <- 4
  rho_vec <- c(0.3, 0.7)
  # n_steps must be a multiple of n_maint + 1 (21 is divisible by 3)
  df <- sim_wiener_maintenance_paths(
    n_units = n_units, t_max = 15, n_steps = 21,
    drift = 1.5, sigma2 = 0.5, rho = rho_vec, n_maint = 2
  )

  expect_s3_class(df, "data.frame")
  expect_equal(length(unique(df$Object)), n_units)
  expect_equal(unique(df$Object), paste0("OBJ_", sprintf("%02d", 1:n_units)))
})

test_that("plot_wiener_maintenance_grid creates a multi-panel arrangement", {
  set.seed(42)
  df <- sim_wiener_maintenance_paths(
    n_units = 2, t_max = 15, n_steps = 21,
    drift = 1.5, sigma2 = 0.5, rho = c(0.3, 0.7), n_maint = 2
  )

  grid_obj <- plot_wiener_maintenance_grid(df, ncol = 2)
  expect_true(inherits(grid_obj, "gtable") || inherits(grid_obj, "grob"))
})

test_that("mle_drift_standard estimates the true drift parameter accurately", {
  set.seed(42)
  true_drift <- 3.0
  # Simulate 100 paths to test asymptotic consistency of MLE
  df <- sim_wiener_paths(n_units = 100, t_max = 20, n_steps = 20, drift = true_drift, sigma2 = 1.0)
  drift_hat <- mle_drift_standard(df)

  expect_true(is.numeric(drift_hat))
  expect_equal(drift_hat, true_drift, tolerance = 0.1)
})

test_that("mle_sigma2_standard estimates the diffusion parameter accurately", {
  set.seed(42)
  true_sigma2 <- 2.0
  df <- sim_wiener_paths(n_units = 100, t_max = 20, n_steps = 50, drift = 3.0, sigma2 = true_sigma2)
  sigma2_hat <- mle_sigma2_standard(df)

  expect_true(is.numeric(sigma2_hat))
  expect_equal(sigma2_hat, true_sigma2, tolerance = 0.2)
})

test_that("mle_drift_maintenance recovers true drift despite imperfect maintenance", {
  set.seed(42)
  true_drift <- 3.0
  # Simulate 100 paths with maintenance
  df <- sim_wiener_maintenance_paths(
    n_units = 100, t_max = 20, n_steps = 20,
    drift = true_drift, sigma2 = 1.0, rho = c(0.2, 0.5, 0.8), n_maint = 3
  )
  drift_maint_hat <- mle_drift_maintenance(df)

  expect_true(is.numeric(drift_maint_hat))
  expect_equal(drift_maint_hat, true_drift, tolerance = 0.1)
})

test_that("mle_sigma2_maintenance recovers true diffusion parameter under maintenance", {
  set.seed(42)
  true_sigma2 <- 2.0
  df <- sim_wiener_maintenance_paths(
    n_units = 100, t_max = 20, n_steps = 20,
    drift = 3.0, sigma2 = true_sigma2, rho = c(0.2, 0.5, 0.8), n_maint = 3
  )
  sigma2_maint_hat <- mle_sigma2_maintenance(df)

  expect_true(is.numeric(sigma2_maint_hat))
  expect_equal(sigma2_maint_hat, true_sigma2, tolerance = 0.3)
})
test_that("plot_reliability generates a valid ggplot object for single threshold", {
  p <- plot_reliability(
    mu = 2.0, sigma2 = 0.5, alpha = 20,
    t0 = 0, x0 = 0, t_max = 20
  )

  expect_s3_class(p, "ggplot")
  expect_equal(p$labels$x, "Time")
  expect_equal(p$labels$y, "Reliability")
  expect_equal(p$labels$colour, "Threshold")

  df_plot <- p$data
  expect_s3_class(df_plot, "data.frame")
  expect_true(all(c("time", "r_mean", "Threshold") %in% names(df_plot)))
  expect_equal(unique(df_plot$Threshold), factor(20))

  # Initial reliability at t=t0 is 1.0
  expect_equal(df_plot$r_mean[1], 1.0, tolerance = 1e-6)

  # Reliability function must be non-increasing over time
  diffs <- diff(df_plot$r_mean)
  expect_true(all(diffs <= 1e-12))
})

test_that("plot_reliability handles multiple thresholds and orders reliability accordingly", {
  thresholds <- c(10, 15, 20)
  p <- plot_reliability(
    mu = 1.5, sigma2 = 0.8, alpha = thresholds,
    t0 = 2, x0 = 1, t_max = 15,
    xlab = "Inspection Time", ylab = "Reliability R(t)",
    palette = "taylor1989", show_title = TRUE, title = "System Reliability"
  )

  expect_s3_class(p, "ggplot")
  expect_equal(p$labels$x, "Inspection Time")
  expect_equal(p$labels$y, "Reliability R(t)")
  expect_equal(p$labels$title, "System Reliability")

  df_plot <- p$data
  expect_equal(levels(df_plot$Threshold), as.character(thresholds))

  # Higher thresholds must yield strictly higher reliability at any positive elapsed time
  mid_time <- df_plot[df_plot$time == 8, ]
  r_10 <- mid_time$r_mean[mid_time$Threshold == "10"]
  r_15 <- mid_time$r_mean[mid_time$Threshold == "15"]
  r_20 <- mid_time$r_mean[mid_time$Threshold == "20"]

  expect_true(r_20 > r_15)
  expect_true(r_15 > r_10)

  # Check Portuguese alias 'paleta' compatibility
  p_alias <- plot_reliability(
    mu = 1.5, sigma2 = 0.8, alpha = thresholds,
    t0 = 0, x0 = 0, t_max = 10, paleta = "taylor1989"
  )
  expect_s3_class(p_alias, "ggplot")
})

test_that("plot_reliability validates input parameters and throws informative errors", {
  # mu <= 0
  expect_error(plot_reliability(mu = -1, sigma2 = 1, alpha = 10, t_max = 20), "mu")
  expect_error(plot_reliability(mu = 0, sigma2 = 1, alpha = 10, t_max = 20), "mu")

  # sigma2 <= 0
  expect_error(plot_reliability(mu = 1, sigma2 = -0.5, alpha = 10, t_max = 20), "sigma2")
  expect_error(plot_reliability(mu = 1, sigma2 = 0, alpha = 10, t_max = 20), "sigma2")

  # alpha <= x0
  expect_error(plot_reliability(mu = 1, sigma2 = 1, alpha = 5, x0 = 5, t_max = 20), "alpha")
  expect_error(plot_reliability(mu = 1, sigma2 = 1, alpha = c(10, 3), x0 = 5, t_max = 20), "alpha")

  # t_max <= t0
  expect_error(plot_reliability(mu = 1, sigma2 = 1, alpha = 10, t0 = 10, t_max = 5), "t_max")
  expect_error(plot_reliability(mu = 1, sigma2 = 1, alpha = 10, t0 = 10, t_max = 10), "t_max")
})

test_that("plot_maintenance creates a valid ggplot object with maintenance jumps", {
  set.seed(42)
  df_maint <- sim_wiener_maintenance(
    t_max = 20, n_steps = 20, drift = 2, sigma2 = 1, rho = 0.5, n_maint = 3
  )

  p <- plot_maintenance(
    df_maint,
    xlab = "Time (hours)",
    ylab = "Degradation (mm)",
    title = "System Degradation",
    show_title = TRUE,
    show_time = TRUE
  )

  expect_s3_class(p, "ggplot")
  expect_equal(p$labels$x, "Time (hours)")
  expect_equal(p$labels$y, "Degradation (mm)")
  expect_equal(p$labels$title, "System Degradation")

  # Layers should include line, segment, and text annotations
  layer_geoms <- vapply(p$layers, function(l) class(l$geom)[1], character(1))
  expect_true("GeomLine" %in% layer_geoms)
  expect_true("GeomSegment" %in% layer_geoms)
  expect_true("GeomText" %in% layer_geoms)

  # Check that Cartesian origin has no padding (expand = c(0, 0))
  expect_equal(p$scales$get_scales("x")$expand, c(0, 0))
  expect_equal(p$scales$get_scales("y")$expand, c(0, 0))
})

test_that("plot_maintenance handles process without maintenance and supports legacy alias", {
  set.seed(42)
  df_std <- sim_wiener_path(t_max = 10, n_steps = 20, drift = 1.5, sigma2 = 0.5)

  # Standard path without duplicated times
  p_std <- plot_maintenance(df_std, xlab = "Time", ylab = "Wt")
  expect_s3_class(p_std, "ggplot")

  # Legacy function name and legacy 'time' parameter
  df_maint <- sim_wiener_maintenance(
    t_max = 15, n_steps = 21, drift = 2, sigma2 = 1, rho = 0.6, n_maint = 2
  )
  p_legacy <- plot_maintanance(df_maint, xlab = "Tempo", ylab = "Degradação", time = TRUE)
  expect_s3_class(p_legacy, "ggplot")
  expect_equal(p_legacy$labels$x, "Tempo")
  expect_equal(p_legacy$labels$y, "Degradação")
})

test_that("plot_maintenance validates inputs and raises informative errors", {
  expect_error(plot_maintenance("not_a_df"), "data frame")
  expect_error(plot_maintenance(data.frame()), "non-empty")
  expect_error(plot_maintenance(data.frame(foo = 1:5, bar = 1:5)), "must contain")
})


test_that("plot_exponential creates a valid combined patchwork/ggplot object", {
  p <- plot_exponential()

  expect_true(inherits(p, "patchwork") || inherits(p, "ggplot"))

  # Check that internal dataset uses English columns
  d <- p[[1]]$data
  expect_true(all(c("time", "lambda", "reliability", "hazard", "density", "lambda_factor") %in% names(d)))

  # Check default English labels on density subplot
  expect_equal(p[[1]]$labels$title, "Density")
  expect_equal(p[[1]]$labels$x, "Time")
  expect_equal(p[[1]]$labels$y, "f(t)")

  # Check that Cartesian origin padding is eliminated (expand = c(0, 0))
  expect_equal(p[[1]]$scales$get_scales("x")$expand, c(0, 0))
  expect_equal(p[[1]]$scales$get_scales("y")$expand, c(0, 0))

  # Test custom lambdas and horizon
  p_custom <- plot_exponential(lambdas = c(0.2, 0.8), t_max = 10, n_points = 50)
  expect_true(inherits(p_custom, "patchwork") || inherits(p_custom, "ggplot"))
})

test_that("gera_plot_exp legacy alias works with 3-element label vectors", {
  labs_01 <- c("Densidade", "Tempo", "f(t)")
  labs_02 <- c("Taxa de Falha", "Tempo", "\u03bb(t)")
  labs_03 <- c("Confiabilidade", "Tempo", "R(t)")

  p_legacy <- gera_plot_exp(labs_01, labs_02, labs_03)
  expect_true(inherits(p_legacy, "patchwork") || inherits(p_legacy, "ggplot"))
})

test_that("plot_exponential validates inputs and raises informative errors", {
  expect_error(plot_exponential(lambdas = -1), "lambdas")
  expect_error(plot_exponential(lambdas = c(1, -0.5)), "lambdas")
  expect_error(plot_exponential(t_max = 0), "t_max")
  expect_error(plot_exponential(t_max = -5), "t_max")
  expect_error(plot_exponential(n_points = 1), "n_points")
})

test_that("plot_weibull creates a valid combined patchwork/ggplot object", {
  p <- plot_weibull()

  expect_true(inherits(p, "patchwork") || inherits(p, "ggplot"))

  # Check that internal dataset uses English columns
  d <- p[[1]]$data
  expect_true(all(c("time", "gamma", "alpha", "reliability", "hazard", "density", "gamma_factor") %in% names(d)))

  # Check default English labels on density subplot
  expect_equal(p[[1]]$labels$title, "Density")
  expect_equal(p[[1]]$labels$x, "Time")
  expect_equal(p[[1]]$labels$y, "f(t)")

  # Check that Cartesian origin padding is eliminated (expand = c(0, 0))
  expect_equal(p[[1]]$scales$get_scales("x")$expand, c(0, 0))
  expect_equal(p[[1]]$scales$get_scales("y")$expand, c(0, 0))

  # Test custom gammas, alpha, and horizon
  p_custom <- plot_weibull(gammas = c(0.8, 1.2), alpha = 2.0, t_max = 8, n_points = 60)
  expect_true(inherits(p_custom, "patchwork") || inherits(p_custom, "ggplot"))
})

test_that("gera_plot_weibull legacy alias works with 3-element label vectors", {
  labs_01 <- c("Densidade", "Tempo", "f(t)")
  labs_02 <- c("Taxa de Falha", "Tempo", "\u03bb(t)")
  labs_03 <- c("Confiabilidade", "Tempo", "R(t)")

  p_legacy <- gera_plot_weibull(labs_01, labs_02, labs_03)
  expect_true(inherits(p_legacy, "patchwork") || inherits(p_legacy, "ggplot"))
})

test_that("plot_weibull validates inputs and raises informative errors", {
  expect_error(plot_weibull(gammas = -1), "gammas")
  expect_error(plot_weibull(gammas = c(1, -0.5)), "gammas")
  expect_error(plot_weibull(alpha = 0), "alpha")
  expect_error(plot_weibull(alpha = -2), "alpha")
  expect_error(plot_weibull(t_max = 0), "t_max")
  expect_error(plot_weibull(t_max = -5), "t_max")
  expect_error(plot_weibull(n_points = 1), "n_points")
})

test_that("plot_lognormal creates a valid combined patchwork/ggplot object", {
  p <- plot_lognormal()

  expect_true(inherits(p, "patchwork") || inherits(p, "ggplot"))

  # Check that internal dataset uses English columns
  d <- p[[1]]$data
  expect_true(all(c("time", "sigma", "mu", "reliability", "density", "hazard", "sigma_factor") %in% names(d)))

  # Check default English labels on density subplot
  expect_equal(p[[1]]$labels$title, "Density")
  expect_equal(p[[1]]$labels$x, "Time")
  expect_equal(p[[1]]$labels$y, "f(t)")

  # Check that Cartesian origin padding is eliminated (expand = c(0, 0))
  expect_equal(p[[1]]$scales$get_scales("x")$expand, c(0, 0))
  expect_equal(p[[1]]$scales$get_scales("y")$expand, c(0, 0))

  # Test custom sigmas, mu, and horizon
  p_custom <- plot_lognormal(sigmas = c(0.4, 0.9), mu = 0.5, t_max = 5, n_points = 50)
  expect_true(inherits(p_custom, "patchwork") || inherits(p_custom, "ggplot"))
})

test_that("gera_plot_lognormal legacy alias works with 3-element label vectors", {
  labs_01 <- c("Densidade", "Tempo", "f(t)")
  labs_02 <- c("Taxa de Falha", "Tempo", "\u03bb(t)")
  labs_03 <- c("Confiabilidade", "Tempo", "R(t)")

  p_legacy <- gera_plot_lognormal(labs_01, labs_02, labs_03)
  expect_true(inherits(p_legacy, "patchwork") || inherits(p_legacy, "ggplot"))
})

test_that("plot_lognormal validates inputs and raises informative errors", {
  expect_error(plot_lognormal(sigmas = -1), "sigmas")
  expect_error(plot_lognormal(sigmas = c(0.5, -0.2)), "sigmas")
  expect_error(plot_lognormal(t_max = 0), "t_max")
  expect_error(plot_lognormal(t_max = -5), "t_max")
  expect_error(plot_lognormal(n_points = 1), "n_points")
})

test_that("plot_censoring creates a valid combined patchwork/ggplot object", {
  p <- plot_censoring()

  expect_true(inherits(p, "patchwork") || inherits(p, "ggplot"))

  # Check that internal dataset uses English columns
  d <- p[[1]]$data
  expect_true(all(c("unit", "start_time", "end_time", "event", "scheme_id", "scheme_name") %in% names(d)))

  # Check default English labels on the first subplot
  expect_equal(p[[1]]$labels$title, "(a) Complete data")
  expect_equal(p[[1]]$labels$x, "Time")
  expect_equal(p[[1]]$labels$y, "Units")

  # Check that Cartesian origin padding is eliminated (expand = c(0, 0))
  expect_equal(p[[1]]$scales$get_scales("x")$expand, c(0, 0))

  # Test custom titles, labels, cutoff, and horizon
  p_custom <- plot_censoring(
    titles = c("Complete", "Type 1", "Type 2", "Random"),
    x_label = "Hours",
    y_label = "Device ID",
    experiment_end = 25,
    xlim = c(0, 30),
    show_end_line = FALSE
  )
  expect_true(inherits(p_custom, "patchwork") || inherits(p_custom, "ggplot"))
  expect_equal(p_custom[[1]]$labels$title, "Complete")
  expect_equal(p_custom[[1]]$labels$x, "Hours")
  expect_equal(p_custom[[1]]$labels$y, "Device ID")
})

test_that("plot_censura_all legacy alias works with Portuguese defaults", {
  p_legacy <- plot_censura_all()

  expect_true(inherits(p_legacy, "patchwork") || inherits(p_legacy, "ggplot"))
  expect_equal(p_legacy[[1]]$labels$title, "(a) Dados completos")
  expect_equal(p_legacy[[1]]$labels$x, "Tempos")
  expect_equal(p_legacy[[1]]$labels$y, "Equipamentos")
})

test_that("plot_censoring validates inputs and raises informative errors", {
  expect_error(plot_censoring(titles = c("A", "B")), "titles")
  expect_error(plot_censoring(titles = 1:4), "titles")
  expect_error(plot_censoring(xlim = c(10, 5)), "xlim")
  expect_error(plot_censoring(xlim = 10), "xlim")
  expect_error(plot_censoring(expand = 0), "expand")
})

test_that("plot_degradation creates a valid ggplot object", {
  p <- plot_degradation()

  expect_true(inherits(p, "ggplot"))

  # Check that internal dataset uses English columns
  expect_true(all(c("time", "degradation") %in% names(p$data)))
  expect_equal(nrow(p$data), 100)

  # Check default English labels
  expect_equal(p$labels$x, "Time")
  expect_equal(p$labels$y, "Degradation")

  # Check that Cartesian origin padding is eliminated (expand = c(0, 0))
  expect_equal(p$scales$get_scales("x")$expand, c(0, 0))
  expect_equal(p$scales$get_scales("y")$expand, c(0, 0))

  # Check reproducibility with seed
  p1 <- plot_degradation(seed = 42)
  p2 <- plot_degradation(seed = 42)
  expect_equal(p1$data, p2$data)

  # Custom parameters
  p_custom <- plot_degradation(
    failure_threshold = 15,
    threshold_label = "Alarm Limit",
    path_label = "Trajectory",
    failure_time_label = "Alarm Time",
    x_label = "Cycles",
    y_label = "Wear",
    t_max = 12,
    n_points = 50,
    xlim = c(0, 15),
    ylim = c(0, 30)
  )
  expect_true(inherits(p_custom, "ggplot"))
  expect_equal(p_custom$labels$x, "Cycles")
  expect_equal(p_custom$labels$y, "Wear")
  expect_equal(nrow(p_custom$data), 50)
})

test_that("gera_plot_degrada legacy alias works with Portuguese defaults and vectors", {
  p_legacy <- gera_plot_degrada()

  expect_true(inherits(p_legacy, "ggplot"))
  expect_equal(p_legacy$labels$x, "Tempo")
  expect_equal(p_legacy$labels$y, "Degrada\u00e7\u00e3o")

  # Legacy 5-element vector
  labs_vec <- c("Limite", "Curva", "Falha", "Horas", "Desgaste")
  p_vec <- gera_plot_degrada(labs_degradacao = labs_vec)
  expect_true(inherits(p_vec, "ggplot"))
  expect_equal(p_vec$labels$x, "Horas")
  expect_equal(p_vec$labels$y, "Desgaste")
})

test_that("plot_degradation validates inputs and raises informative errors", {
  expect_error(plot_degradation(failure_threshold = -5), "failure_threshold")
  expect_error(plot_degradation(failure_threshold = 0), "failure_threshold")
  expect_error(plot_degradation(t_max = 0), "t_max")
  expect_error(plot_degradation(t_max = -1), "t_max")
  expect_error(plot_degradation(n_points = 1), "n_points")
  expect_error(plot_degradation(xlim = c(10, 5)), "xlim")
  expect_error(plot_degradation(ylim = c(25, 0)), "ylim")
  expect_error(plot_degradation(expand = 0), "expand")
  expect_error(plot_degradation(labs_degradacao = c("A", "B")), "labs_degradacao")
})

test_that("plot_bathtub_curve creates a valid ggplot object", {
  p <- plot_bathtub_curve()

  expect_true(inherits(p, "ggplot"))

  # Check that internal dataset uses English columns
  expect_true(all(c("time", "hazard") %in% names(p$data)))
  expect_equal(nrow(p$data), 500)

  # Check default English labels
  expect_equal(p$labels$x, "Time")

  # Check that Cartesian origin padding is eliminated (expand = c(0, 0))
  expect_equal(p$scales$get_scales("x")$expand, c(0, 0))
  expect_equal(p$scales$get_scales("y")$expand, c(0, 0))

  # Custom parameters
  p_custom <- plot_bathtub_curve(
    t_phase1 = 20,
    t_phase2 = 80,
    useful_life_label = "Steady State",
    x_label = "Operating Hours",
    show_axis_text = TRUE,
    n_points = 200
  )
  expect_true(inherits(p_custom, "ggplot"))
  expect_equal(p_custom$labels$x, "Operating Hours")
  expect_equal(nrow(p_custom$data), 200)
})

test_that("gera_plot_banheira legacy alias works with Portuguese defaults and vectors", {
  p_legacy <- gera_plot_banheira()

  expect_true(inherits(p_legacy, "ggplot"))
  expect_equal(p_legacy$labels$x, "Tempo")

  # Legacy 4-element vector
  labs_vec <- c("Fase Inicial", "Fase Estável", "Fase Final", "Horas")
  p_vec <- gera_plot_banheira(labs_banheira = labs_vec)
  expect_true(inherits(p_vec, "ggplot"))
  expect_equal(p_vec$labels$x, "Horas")
})

test_that("plot_bathtub_curve validates inputs and raises informative errors", {
  expect_error(plot_bathtub_curve(t_max = 0), "t_max")
  expect_error(plot_bathtub_curve(t_max = -5), "t_max")
  expect_error(plot_bathtub_curve(t_phase1 = -1), "t_phase1")
  expect_error(plot_bathtub_curve(t_phase1 = 150), "t_phase1")
  expect_error(plot_bathtub_curve(t_phase2 = 25), "t_phase2")
  expect_error(plot_bathtub_curve(t_phase2 = 120), "t_phase2")
  expect_error(plot_bathtub_curve(n_points = 1), "n_points")
  expect_error(plot_bathtub_curve(xlim = c(100, 0)), "xlim")
  expect_error(plot_bathtub_curve(ylim = c(30, 0)), "ylim")
  expect_error(plot_bathtub_curve(expand = 0), "expand")
  expect_error(plot_bathtub_curve(labs_banheira = c("A", "B")), "labs_banheira")
})

test_that("plot_wiener_drift creates a valid ggplot object", {
  p <- plot_wiener_drift()

  expect_true(inherits(p, "ggplot"))

  # Check that internal dataset uses English columns
  expect_true(all(c("time", "degradation", "drift_factor") %in% names(p$data)))
  expect_equal(nrow(p$data), 202)  # 2 paths * 101 points

  # Check default English labels
  expect_equal(p$labels$x, "Time")
  expect_equal(p$labels$y, "Degradation")

  # Check that Cartesian origin padding is eliminated (expand = c(0, 0))
  expect_equal(p$scales$get_scales("x")$expand, c(0, 0))

  # Check reproducibility with seed
  p1 <- plot_wiener_drift(seed = 99)
  p2 <- plot_wiener_drift(seed = 99)
  expect_equal(p1$data, p2$data)

  # Custom parameters
  p_custom <- plot_wiener_drift(
    drifts = c(1, 8),
    sigma = 2,
    t_max = 30,
    n_steps = 50,
    x_label = "Hours",
    y_label = "Wear (um)"
  )
  expect_true(inherits(p_custom, "ggplot"))
  expect_equal(p_custom$labels$x, "Hours")
  expect_equal(p_custom$labels$y, "Wear (um)")
  expect_equal(nrow(p_custom$data), 102)  # 2 paths * 51 points
})

test_that("gera_plot_wiener legacy alias works with Portuguese defaults and vectors", {
  p_legacy <- gera_plot_wiener()

  expect_true(inherits(p_legacy, "ggplot"))
  expect_equal(p_legacy$labels$y, "Degrada\u00e7\u00e3o")
  expect_equal(p_legacy$labels$x, "Tempo")

  # Legacy 2-element vector (y, x)
  labs_vec <- c("Nível", "Horas")
  p_vec <- gera_plot_wiener(labs_wiener = labs_vec)
  expect_true(inherits(p_vec, "ggplot"))
  expect_equal(p_vec$labels$y, "Nível")
  expect_equal(p_vec$labels$x, "Horas")
})

test_that("plot_wiener_drift validates inputs and raises informative errors", {
  expect_error(plot_wiener_drift(drifts = 1), "drifts")
  expect_error(plot_wiener_drift(sigma = 0), "sigma")
  expect_error(plot_wiener_drift(sigma = -3), "sigma")
  expect_error(plot_wiener_drift(sigma2 = -1), "sigma2")
  expect_error(plot_wiener_drift(t_max = 0), "t_max")
  expect_error(plot_wiener_drift(t_max = -5), "t_max")
  expect_error(plot_wiener_drift(n_steps = 1), "n_steps")
  expect_error(plot_wiener_drift(xlim = c(20, 0)), "xlim")
  expect_error(plot_wiener_drift(expand = 0), "expand")
  expect_error(plot_wiener_drift(labs_wiener = c("A")), "labs_wiener")
})

test_that("plot_repair_types creates a valid combined patchwork/ggplot object", {
  p <- plot_repair_types()

  expect_true(inherits(p, "patchwork") || inherits(p, "ggplot"))

  # Check that internal dataset uses English columns
  expect_true(all(c("time", "degradation") %in% names(p[[1]]$data)))
  expect_true(all(c("time", "degradation") %in% names(p[[2]]$data)))
  expect_true(all(c("time", "degradation") %in% names(p[[3]]$data)))

  # Check default English titles
  expect_equal(p[[1]]$labels$title, "(a) Perfect Repair")
  expect_equal(p[[2]]$labels$title, "(b) Minimal Repair")
  expect_equal(p[[3]]$labels$title, "(c) Imperfect Repair")

  # Check default English axis labels
  expect_equal(p[[1]]$labels$x, "Time")
  expect_equal(p[[1]]$labels$y, "Degradation")

  # Check that Cartesian origin padding is eliminated (expand = c(0, 0))
  expect_equal(p[[1]]$scales$get_scales("x")$expand, c(0, 0))
  expect_equal(p[[1]]$scales$get_scales("y")$expand, c(0, 0))

  # Custom parameters
  p_custom <- plot_repair_types(
    titles = c("Full Overhaul", "Quick Patch", "Partial Overhaul"),
    x_label = "Operating Months",
    y_label = "Damage Index",
    t_maint = 5,
    t_max = 10
  )
  expect_true(inherits(p_custom, "patchwork") || inherits(p_custom, "ggplot"))
  expect_equal(p_custom[[1]]$labels$title, "Full Overhaul")
  expect_equal(p_custom[[2]]$labels$title, "Quick Patch")
  expect_equal(p_custom[[3]]$labels$title, "Partial Overhaul")
  expect_equal(p_custom[[1]]$labels$x, "Operating Months")
  expect_equal(p_custom[[1]]$labels$y, "Damage Index")
})

test_that("gera_plot_reparos legacy alias works with Portuguese defaults and vectors", {
  p_legacy <- gera_plot_reparos()

  expect_true(inherits(p_legacy, "patchwork") || inherits(p_legacy, "ggplot"))
  expect_equal(p_legacy[[1]]$labels$x, "Tempo")
  expect_equal(p_legacy[[1]]$labels$y, "Degrada\u00e7\u00e3o")
  expect_equal(p_legacy[[1]]$labels$title, "(a) Reparo Perfeito")
  expect_equal(p_legacy[[2]]$labels$title, "(b) Reparo M\u00ednimo")
  expect_equal(p_legacy[[3]]$labels$title, "(c) Reparo Imperfeito")

  # Legacy 5-element vector
  labs_vec <- c("Horas", "Dano", "T1", "T2", "T3")
  p_vec <- gera_plot_reparos(labs_reparos = labs_vec)
  expect_true(inherits(p_vec, "patchwork") || inherits(p_vec, "ggplot"))
  expect_equal(p_vec[[1]]$labels$x, "Horas")
  expect_equal(p_vec[[1]]$labels$y, "Dano")
  expect_equal(p_vec[[1]]$labels$title, "T1")
  expect_equal(p_vec[[2]]$labels$title, "T2")
  expect_equal(p_vec[[3]]$labels$title, "T3")
})

test_that("plot_repair_types validates inputs and raises informative errors", {
  expect_error(plot_repair_types(titles = c("A", "B")), "titles")
  expect_error(plot_repair_types(t_maint = -1), "t_maint")
  expect_error(plot_repair_types(t_maint = 0), "t_maint")
  expect_error(plot_repair_types(t_max = 3, t_maint = 4), "t_max")
  expect_error(plot_repair_types(expand = 0), "expand")
  expect_error(plot_repair_types(labs_reparos = c("A", "B")), "labs_reparos")
})

test_that("plot_maintenance_scheme creates a valid ggplot object", {
  p <- plot_maintenance_scheme()

  expect_true(inherits(p, "ggplot"))

  # Check that internal dataset uses English columns
  expect_true(all(c("Time", "Y") %in% names(p$data)))

  # Check default English labels
  expect_equal(p$labels$x, "Time")
  expect_equal(p$labels$y, "Degradation")

  # Check that Cartesian origin padding is eliminated (expand = c(0, 0))
  expect_equal(p$scales$get_scales("x")$expand, c(0, 0))
  expect_equal(p$scales$get_scales("y")$expand, c(0, 0))

  # Check reproducibility with seed
  p1 <- plot_maintenance_scheme(seed = 123)
  p2 <- plot_maintenance_scheme(seed = 123)
  expect_equal(p1$data, p2$data)

  # Custom parameters
  p_custom <- plot_maintenance_scheme(
    x_label = "Horas",
    y_label = "Desgaste",
    drift = 3,
    sigma2 = 1
  )
  expect_true(inherits(p_custom, "ggplot"))
  expect_equal(p_custom$labels$x, "Horas")
  expect_equal(p_custom$labels$y, "Desgaste")
})

test_that("gera_plot_scheme legacy alias works with Portuguese defaults and vectors", {
  p_legacy <- gera_plot_scheme()

  expect_true(inherits(p_legacy, "ggplot"))
  expect_equal(p_legacy$labels$x, "Tempo")
  expect_equal(p_legacy$labels$y, "Degrada\u00e7\u00e3o")

  # Legacy 2-element vector (x, y)
  labs_vec <- c("Período", "Medida")
  p_vec <- gera_plot_scheme(labs_scheme = labs_vec)
  expect_true(inherits(p_vec, "ggplot"))
  expect_equal(p_vec$labels$x, "Período")
  expect_equal(p_vec$labels$y, "Medida")
})

test_that("plot_maintenance_scheme validates inputs and raises informative errors", {
  expect_error(plot_maintenance_scheme(t_max = 0), "t_max")
  expect_error(plot_maintenance_scheme(t_max = -5), "t_max")
  expect_error(plot_maintenance_scheme(n_maint = 0), "n_maint")
  expect_error(plot_maintenance_scheme(intra_maint = 0), "intra_maint")
  expect_error(plot_maintenance_scheme(sigma2 = -1), "sigma2")
  expect_error(plot_maintenance_scheme(xlim = c(20, 0)), "xlim")
  expect_error(plot_maintenance_scheme(ylim = c(30, 0)), "ylim")
  expect_error(plot_maintenance_scheme(expand = 0), "expand")
  expect_error(plot_maintenance_scheme(labs_scheme = c("A")), "labs_scheme")
})

test_that("plot_simulation_bias creates a valid ggplot object", {
  mock_sim_data <- data.frame(
    n_system = rep(c(1, 10, 20), each = 4),
    mu = rep(c(4, 16), each = 2, length.out = 12),
    sigma2 = rep(c(1, 25), length.out = 12),
    n_main = 3,
    n_intra = 2,
    bias.mu_hat = c(-0.01, 0.02, -0.005, 0.001, -0.002, 0.004, -0.001, 0.002, 0.0, 0.001, 0.001, -0.001),
    bias.sigma_hat = c(-0.03, 0.01, -0.004, 0.002, -0.008, 0.006, -0.002, 0.001, 0.0, 0.002, 0.001, 0.0)
  )

  p <- plot_simulation_bias(mock_sim_data)

  expect_true(inherits(p, "ggplot"))

  # Check that internal dataset uses English columns
  expect_true(all(c("n_system", "parameter", "bias_value", "mu_expr", "sigma_expr") %in% names(p$data)))

  # Check default English labels
  expect_equal(p$labels$x, "Number of Systems")
  expect_equal(p$labels$y, "Bias")

  # Custom parameters
  p_custom <- plot_simulation_bias(
    mock_sim_data,
    x_label = "Sistemas",
    y_label = "Viés",
    breaks = c(1, 10, 20)
  )
  expect_true(inherits(p_custom, "ggplot"))
  expect_equal(p_custom$labels$x, "Sistemas")
  expect_equal(p_custom$labels$y, "Viés")
})

test_that("gera_plot_bias legacy alias works with Portuguese defaults and vectors", {
  mock_sim_data <- data.frame(
    n_system = rep(c(1, 10, 20), each = 4),
    mu = rep(c(4, 16), each = 2, length.out = 12),
    sigma2 = rep(c(1, 25), length.out = 12),
    n_main = 3,
    n_intra = 2,
    bias.mu_hat = rep(0.01, 12),
    bias.sigma_hat = rep(0.02, 12)
  )

  p_legacy <- gera_plot_bias(mock_sim_data)

  expect_true(inherits(p_legacy, "ggplot"))
  expect_equal(p_legacy$labels$x, "Número de Sistemas")
  expect_equal(p_legacy$labels$y, "Viés")

  # Legacy 2-element vector (x, y)
  labs_vec <- c("Qtd Sistemas", "Erro Médio")
  p_vec <- gera_plot_bias(mock_sim_data, labs_bias = labs_vec)
  expect_true(inherits(p_vec, "ggplot"))
  expect_equal(p_vec$labels$x, "Qtd Sistemas")
  expect_equal(p_vec$labels$y, "Erro Médio")
})

test_that("plot_simulation_bias validates inputs and raises informative errors", {
  expect_error(plot_simulation_bias("not a df"), "data")
  expect_error(plot_simulation_bias(data.frame(x = 1)), "missing required column")
  expect_error(plot_simulation_bias(data.frame(), expand = 0), "missing required column")
  expect_error(
    plot_simulation_bias(
      data.frame(
        n_system = 1, mu = 4, sigma2 = 1, n_main = 3, n_intra = 0,
        bias.mu_hat = 0, bias.sigma_hat = 0
      ),
      expand = 0
    ),
    "expand"
  )
  expect_error(
    plot_simulation_bias(
      data.frame(
        n_system = 1, mu = 4, sigma2 = 1, n_main = 3, n_intra = 0,
        bias.mu_hat = 0, bias.sigma_hat = 0
      ),
      labs_bias = c("A")
    ),
    "labs_bias"
  )
})

test_that("plot_wiener_maintenance_comparison creates a valid ggplot object", {
  p <- plot_wiener_maintenance_comparison(seed = 123)

  expect_true(inherits(p, "ggplot"))
  expect_equal(p$labels$x, "Time")
  expect_equal(p$labels$y, "Degradation")
  expect_equal(p$labels$title, "(I)")

  # Check zero Cartesian origin expansion
  x_scale <- p$scales$get_scales("x")
  y_scale <- p$scales$get_scales("y")
  expect_equal(x_scale$expand, c(0, 0))
  expect_equal(y_scale$expand, c(0, 0))

  # Custom parameters
  p_custom <- plot_wiener_maintenance_comparison(
    t_max = 10,
    n_maint = 2,
    intra_maint = 2,
    rho = c(0.8, 0.4),
    x_label = "Tempo",
    y_label = "Degradação",
    title = "Comparação",
    expand = c(0.05, 0.05)
  )
  expect_true(inherits(p_custom, "ggplot"))
  expect_equal(p_custom$labels$x, "Tempo")
  expect_equal(p_custom$labels$y, "Degradação")
  expect_equal(p_custom$labels$title, "Comparação")
  expect_equal(p_custom$scales$get_scales("x")$expand, c(0.05, 0.05))
})

test_that("gera_plot_xtyt legacy alias works with Portuguese defaults and vectors", {
  p_legacy <- gera_plot_xtyt(seed = 123)

  expect_true(inherits(p_legacy, "ggplot"))
  expect_equal(p_legacy$labels$x, "Tempo")
  expect_true(grepl("Degrada", p_legacy$labels$y))

  # Custom 4-element vector
  labs_vec <- c("Manutenção", "Natural", "Horas", "Nível")
  p_custom <- gera_plot_xtyt(labs_xtyt = labs_vec, seed = 123)
  expect_true(inherits(p_custom, "ggplot"))
  expect_equal(p_custom$labels$x, "Horas")
  expect_equal(p_custom$labels$y, "Nível")
})

test_that("plot_wiener_maintenance_comparison validates inputs and raises informative errors", {
  expect_error(plot_wiener_maintenance_comparison(t_max = -1), "t_max")
  expect_error(plot_wiener_maintenance_comparison(n_maint = 0), "n_maint")
  expect_error(plot_wiener_maintenance_comparison(intra_maint = 0), "intra_maint")
  expect_error(plot_wiener_maintenance_comparison(expand = 0), "expand")
  expect_error(plot_wiener_maintenance_comparison(labs_xtyt = c("A", "B")), "labs_xtyt")
})

test_that("plot_simulation_rmse creates a valid ggplot object", {
  mock_sim_data <- data.frame(
    n_system = rep(c(1, 10, 20), each = 4),
    mu = rep(c(4, 16), each = 2, length.out = 12),
    sigma2 = rep(c(1, 25), length.out = 12),
    n_main = 3,
    n_intra = 2,
    RMSE.mu_hat = rep(0.15, 12),
    RMSE.sigma_hat = rep(0.25, 12)
  )

  p <- plot_simulation_rmse(mock_sim_data)

  expect_true(inherits(p, "ggplot"))
  expect_true(all(c("n_system", "parameter", "rmse_value", "mu_expr", "sigma_expr") %in% names(p$data)))
  expect_equal(p$labels$x, "Number of Systems")
  expect_equal(p$labels$y, "RMSE")

  # Zero Cartesian origin expansion check
  expect_equal(p$scales$get_scales("x")$expand, c(0, 0))
  expect_equal(p$scales$get_scales("y")$expand, c(0, 0))

  # Custom parameters
  p_custom <- plot_simulation_rmse(
    mock_sim_data,
    x_label = "Sistemas",
    y_label = "REQM",
    breaks = c(1, 10, 20)
  )
  expect_true(inherits(p_custom, "ggplot"))
  expect_equal(p_custom$labels$x, "Sistemas")
  expect_equal(p_custom$labels$y, "REQM")
})

test_that("gera_plot_rmse legacy alias works with Portuguese defaults and vectors", {
  mock_sim_data <- data.frame(
    n_system = rep(c(1, 10, 20), each = 4),
    mu = rep(c(4, 16), each = 2, length.out = 12),
    sigma2 = rep(c(1, 25), length.out = 12),
    n_main = 3,
    n_intra = 2,
    RMSE.mu_hat = rep(0.15, 12),
    RMSE.sigma_hat = rep(0.25, 12)
  )

  p_legacy <- gera_plot_rmse(mock_sim_data)

  expect_true(inherits(p_legacy, "ggplot"))
  expect_equal(p_legacy$labels$x, "Número de Sistemas")
  expect_equal(p_legacy$labels$y, "REQM")

  # Legacy 2-element vector (x, y)
  labs_vec <- c("Qtd Sistemas", "Erro Quadrático Médio")
  p_vec <- gera_plot_rmse(mock_sim_data, labs_rmse = labs_vec)
  expect_true(inherits(p_vec, "ggplot"))
  expect_equal(p_vec$labels$x, "Qtd Sistemas")
  expect_equal(p_vec$labels$y, "Erro Quadrático Médio")
})

test_that("plot_simulation_rmse validates inputs and raises informative errors", {
  expect_error(plot_simulation_rmse("not a df"), "data")
  expect_error(plot_simulation_rmse(data.frame(x = 1)), "missing required column")
  expect_error(
    plot_simulation_rmse(
      data.frame(
        n_system = 1, mu = 4, sigma2 = 1, n_main = 3, n_intra = 0,
        RMSE.mu_hat = 0, RMSE.sigma_hat = 0
      ),
      expand = 0
    ),
    "expand"
  )
  expect_error(
    plot_simulation_rmse(
      data.frame(
        n_system = 1, mu = 4, sigma2 = 1, n_main = 3, n_intra = 0,
        RMSE.mu_hat = 0, RMSE.sigma_hat = 0
      ),
      labs_rmse = c("A")
    ),
    "labs_rmse"
  )
})

test_that("plot_simulation_coverage creates a valid ggplot object", {
  mock_sim_data <- data.frame(
    n_system = rep(c(1, 10, 20), each = 4),
    mu = rep(c(4, 16), each = 2, length.out = 12),
    sigma2 = rep(c(1, 25), length.out = 12),
    n_main = 3,
    n_intra = 2,
    CP_mu_hat = rep(0.94, 12),
    CP_sigma2_hat = rep(0.96, 12)
  )

  p <- plot_simulation_coverage(mock_sim_data)

  expect_true(inherits(p, "ggplot"))
  expect_true(all(c("n_system", "parameter", "coverage_value", "mu_expr", "sigma_expr") %in% names(p$data)))
  expect_equal(p$labels$x, "Number of Systems")
  expect_equal(p$labels$y, "Coverage Probability")

  # Expansion check on x axis
  expect_equal(p$scales$get_scales("x")$expand, c(0, 0))

  # Custom parameters
  p_custom <- plot_simulation_coverage(
    mock_sim_data,
    nominal_coverage = 0.90,
    x_label = "Sistemas",
    y_label = "Cobertura",
    breaks = c(1, 10, 20)
  )
  expect_true(inherits(p_custom, "ggplot"))
  expect_equal(p_custom$labels$x, "Sistemas")
  expect_equal(p_custom$labels$y, "Cobertura")
})

test_that("gera_plot_coveragep legacy alias works with Portuguese defaults and vectors", {
  mock_sim_data <- data.frame(
    n_system = rep(c(1, 10, 20), each = 4),
    mu = rep(c(4, 16), each = 2, length.out = 12),
    sigma2 = rep(c(1, 25), length.out = 12),
    n_main = 3,
    n_intra = 2,
    CP_mu_hat = rep(0.94, 12),
    CP_sigma2_hat = rep(0.96, 12)
  )

  p_legacy <- gera_plot_coveragep(mock_sim_data)

  expect_true(inherits(p_legacy, "ggplot"))
  expect_equal(p_legacy$labels$x, "Número de Sistemas")
  expect_equal(p_legacy$labels$y, "Probabilidade de Cobertura")

  # Legacy 2-element vector (x, y)
  labs_vec <- c("Qtd Sistemas", "Taxa Cobertura")
  p_vec <- gera_plot_coveragep(mock_sim_data, labs_coverage = labs_vec)
  expect_true(inherits(p_vec, "ggplot"))
  expect_equal(p_vec$labels$x, "Qtd Sistemas")
  expect_equal(p_vec$labels$y, "Taxa Cobertura")
})

test_that("plot_simulation_coverage validates inputs and raises informative errors", {
  expect_error(plot_simulation_coverage("not a df"), "data")
  expect_error(plot_simulation_coverage(data.frame(x = 1)), "missing required column")
  expect_error(
    plot_simulation_coverage(
      data.frame(
        n_system = 1, mu = 4, sigma2 = 1, n_main = 3, n_intra = 0,
        CP_mu_hat = 0.95, CP_sigma2_hat = 0.95
      ),
      nominal_coverage = 1.5
    ),
    "nominal_coverage"
  )
  expect_error(
    plot_simulation_coverage(
      data.frame(
        n_system = 1, mu = 4, sigma2 = 1, n_main = 3, n_intra = 0,
        CP_mu_hat = 0.95, CP_sigma2_hat = 0.95
      ),
      expand = 0
    ),
    "expand"
  )
  expect_error(
    plot_simulation_coverage(
      data.frame(
        n_system = 1, mu = 4, sigma2 = 1, n_main = 3, n_intra = 0,
        CP_mu_hat = 0.95, CP_sigma2_hat = 0.95
      ),
      labs_coverage = c("A")
    ),
    "labs_coverage"
  )
})

test_that("plot_simulation_variance_ratio creates a valid ggplot object", {
  mock_sim_data <- data.frame(
    n_system = rep(c(1, 10, 20), each = 4),
    mu = rep(c(4, 16), each = 2, length.out = 12),
    sigma2 = rep(c(1, 25), length.out = 12),
    n_main = 3,
    n_intra = 2,
    obs_ModVar_mu = rep(0.04, 12),
    obs_ModVar_sigma2 = rep(0.08, 12),
    obs_EmpVar_mu = rep(0.04, 12),
    obs_EmpVar_sigma2 = rep(0.08, 12)
  )

  p <- plot_simulation_variance_ratio(mock_sim_data)

  expect_true(inherits(p, "ggplot"))
  expect_true(all(c("n_system", "parameter", "ratio_value", "mu_expr", "sigma_expr") %in% names(p$data)))
  expect_equal(p$labels$x, "Number of Systems")
  expect_equal(p$labels$y, "Variance Ratio (Model / Empirical)")

  # Zero Cartesian origin expansion check
  expect_equal(p$scales$get_scales("x")$expand, c(0, 0))
  expect_equal(p$scales$get_scales("y")$expand, c(0, 0))

  # Custom parameters
  p_custom <- plot_simulation_variance_ratio(
    mock_sim_data,
    reference_ratio = 1.05,
    x_label = "Sistemas",
    y_label = "Razão de Variâncias",
    breaks = c(1, 10, 20)
  )
  expect_true(inherits(p_custom, "ggplot"))
  expect_equal(p_custom$labels$x, "Sistemas")
  expect_equal(p_custom$labels$y, "Razão de Variâncias")
})

test_that("gera_plot_ratiovar legacy alias works with Portuguese defaults and vectors", {
  mock_sim_data <- data.frame(
    n_system = rep(c(1, 10, 20), each = 4),
    mu = rep(c(4, 16), each = 2, length.out = 12),
    sigma2 = rep(c(1, 25), length.out = 12),
    n_main = 3,
    n_intra = 2,
    obs_ModVar_mu = rep(0.04, 12),
    obs_ModVar_sigma2 = rep(0.08, 12),
    obs_EmpVar_mu = rep(0.04, 12),
    obs_EmpVar_sigma2 = rep(0.08, 12)
  )

  p_legacy <- gera_plot_ratiovar(mock_sim_data)

  expect_true(inherits(p_legacy, "ggplot"))
  expect_equal(p_legacy$labels$x, "Número de Sistemas")
  expect_equal(p_legacy$labels$y, "Razão de Variâncias")

  # Legacy 2-element vector (x, y)
  labs_vec <- c("Qtd Sistemas", "Razão Var")
  p_vec <- gera_plot_ratiovar(mock_sim_data, labs_ratiovar = labs_vec)
  expect_true(inherits(p_vec, "ggplot"))
  expect_equal(p_vec$labels$x, "Qtd Sistemas")
  expect_equal(p_vec$labels$y, "Razão Var")
})

test_that("plot_simulation_variance_ratio validates inputs and raises informative errors", {
  expect_error(plot_simulation_variance_ratio("not a df"), "data")
  expect_error(plot_simulation_variance_ratio(data.frame(x = 1)), "missing required column")
  expect_error(
    plot_simulation_variance_ratio(
      data.frame(
        n_system = 1, mu = 4, sigma2 = 1, n_main = 3, n_intra = 0,
        obs_ModVar_mu = 1, obs_ModVar_sigma2 = 1,
        obs_EmpVar_mu = 1, obs_EmpVar_sigma2 = 1
      ),
      reference_ratio = "not num"
    ),
    "reference_ratio"
  )
  expect_error(
    plot_simulation_variance_ratio(
      data.frame(
        n_system = 1, mu = 4, sigma2 = 1, n_main = 3, n_intra = 0,
        obs_ModVar_mu = 1, obs_ModVar_sigma2 = 1,
        obs_EmpVar_mu = 1, obs_EmpVar_sigma2 = 1
      ),
      expand = 0
    ),
    "expand"
  )
  expect_error(
    plot_simulation_variance_ratio(
      data.frame(
        n_system = 1, mu = 4, sigma2 = 1, n_main = 3, n_intra = 0,
        obs_ModVar_mu = 1, obs_ModVar_sigma2 = 1,
        obs_EmpVar_mu = 1, obs_EmpVar_sigma2 = 1
      ),
      labs_ratiovar = c("A")
    ),
    "labs_ratiovar"
  )
})

test_that("plot_merit_functions creates a valid patchwork/ggplot composite", {
  p <- plot_merit_functions(drift = 3, sigma2 = 2, threshold = 50, t0 = 10, x0 = 20, t_max = 30)

  expect_true(inherits(p, "ggplot") || inherits(p, "patchwork"))

  # Check custom parameters and legacy vector arguments
  labs01 <- c("Densidade", "Tempo (h)", "f(t)")
  labs02 <- c("Acumulada", "Tempo (h)", "F(t)")
  p_custom <- plot_merit_functions(
    drift = 4,
    sigma2 = 1,
    threshold = 60,
    t0 = 5,
    x0 = 10,
    t_max = 25,
    labs_merito01 = labs01,
    labs_merito02 = labs02
  )
  expect_true(inherits(p_custom, "ggplot") || inherits(p_custom, "patchwork"))
})

test_that("gera_plot_merito legacy alias works as expected", {
  p_legacy <- gera_plot_merito(mu = 3, sigma = 2, alpha = 50, t0 = 10, x0 = 20, t_max = 30)

  expect_true(inherits(p_legacy, "ggplot") || inherits(p_legacy, "patchwork"))
})

test_that("plot_merit_functions validates inputs and raises informative errors", {
  expect_error(plot_merit_functions(drift = -1), "drift")
  expect_error(plot_merit_functions(sigma2 = -1), "sigma2")
  expect_error(plot_merit_functions(threshold = 10, x0 = 20), "threshold")
  expect_error(plot_merit_functions(t0 = -5), "t0")
  expect_error(plot_merit_functions(t0 = 20, t_max = 10), "t_max")
  expect_error(plot_merit_functions(step_size = -1), "step_size")
  expect_error(plot_merit_functions(labs_merito01 = c("A")), "labs_merito01")
  expect_error(plot_merit_functions(labs_merito02 = c("A")), "labs_merito02")
})

test_that("plot_diagnostic_qq creates a valid diagnostic composite plot", {
  sim_data <- sim_wiener_maintenance_path(
    t_max = 20,
    n_steps = 30,
    drift = 3,
    sigma2 = 2,
    rho = c(0.8, 0.4),
    n_maint = 2,
    obj_id = 1
  )

  p <- plot_diagnostic_qq(sim_data)
  expect_true(inherits(p, "ggplot") || inherits(p, "patchwork"))
  expect_true(grepl("AD Test:", p$patches$annotation$title))
  expect_true(grepl("p-value =", p$patches$annotation$title))

  # Custom parameters and exclude_indices
  p_custom <- plot_diagnostic_qq(
    sim_data,
    exclude_indices = c(1, 2),
    pp_title = "P-P",
    qq_title = "Q-Q"
  )
  expect_true(inherits(p_custom, "ggplot") || inherits(p_custom, "patchwork"))
  expect_true(grepl("AD Test:", p_custom$patches$annotation$title))
})

test_that("gera_plot_qqplot legacy alias works as expected", {
  sim_data <- sim_wiener_maintenance_path(
    t_max = 20,
    n_steps = 30,
    drift = 2,
    sigma2 = 1,
    rho = c(0.5),
    n_maint = 1,
    obj_id = 1
  )

  p_legacy <- gera_plot_qqplot(sim_data)
  expect_true(inherits(p_legacy, "ggplot") || inherits(p_legacy, "patchwork"))
  expect_true(grepl("AD Test:", p_legacy$patches$annotation$title))

  labs1 <- c("Teórico", "Empírico", "P-P")
  labs2 <- c("Teórico", "Amostral", "Q-Q")
  p_vec <- gera_plot_qqplot(sim_data, labs_qqplot01 = labs1, labs_qqplot02 = labs2)
  expect_true(inherits(p_vec, "ggplot") || inherits(p_vec, "patchwork"))
  expect_true(grepl("AD Test:", p_vec$patches$annotation$title))
})

test_that("plot_diagnostic_qq validates inputs and raises informative errors", {
  expect_error(plot_diagnostic_qq("not a df"), "data")
  expect_error(plot_diagnostic_qq(data.frame(x = 1)), "missing required column")
  expect_error(plot_diagnostic_qq(data.frame(Time = 1:2, Y = 1:2)), "at least 4 observations")
  expect_error(plot_diagnostic_qq(data.frame(Time = 1:10, Y = 1:10), labs_qqplot01 = c("A")), "labs_qqplot01")
  expect_error(plot_diagnostic_qq(data.frame(Time = 1:10, Y = 1:10), labs_qqplot02 = c("A")), "labs_qqplot02")
})

test_that("plot_exponential_degradation creates a valid ggplot object", {
  p <- plot_exponential_degradation(seed = 123)

  expect_true(inherits(p, "ggplot"))
  expect_true(all(c("time", "degradation", "unit") %in% names(p$data)))
  expect_equal(p$labels$x, "Time")
  expect_equal(p$labels$y, "Degradation")

  # Zero Cartesian origin expansion check
  expect_equal(p$scales$get_scales("x")$expand, c(0, 0))
  expect_equal(p$scales$get_scales("y")$expand, c(0, 0))

  # Custom parameters
  p_custom <- plot_exponential_degradation(
    t_max = 20,
    by = 4,
    rates = c(0.5, 0.25),
    unit_names = c("A", "B"),
    x_label = "Tempo",
    y_label = "Desgaste",
    expand = c(0.05, 0.05)
  )
  expect_true(inherits(p_custom, "ggplot"))
  expect_equal(p_custom$labels$x, "Tempo")
  expect_equal(p_custom$labels$y, "Desgaste")
})

test_that("gera_plot_degrada01 legacy alias works as expected", {
  p_legacy <- gera_plot_degrada01()

  expect_true(inherits(p_legacy, "ggplot"))
  expect_equal(p_legacy$labels$x, "Tempo")
  expect_true(grepl("Degrada", p_legacy$labels$y))

  # Custom 2-element vector
  p_custom <- gera_plot_degrada01(labs_degrada01 = c("Horas", "Nível"))
  expect_true(inherits(p_custom, "ggplot"))
  expect_equal(p_custom$labels$x, "Horas")
  expect_equal(p_custom$labels$y, "Nível")
})

test_that("plot_exponential_degradation validates inputs and raises informative errors", {
  expect_error(plot_exponential_degradation(t_max = -1), "t_max")
  expect_error(plot_exponential_degradation(by = -1), "by")
  expect_error(plot_exponential_degradation(rates = -1), "rates")
  expect_error(plot_exponential_degradation(rates = c(1, 2), unit_names = "A"), "unit_names")
  expect_error(plot_exponential_degradation(expand = 0), "expand")
  expect_error(plot_exponential_degradation(labs_degrada01 = c("A")), "labs_degrada01")
})

test_that("plot_exponential_reliability creates a valid ggplot object", {
  p <- plot_exponential_reliability()

  expect_true(inherits(p, "ggplot"))
  expect_true(all(c("time", "reliability") %in% names(p$data)))
  expect_equal(p$labels$x, "Time")
  expect_equal(p$labels$y, "R(t)")

  # Zero Cartesian origin expansion check
  expect_equal(p$scales$get_scales("x")$expand, c(0, 0))
  expect_equal(p$scales$get_scales("y")$expand, c(0, 0))

  # Custom parameters
  p_custom <- plot_exponential_reliability(
    mean_lifetime = 50,
    t_max = 200,
    highlight_median = FALSE,
    x_label = "Horas",
    y_label = "Sobrevivência",
    expand = c(0.05, 0.05)
  )
  expect_true(inherits(p_custom, "ggplot"))
  expect_equal(p_custom$labels$x, "Horas")
  expect_equal(p_custom$labels$y, "Sobrevivência")
})

test_that("gera_plot_confiabilidade legacy alias works as expected", {
  p_legacy <- gera_plot_confiabilidade()

  expect_true(inherits(p_legacy, "ggplot"))
  expect_equal(p_legacy$labels$x, "Tempo")
  expect_equal(p_legacy$labels$y, "R(t)")

  # Custom parameters
  p_custom <- gera_plot_confiabilidade(mean_lifetime = 150)
  expect_true(inherits(p_custom, "ggplot"))
})

test_that("plot_exponential_reliability validates inputs and raises informative errors", {
  expect_error(plot_exponential_reliability(mean_lifetime = -1), "mean_lifetime")
  expect_error(plot_exponential_reliability(t_max = -1), "t_max")
  expect_error(plot_exponential_reliability(n_points = 2), "n_points")
  expect_error(plot_exponential_reliability(expand = 0), "expand")
  expect_error(plot_exponential_reliability(labs_reliability = c("A")), "labs_reliability")
})

test_that("plot_reliability_ci creates a valid ggplot and data structure", {
  res <- plot_reliability_ci(
    drift = 3,
    sigma2 = 2,
    var_drift = 0.05,
    var_sigma2 = 0.05,
    threshold = 50,
    t0 = 10,
    x0 = 20,
    t_max = 30
  )

  expect_true(is.list(res))
  expect_true(all(c("plot", "data", "p", "df_visu") %in% names(res)))
  expect_true(inherits(res$plot, "ggplot"))
  expect_true(inherits(res$p, "ggplot"))
  expect_true(all(c("time", "reliability", "lower", "upper", "threshold") %in% names(res$data)))

  expect_equal(res$plot$labels$x, "Time")
  expect_equal(res$plot$labels$y, "Reliability")
  expect_equal(res$plot$labels$title, "(II)")

  # Zero Cartesian origin expansion check
  expect_equal(res$plot$scales$get_scales("x")$expand, c(0, 0))
  expect_equal(res$plot$scales$get_scales("y")$expand, c(0, 0))

  # Point-wise bounds between 0 and 1
  expect_true(all(res$data$lower >= 0 & res$data$lower <= 1))
  expect_true(all(res$data$upper >= 0 & res$data$upper <= 1))
  expect_true(all(res$data$lower <= res$data$upper))
})

test_that("plot_reliability_ic legacy alias works with Portuguese defaults and vectors", {
  res_legacy <- plot_reliability_ic(
    mu = 3,
    sigma2 = 2,
    var_mu = 0.05,
    var_sigma2 = 0.05,
    alpha = 50,
    t0 = 10,
    x0 = 20,
    t_max = 30
  )

  expect_true(is.list(res_legacy))
  expect_true(inherits(res_legacy$p, "ggplot"))
  expect_equal(res_legacy$p$labels$x, "Tempo")
  expect_equal(res_legacy$p$labels$y, "Confiabilidade")
})

test_that("plot_reliability_ci validates inputs and raises informative errors", {
  expect_error(plot_reliability_ci(drift = -1), "drift")
  expect_error(plot_reliability_ci(sigma2 = -1), "sigma2")
  expect_error(plot_reliability_ci(var_drift = -1), "var_drift")
  expect_error(plot_reliability_ci(var_sigma2 = -1), "var_sigma2")
  expect_error(plot_reliability_ci(threshold = 10, x0 = 20), "threshold")
  expect_error(plot_reliability_ci(t0 = -1), "t0")
  expect_error(plot_reliability_ci(t0 = 30, t_max = 20), "t_max")
  expect_error(plot_reliability_ci(step_size = -1), "step_size")
  expect_error(plot_reliability_ci(conf_level = 1.5), "conf_level")
  expect_error(plot_reliability_ci(expand = 0), "expand")
  expect_error(plot_reliability_ci(labs_ci = c("A")), "labs_ci")
})
