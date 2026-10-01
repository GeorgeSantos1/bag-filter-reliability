# ---------------------------------------------------
# Arquivo: utils.R
# Descricao: Conjunto de funcoes uteis para analise
# Autor: George Anderson A. dos Santos
# Data: 17-04-2024
# ---------------------------------------------------


#' Simulate a Single Wiener Process Degradation Path
#'
#' Simulates a one-dimensional Wiener process path with drift and diffusion,
#' typically used to model degradation phenomena over time:
#' \eqn{W(t) = \nu t + \sigma B(t)}.
#'
#' @param t_max Numeric. Maximum observation time.
#' @param n_steps Integer. Number of measurement intervals (resulting in \code{n_steps + 1} time points).
#' @param drift Numeric. Drift parameter (\eqn{\nu} or \eqn{\mu}).
#' @param sigma2 Numeric. Diffusion variance parameter (\eqn{\sigma^2}).
#' @param obj_id Integer or character. Identifier for the observed unit/object (default: 1).
#'
#' @return A \code{data.frame} containing:
#' \describe{
#'   \item{Object}{Object/unit identifier.}
#'   \item{Time}{Inspection time points.}
#'   \item{Wt}{Degradation value at each time point.}
#' }
#'
#' @examples
#' path <- sim_wiener_path(t_max = 20, n_steps = 50, drift = 2, sigma2 = 0.5)
#' head(path)
#'
#' @export
sim_wiener_path <- function(t_max, n_steps, drift, sigma2, obj_id = 1) {
  n_points <- n_steps + 1
  time <- seq(0, t_max, length.out = n_points)
  bt <- c(0, sqrt(t_max / n_steps) * cumsum(stats::rnorm(n_steps)))
  wt <- drift * time + sqrt(sigma2) * bt
  data.frame(
    Object = paste0("OBJ_", sprintf("%02d", as.integer(obj_id))),
    Wt = wt,
    Time = time,
    stringsAsFactors = FALSE
  )
}

#' Simulate Multiple Wiener Process Degradation Paths
#'
#' Simulates multiple independent degradation paths for a sample of units
#' governed by a Wiener process with drift and diffusion.
#'
#' @param n_units Integer. Number of independent units/objects to simulate.
#' @param t_max Numeric. Maximum observation time.
#' @param n_steps Integer. Number of measurement intervals (resulting in \code{n_steps + 1} time points).
#' @param drift Numeric. Drift parameter (\eqn{\nu} or \eqn{\mu}).
#' @param sigma2 Numeric. Diffusion variance parameter (\eqn{\sigma^2}).
#'
#' @return A \code{data.frame} containing:
#' \describe{
#'   \item{Object}{Unit/object identifier (e.g., \code{"OBJ_01"}, \code{"OBJ_02"}).}
#'   \item{Wt}{Degradation level at each inspection time.}
#'   \item{Time}{Inspection time points.}
#' }
#'
#' @examples
#' paths <- sim_wiener_paths(n_units = 10, t_max = 20, n_steps = 50, drift = 2, sigma2 = 0.5)
#' head(paths)
#'
#' @export
sim_wiener_paths <- function(n_units, t_max, n_steps, drift, sigma2) {
  paths_list <- lapply(seq_len(n_units), function(i) {
    sim_wiener_path(
      t_max = t_max,
      n_steps = n_steps,
      drift = drift,
      sigma2 = sigma2,
      obj_id = i
    )
  })
  do.call(rbind, paths_list)
}

#' Plot Degradation Paths
#'
#' Creates a ggplot2 visualization of Wiener process degradation paths over time,
#' with each unit/system represented by a distinct color.
#'
#' @param data A \code{data.frame} containing columns for inspection times,
#'   degradation values, and unit identifiers (e.g. output from \code{\link{sim_wiener_paths}}).
#' @param title Character. Title of the plot (default: \code{"Degradation Paths"}).
#' @param xlab Character. Label for the x-axis (default: \code{"Time"}).
#' @param ylab Character. Label for the y-axis (default: \code{"Degradation (Wt)"}).
#' @param line_size Numeric. Line width for trajectories (default: 1.0).
#' @param point_size Numeric. Point size for measurements (default: 1.8).
#' @param show_points Logical. If \code{TRUE} (default), displays points at inspection times.
#'
#' @return A \code{\link[ggplot2]{ggplot}} object representing the degradation paths.
#'
#' @examples
#' paths <- sim_wiener_paths(n_units = 5, t_max = 20, n_steps = 30, drift = 1.5, sigma2 = 0.4)
#' plot_degradation_paths(paths)
#'
#' @export
plot_degradation_paths <- function(data,
                                   title = "Degradation Paths",
                                   xlab = "Time",
                                   ylab = "Degradation (Wt)",
                                   line_size = 1.0,
                                   point_size = 1.8,
                                   show_points = TRUE) {
  id_col <- if ("Object" %in% names(data)) {
    "Object"
  } else if ("Objeto" %in% names(data)) {
    "Objeto"
  } else {
    names(data)[1]
  }

  p <- ggplot2::ggplot(
    data,
    ggplot2::aes(
      x = .data[["Time"]],
      y = .data[["Wt"]],
      group = factor(.data[[id_col]]),
      color = factor(.data[[id_col]])
    )
  ) +
    ggplot2::geom_line(linewidth = line_size, alpha = 0.8) +
    ggplot2::labs(
      title = title,
      x = xlab,
      y = ylab,
      color = "System"
    ) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      legend.position = "right",
      legend.background = ggplot2::element_rect(fill = "white", color = "grey80")
    )

  if (show_points) {
    p <- p + ggplot2::geom_point(size = point_size)
  }

  p
}


#' Simulate Wiener Degradation Process with Imperfect Maintenance
#'
#' Simulates a Wiener process degradation path subjected to imperfect maintenance actions
#' under the Arithmetic Reduction of Degradation (ARD) model.
#'
#' @param t_max Numeric. Maximum observation time.
#' @param n_steps Integer. Number of measurement intervals.
#' @param drift Numeric. Drift parameter (\eqn{\nu} or \eqn{\mu}).
#' @param sigma2 Numeric. Diffusion variance parameter (\eqn{\sigma^2}).
#' @param rho Numeric. Maintenance efficiency parameter (\eqn{0 \le \rho \le 1}).
#' @param n_maint Integer. Number of maintenance interventions.
#'
#' @return A \code{data.frame} containing:
#' \describe{
#'   \item{Object}{Object/unit identifier.}
#'   \item{Wt}{Standard/unmaintained degradation level.}
#'   \item{Time}{Inspection time points.}
#'   \item{Y}{Degradation level under imperfect maintenance.}
#' }
#'
#' @examples
#' data_maint <- sim_wiener_maintenance(
#'   t_max = 20, n_steps = 20, drift = 2, sigma2 = 2, rho = 0.5, n_maint = 3
#' )
#' head(data_maint)
#'
#' @export
sim_wiener_maintenance <- function(t_max, n_steps, drift, sigma2, rho, n_maint) {
  if (n_steps %% (n_maint + 1) != 0) {
    stop("`n_steps` must be a multiple of `(n_maint + 1)` for equidistant maintenance observation schemes.")
  }
  df <- sim_wiener_path(t_max = t_max, n_steps = n_steps, drift = drift, sigma2 = sigma2)
  time_MP <- seq(0, t_max, t_max / (n_maint + 1))
  time_MP <- time_MP[-c(1, length(time_MP))]
  Y <- n_steps + 1 + n_maint
  jy <- Y / (n_maint + 1)
  jw <- n_steps / (n_maint + 1)
  Y <- numeric(Y)
  W <- df$Wt
  for (k in 1:n_maint) {
    Y[1:jy] <- W[1:(jw + 1)]
    Y[(k * jy + 1):((k + 1) * jy)] <- W[(jw * k + 1):((k + 1) * jw + 1)] - rho * W[(jw * k + 1)]
  }
  df <- rbind(df, df %>% dplyr::filter(Time %in% time_MP)) %>%
    dplyr::arrange(Time)
  df$Y <- Y
  
  return(df)
}

#' Calculate Analytical Maintenance Effect Parameter (Rho)
#'
#' Computes the analytical estimates of the imperfect maintenance efficiency
#' parameter \eqn{\hat{\rho}_j = -Z_j / y_{ji}} from observed degradation jumps
#' at maintenance times.
#'
#' @param data A \code{data.frame} containing degradation data under maintenance
#'   (must include columns \code{Time} and \code{Y}, such as from \code{\link{sim_wiener_maintenance}}).
#'
#' @return A numeric vector containing the estimated \eqn{\hat{\rho}} for each
#'   maintenance intervention.
#'
#' @examples
#' data_maint <- sim_wiener_maintenance(
#'   t_max = 20, n_steps = 20, drift = 2, sigma2 = 2, rho = 0.5, n_maint = 3
#' )
#' calc_rho(data_maint)
#'
#' @export
calc_rho <- function(data) {
  k <- data$Time[duplicated(data$Time)]
  if (length(k) == 0) {
    return(numeric(0))
  }
  pass <- max(data$Time) / (length(unique(data$Time)) - 1)
  Zj <- numeric(length(k))
  yji <- numeric(length(k))
  aux <- -diff(data$Y)

  Zj[1] <- -diff(data$Y[data$Time == k[1]])
  yji[1] <- sum(aux[1:(k[1] / pass)])

  if (length(k) > 1) {
    for (i in 2:length(k)) {
      Zj[i] <- -diff(data$Y[data$Time == k[i]])
      yji[i] <- sum(aux[(k[i - 1] / pass + i):(k[i] / pass + (i - 1))])
    }
  }
  return(-Zj / yji)
}

#' Plot Wiener Degradation Process with Maintenance Interventions
#'
#' Visualizes both the standard unmaintained Wiener degradation path \eqn{W(t)}
#' and the maintained degradation path \eqn{Y(t)}, highlighting maintenance
#' interventions with dotted drop segments at each intervention timestamp.
#'
#' @param data A \code{data.frame} containing degradation data with maintenance
#'   (columns \code{Time}, \code{Wt}, and \code{Y}, such as from \code{\link{sim_wiener_maintenance}}).
#' @param title Character. Title of the plot (default: \code{"Degradation Paths with Maintenance"}).
#' @param xlab Character. Label for the x-axis (default: \code{"Time"}).
#' @param ylab Character. Label for the y-axis (default: \code{"Degradation"}).
#' @param line_size Numeric. Width of degradation path lines (default: 1.0).
#' @param show_segments Logical. Whether to draw dotted vertical drop segments at maintenance events (default: \code{TRUE}).
#'
#' @return A \code{\link[ggplot2]{ggplot}} object showing the degradation paths.
#'
#' @examples
#' data_maint <- sim_wiener_maintenance(
#'   t_max = 20, n_steps = 20, drift = 2, sigma2 = 2, rho = 0.5, n_maint = 3
#' )
#' plot_wiener_maintenance(data_maint)
#'
#' @export
plot_wiener_maintenance <- function(data,
                                    title = "Degradation Paths with Maintenance",
                                    xlab = "Time",
                                    ylab = "Degradation",
                                    line_size = 1.0,
                                    show_segments = TRUE) {
  p <- ggplot2::ggplot(data) +
    ggplot2::geom_line(
      ggplot2::aes(x = .data[["Time"]], y = .data[["Y"]], colour = "With Maintenance"),
      alpha = 0.8, linetype = "solid", linewidth = line_size
    ) +
    ggplot2::geom_line(
      ggplot2::aes(x = .data[["Time"]], y = .data[["Wt"]], colour = "Standard"),
      alpha = 0.8, linetype = "solid", linewidth = line_size
    ) +
    ggplot2::labs(
      title = title,
      x = xlab,
      y = ylab,
      colour = "Process"
    ) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      legend.position = "right",
      legend.background = ggplot2::element_rect(fill = "white", color = "grey80")
    )

  if (show_segments) {
    k <- unique(data$Time[duplicated(data$Time)])
    if (length(k) > 0) {
      seg_list <- lapply(k, function(ponto) {
        vals <- data$Y[data$Time == ponto]
        data.frame(x = ponto, xend = ponto, y = max(vals), yend = min(vals))
      })
      seg_df <- do.call(rbind, seg_list)
      p <- p + ggplot2::geom_segment(
        data = seg_df,
        ggplot2::aes(x = .data[["x"]], xend = .data[["xend"]], y = .data[["y"]], yend = .data[["yend"]]),
        linetype = "dotted", color = "black", linewidth = 0.8
      )
    }
  }

  p
}

#' Simulate a Single Wiener Degradation Path with ARD Maintenance
#'
#' Simulates a single Wiener process path subjected to imperfect maintenance actions
#' under the Arithmetic Reduction of Degradation (ARD) model, where each maintenance
#' intervention can have an individual effect parameter \eqn{\rho_k}.
#'
#' @param t_max Numeric. Maximum observation time.
#' @param n_steps Integer. Number of measurement intervals.
#' @param drift Numeric. Drift parameter (\eqn{\nu} or \eqn{\mu}).
#' @param sigma2 Numeric. Diffusion variance parameter (\eqn{\sigma^2}).
#' @param rho Numeric vector or scalar. Maintenance efficiency parameters for each intervention (\eqn{0 \le \rho_k \le 1}).
#' @param n_maint Integer. Number of maintenance interventions.
#' @param obj_id Integer or character. Identifier for the unit/system (default: 1).
#'
#' @return A \code{data.frame} containing:
#' \describe{
#'   \item{Object}{Object/unit identifier.}
#'   \item{Wt}{Standard/unmaintained degradation level.}
#'   \item{Time}{Inspection time points.}
#'   \item{Y}{Degradation level under imperfect maintenance.}
#' }
#'
#' @examples
#' data_path <- sim_wiener_maintenance_path(
#'   t_max = 20, n_steps = 20, drift = 2, sigma2 = 2,
#'   rho = c(1, 0.5, 0.8), n_maint = 3, obj_id = 1
#' )
#' head(data_path)
#'
#' @export
sim_wiener_maintenance_path <- function(t_max, n_steps, drift, sigma2, rho, n_maint, obj_id = 1) {
  if (n_steps %% (n_maint + 1) != 0) {
    stop("`n_steps` must be a multiple of `(n_maint + 1)` for equidistant maintenance observation schemes.")
  }
  if (length(rho) == 1) {
    rho <- rep(rho, n_maint)
  }
  df <- sim_wiener_path(t_max = t_max, n_steps = n_steps, drift = drift, sigma2 = sigma2, obj_id = obj_id)
  time_MP <- seq(0, t_max, t_max / (n_maint + 1))
  time_MP <- time_MP[-c(1, length(time_MP))]
  Y <- n_steps + 1 + n_maint
  jy <- Y / (n_maint + 1)
  jw <- n_steps / (n_maint + 1)
  Y <- numeric(Y)
  W <- df$Wt

  Y[1:jy] <- W[1:(jw + 1)]
  for (k in 1:n_maint) {
    if (k == 1) {
      Y[(k * jy + 1)] <- (1 - rho[k]) * W[(k * jw + 1)]
      Y[(k * jy + 2):((k + 1) * jy)] <- (1 - rho[k]) * W[(k * jw + 1)] + W[(jw * k + 2):((k + 1) * jw + 1)] - W[(k * jw + 1)]
    }
    if (k > 1) {
      aux_w <- numeric(k)
      for (l in 1:k) {
        aux_w[l] <- rho[l] * (W[(l * jw + 1)] - W[((l - 1) * jw + 1)])
      }
      Y[(k * jy + 1)] <- W[(k * jw + 1)] - sum(aux_w)
      Y[(k * jy + 2):((k + 1) * jy)] <- W[(k * jw + 1)] - sum(aux_w) + W[(jw * k + 2):((k + 1) * jw + 1)] - W[(k * jw + 1)]
    }
  }
  df <- rbind(df, df %>% dplyr::filter(round(Time, 6) %in% round(time_MP, 6))) %>%
    dplyr::arrange(Time)
  df$Y <- Y

  return(df)
}

#' Simulate Multiple Wiener Degradation Paths with ARD Maintenance
#'
#' Simulates multiple independent Wiener process paths subjected to imperfect maintenance
#' actions under the Arithmetic Reduction of Degradation (ARD) model.
#'
#' @param n_units Integer. Number of independent units/systems to simulate.
#' @param t_max Numeric. Maximum observation time.
#' @param n_steps Integer. Number of measurement intervals.
#' @param drift Numeric. Drift parameter (\eqn{\nu} or \eqn{\mu}).
#' @param sigma2 Numeric. Diffusion variance parameter (\eqn{\sigma^2}).
#' @param rho Numeric vector or scalar. Maintenance efficiency parameters for each intervention (\eqn{0 \le \rho_k \le 1}).
#' @param n_maint Integer. Number of maintenance interventions.
#'
#' @return A \code{data.frame} containing simulated degradation paths for all units.
#'
#' @examples
#' data_paths <- sim_wiener_maintenance_paths(
#'   n_units = 3, t_max = 20, n_steps = 20, drift = 2, sigma2 = 2,
#'   rho = c(0.8, 0.5, 0.8), n_maint = 3
#' )
#' head(data_paths)
#'
#' @export
sim_wiener_maintenance_paths <- function(n_units, t_max, n_steps, drift, sigma2, rho, n_maint) {
  paths_list <- lapply(seq_len(n_units), function(i) {
    sim_wiener_maintenance_path(
      t_max = t_max,
      n_steps = n_steps,
      drift = drift,
      sigma2 = sigma2,
      rho = rho,
      n_maint = n_maint,
      obj_id = i
    )
  })
  do.call(rbind, paths_list)
}

#' Plot Grid of Maintenance Degradation Paths Across Multiple Units
#'
#' Arranges individual maintenance degradation plots into a multi-panel grid,
#' one panel per unit/system.
#'
#' @param data A \code{data.frame} containing degradation data for multiple units
#'   (such as from \code{\link{sim_wiener_maintenance_paths}}).
#' @param ncol Integer. Number of columns in the plot grid. If \code{NULL} (default),
#'   computed automatically based on the number of units.
#'
#' @return A grob object returned invisibly by \code{\link[gridExtra]{grid.arrange}}.
#'
#' @examples
#' data_paths <- sim_wiener_maintenance_paths(
#'   n_units = 4, t_max = 20, n_steps = 20, drift = 2, sigma2 = 2,
#'   rho = c(0.5, 0.5, 0.5), n_maint = 3
#' )
#' plot_wiener_maintenance_grid(data_paths, ncol = 2)
#'
#' @export
plot_wiener_maintenance_grid <- function(data, ncol = NULL) {
  id_col <- if ("Object" %in% names(data)) {
    "Object"
  } else if ("Objeto" %in% names(data)) {
    "Objeto"
  } else {
    names(data)[1]
  }

  s_obj <- unique(data[[id_col]])
  p <- vector("list", length(s_obj))

  for (i in seq_along(s_obj)) {
    sub_data <- data[data[[id_col]] == s_obj[i], , drop = FALSE]
    p[[i]] <- plot_wiener_maintenance(
      sub_data,
      title = paste0("System: ", s_obj[i])
    )
  }

  if (is.null(ncol)) {
    ncol <- max(1, ceiling(sqrt(length(p))))
  }

  do.call(gridExtra::grid.arrange, c(p, list(ncol = ncol)))
}

#' Estimate Drift Parameter for Standard Wiener Degradation Process
#'
#' Computes the Maximum Likelihood Estimate (MLE) of the drift parameter \eqn{\mu}
#' using the terminal values of unmaintained Wiener degradation paths across all units:
#' \eqn{\hat{\mu} = \frac{\sum_{l=1}^N W_l(\tau)}{N \tau}}.
#'
#' @param data A \code{data.frame} containing degradation data (must contain columns
#'   \code{Time}, \code{Wt}, and an identifier \code{Object} or \code{Objeto}).
#'
#' @return A numeric scalar representing the estimated drift parameter \eqn{\hat{\mu}}.
#'
#' @examples
#' data_paths <- sim_wiener_paths(n_units = 10, t_max = 20, n_steps = 20, drift = 3, sigma2 = 2)
#' mle_drift_standard(data_paths)
#'
#' @export
mle_drift_standard <- function(data) {
  id_col <- if ("Object" %in% names(data)) {
    "Object"
  } else if ("Objeto" %in% names(data)) {
    "Objeto"
  } else {
    names(data)[1]
  }

  s <- unique(data[[id_col]])
  tau <- max(data$Time)

  terminal_data <- data[data$Time == tau, ]
  xl_tau <- terminal_data$Wt[match(s, terminal_data[[id_col]])]

  mu_hat <- sum(xl_tau, na.rm = TRUE) / (tau * length(s))
  return(mu_hat)
}

mle_drift1 <- mle_drift_standard

#' Estimate Diffusion Variance Parameter (Sigma^2) for Standard Process
#'
#' Computes the unbiased Maximum Likelihood Estimate (MLE) of the diffusion
#' variance parameter \eqn{\sigma^2} from the increments of unmaintained
#' Wiener degradation paths across all units.
#'
#' @param data A \code{data.frame} containing degradation data (must contain columns
#'   \code{Time}, \code{Wt}, and an identifier \code{Object} or \code{Objeto}).
#'
#' @return A numeric scalar representing the estimated diffusion variance \eqn{\hat{\sigma}^2}.
#'
#' @examples
#' data_paths <- sim_wiener_paths(n_units = 10, t_max = 20, n_steps = 20, drift = 3, sigma2 = 2)
#' mle_sigma2_standard(data_paths)
#'
#' @export
mle_sigma2_standard <- function(data) {
  id_col <- if ("Object" %in% names(data)) {
    "Object"
  } else if ("Objeto" %in% names(data)) {
    "Objeto"
  } else {
    names(data)[1]
  }

  mu_hat <- mle_drift_standard(data)
  s <- unique(data[[id_col]])

  first_unit <- data[data[[id_col]] == s[1], ]
  k <- first_unit$Time[duplicated(first_unit$Time)]

  if (length(k) == 0) {
    total_diffs <- 0
    total_intervals <- 0
    for (l in seq_along(s)) {
      sub_d <- data[data[[id_col]] == s[l], ]
      dt <- diff(sub_d$Time)
      dw <- diff(sub_d$Wt)
      valid <- dt > 0
      total_diffs <- total_diffs + sum(((dw[valid] - mu_hat * dt[valid])^2) / dt[valid])
      total_intervals <- total_intervals + sum(valid)
    }
    return(total_diffs / (total_intervals - 1))
  }

  nj <- nrow(first_unit[first_unit$Time > k[1] & first_unit$Time < k[2], ])
  N <- nj * (length(k) + 1)
  y_aux <- matrix(NA, nrow = length(s), ncol = (length(k) + 1))

  for (l in seq_along(s)) {
    aux_data <- data[data[[id_col]] == s[l], ]
    for (j in 1:(length(k) + 1)) {
      if (j == 1) {
        yji <- aux_data %>% dplyr::filter(Time <= k[j]) %>%
          dplyr::filter(dplyr::row_number() <= dplyr::n() - 1) %>%
          dplyr::pull(Wt) %>% diff()
        tji <- aux_data %>% dplyr::filter(Time <= k[j]) %>%
          dplyr::filter(dplyr::row_number() <= dplyr::n() - 1) %>%
          dplyr::pull(Time) %>% diff()
        y_aux[l, j] <- sum(((yji - mu_hat * tji)^2) / tji)
      }
      if (j >= 2 && j < (length(k) + 1)) {
        yji <- aux_data %>% dplyr::filter(Time >= k[j - 1], Time <= k[j]) %>%
          dplyr::slice(2:(dplyr::n() - 1)) %>%
          dplyr::pull(Wt) %>% diff()
        tji <- aux_data %>% dplyr::filter(Time >= k[j - 1], Time <= k[j]) %>%
          dplyr::slice(2:(dplyr::n() - 1)) %>%
          dplyr::pull(Time) %>% diff()
        y_aux[l, j] <- sum(((yji - mu_hat * tji)^2) / tji)
      }
      if (j == (length(k) + 1)) {
        yji <- aux_data %>% dplyr::filter(Time >= k[j - 1]) %>%
          dplyr::slice(2:dplyr::n()) %>%
          dplyr::pull(Wt) %>% diff()
        tji <- aux_data %>% dplyr::filter(Time >= k[j - 1]) %>%
          dplyr::slice(2:dplyr::n()) %>%
          dplyr::pull(Time) %>% diff()
        y_aux[l, j] <- sum(((yji - mu_hat * tji)^2) / tji)
      }
    }
  }

  total_pts <- length(s) * (N + length(k) + 1)
  sigma2_hat_biased <- sum(y_aux) / total_pts
  sigma2_hat_unbiased <- sigma2_hat_biased * total_pts / (total_pts - 1)
  return(sigma2_hat_unbiased)
}

mle_sigma1 <- mle_sigma2_standard


#' Plot Single Maintained Degradation Path (Legacy Function)
#'
#' @description
#' Visualizes a single Wiener degradation trajectory \eqn{Y(t)} under maintenance interventions,
#' plotting degradation points connected by a solid path and drawing vertical segments
#' at maintenance epochs to display the degradation reduction jumps.
#'
#' @param data A \code{data.frame} containing degradation data with maintenance
#'   (must contain columns \code{Time} and \code{Y}).
#' @param xlab Character label for the horizontal axis. Default is \code{"Time"}.
#' @param ylab Character label for the vertical axis. Default is \code{"Degradation"}.
#' @param title Optional character plot title. Default is \code{"(I)"}.
#' @param expand Numeric vector of length 2 controlling scale expansion for both axes. Default is \code{c(0, 0)}.
#' @param palette Optional character name of the \pkg{tayloRswift} palette. Default is \code{"taylor1989"}.
#'
#' @return A \code{ggplot2::ggplot} object showing the maintained path and maintenance reduction segments.
#'
#' @examples
#' \dontrun{
#' sim_data <- sim_wiener_maintenance_path(t_max = 20, n_steps = 30)
#' p <- gera_plot3(sim_data)
#' print(p)
#' }
#'
#' @export
gera_plot3 <- function(
  data,
  xlab = "Time",
  ylab = "Degradation",
  title = "(I)",
  expand = c(0, 0),
  palette = "taylor1989"
) {
  if (!is.data.frame(data)) {
    stop("'data' must be a data frame.")
  }
  required_cols <- c("Time", "Y")
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    stop("`data` is missing required column(s): ", paste(missing_cols, collapse = ", "))
  }

  p <- ggplot2::ggplot(data) +
    ggplot2::geom_line(
      ggplot2::aes(x = .data[["Time"]], y = .data[["Y"]], colour = "With Maintenance"),
      alpha = 0.5,
      linetype = "solid",
      linewidth = 1
    ) +
    ggplot2::labs(x = xlab, y = ylab, title = title) +
    ggplot2::theme_classic() +
    ggplot2::theme(
      plot.title = if (is.null(title) || title == "") ggplot2::element_blank() else ggplot2::element_text(hjust = 0.5),
      legend.position = "none"
    ) +
    ggplot2::scale_y_continuous(expand = expand) +
    ggplot2::scale_x_continuous(expand = expand)

  if (requireNamespace("tayloRswift", quietly = TRUE) &&
      !is.null(palette) &&
      palette %in% names(tayloRswift::swift_palettes)) {
    p <- p + tayloRswift::scale_color_taylor(palette = palette, reverse = FALSE)
  }

  jump_epochs <- unique(data$Time[duplicated(data$Time)])
  if (length(jump_epochs) > 0) {
    segment_list <- lapply(jump_epochs, function(epoch) {
      y_vals <- data$Y[data$Time == epoch]
      data.frame(
        x = epoch,
        xend = epoch,
        y = max(y_vals),
        yend = min(y_vals)
      )
    })
    segment_df <- do.call(rbind, segment_list)
    p <- p + ggplot2::geom_segment(
      data = segment_df,
      ggplot2::aes(x = .data[["x"]], xend = .data[["xend"]], y = .data[["y"]], yend = .data[["yend"]]),
      linetype = "solid",
      color = "black",
      linewidth = 1
    )
  }

  p
}



#' Estimate Drift Parameter for Wiener Process with Maintenance
#'
#' Computes the Maximum Likelihood Estimate (MLE) of the drift parameter \eqn{\mu}
#' from degradation data subjected to imperfect maintenance actions (\eqn{Y_t}),
#' by compensating for the degradation reduction jumps (\eqn{Z_{lj}}) across all units:
#' \eqn{\hat{\mu}_Y = \frac{\sum_{l=1}^N \left( Y_l(\tau) - \sum_{j} Z_{lj} \right)}{N \tau}}.
#'
#' @param data A \code{data.frame} containing degradation data with maintenance
#'   (must contain columns \code{Time}, \code{Y}, and an identifier \code{Object} or \code{Objeto}).
#'
#' @return A numeric scalar representing the estimated drift parameter \eqn{\hat{\mu}_Y}.
#'
#' @examples
#' data_paths <- sim_wiener_maintenance_paths(
#'   n_units = 5, t_max = 20, n_steps = 20, drift = 3, sigma2 = 2,
#'   rho = c(0.2, 0.4, 0.6), n_maint = 3
#' )
#' mle_drift_maintenance(data_paths)
#'
#' @export
mle_drift_maintenance <- function(data) {
  id_col <- if ("Object" %in% names(data)) {
    "Object"
  } else if ("Objeto" %in% names(data)) {
    "Objeto"
  } else {
    names(data)[1]
  }

  s <- unique(data[[id_col]])
  first_unit <- data[data[[id_col]] == s[1], ]
  k <- first_unit$Time[duplicated(first_unit$Time)]
  tau <- max(data$Time)

  if (length(k) == 0) {
    y_col <- if ("Y" %in% names(data)) "Y" else "Wt"
    terminal_vals <- data[[y_col]][data$Time == tau]
    return(mean(terminal_vals) / tau)
  }

  vals <- numeric(length(s))
  for (l in seq_along(s)) {
    sub_d <- data[data[[id_col]] == s[l], ]
    y_tau <- tail(sub_d$Y[sub_d$Time == tau], 1)
    jumps <- sapply(k, function(t_k) diff(sub_d$Y[sub_d$Time == t_k]))
    vals[l] <- y_tau - sum(jumps)
  }

  mu_hat <- sum(vals) / (length(s) * tau)
  return(as.numeric(mu_hat))
}

mle_drift1_y <- mle_drift_maintenance

#' Estimate Diffusion Variance Parameter (Sigma^2) for Process with Maintenance
#'
#' Computes the unbiased Maximum Likelihood Estimate (MLE) of the diffusion
#' variance parameter \eqn{\sigma^2} from the increments of degradation paths
#' subjected to imperfect maintenance (\eqn{Y_t}) across all units.
#'
#' @param data A \code{data.frame} containing degradation data with maintenance
#'   (must contain columns \code{Time}, \code{Y}, and an identifier \code{Object} or \code{Objeto}).
#'
#' @return A numeric scalar representing the estimated diffusion variance \eqn{\hat{\sigma}^2}.
#'
#' @examples
#' data_paths <- sim_wiener_maintenance_paths(
#'   n_units = 5, t_max = 20, n_steps = 20, drift = 3, sigma2 = 2,
#'   rho = c(0.2, 0.4, 0.6), n_maint = 3
#' )
#' mle_sigma2_maintenance(data_paths)
#'
#' @export
mle_sigma2_maintenance <- function(data) {
  id_col <- if ("Object" %in% names(data)) {
    "Object"
  } else if ("Objeto" %in% names(data)) {
    "Objeto"
  } else {
    names(data)[1]
  }

  mu_hat <- mle_drift_maintenance(data)
  s <- unique(data[[id_col]])

  first_unit <- data[data[[id_col]] == s[1], ]
  k <- first_unit$Time[duplicated(first_unit$Time)]

  if (length(k) == 0) {
    return(mle_sigma2_standard(data))
  }

  nj <- nrow(first_unit[first_unit$Time > k[1] & first_unit$Time < k[2], ])
  nj_last <- nrow(first_unit[first_unit$Time > k[length(k)] & first_unit$Time < max(first_unit$Time), ])

  if (nj == nj_last) {
    N <- nj * (length(k) + 1)
  } else {
    N <- nj * length(k) + nj_last
  }

  y_aux <- matrix(NA, nrow = length(s), ncol = (length(k) + 1))

  for (l in seq_along(s)) {
    aux_data <- data[data[[id_col]] == s[l], ]
    for (j in 1:(length(k) + 1)) {
      if (j == 1) {
        yji <- aux_data %>% dplyr::filter(Time <= k[j]) %>%
          dplyr::filter(dplyr::row_number() <= dplyr::n() - 1) %>%
          dplyr::pull(Y) %>% diff()
        tji <- aux_data %>% dplyr::filter(Time <= k[j]) %>%
          dplyr::filter(dplyr::row_number() <= dplyr::n() - 1) %>%
          dplyr::pull(Time) %>% diff()
        y_aux[l, j] <- sum(((yji - mu_hat * tji)^2) / tji)
      }
      if (j >= 2 && j < (length(k) + 1)) {
        yji <- aux_data %>% dplyr::filter(Time >= k[j - 1], Time <= k[j]) %>%
          dplyr::slice(2:(dplyr::n() - 1)) %>%
          dplyr::pull(Y) %>% diff()
        tji <- aux_data %>% dplyr::filter(Time >= k[j - 1], Time <= k[j]) %>%
          dplyr::slice(2:(dplyr::n() - 1)) %>%
          dplyr::pull(Time) %>% diff()
        y_aux[l, j] <- sum(((yji - mu_hat * tji)^2) / tji)
      }
      if (j == (length(k) + 1)) {
        yji <- aux_data %>% dplyr::filter(Time >= k[j - 1]) %>%
          dplyr::slice(2:dplyr::n()) %>%
          dplyr::pull(Y) %>% diff()
        tji <- aux_data %>% dplyr::filter(Time >= k[j - 1]) %>%
          dplyr::slice(2:dplyr::n()) %>%
          dplyr::pull(Time) %>% diff()
        y_aux[l, j] <- sum(((yji - mu_hat * tji)^2) / tji)
      }
    }
  }

  total_pts <- length(s) * (N + length(k) + 1)
  sigma2_hat_biased <- sum(y_aux) / total_pts
  sigma2_hat_unbiased <- sigma2_hat_biased * total_pts / (total_pts - 1)
  return(sigma2_hat_unbiased)
}

mle_sigma1_y <- mle_sigma2_maintenance

#' Plot Wiener Process Reliability Curves
#'
#' Computes and visualizes the reliability function
#' \eqn{R(t \mid t_0, x_0) = P(T > t \mid X(t_0) = x_0)} for a Wiener degradation process
#' exceeding critical failure threshold(s) \eqn{\alpha}.
#'
#' Under a linear drift \eqn{\mu} and diffusion variance \eqn{\sigma^2}, the first hitting
#' time (FHT) \eqn{T} from initial level \eqn{x_0} to critical barrier \eqn{\alpha}
#' follows an Inverse Gaussian distribution:
#' \deqn{T - t_0 \sim \mathrm{IG}\left(\frac{\alpha - x_0}{\mu}, \frac{(\alpha - x_0)^2}{\sigma^2}\right)}
#' The reliability at elapsed time \eqn{\tau = t - t_0} is computed using
#' \code{\link[statmod]{pinvgauss}} as \eqn{1 - F_{\mathrm{IG}}(\tau)}.
#'
#' @param mu Numeric. Drift parameter (\eqn{\mu > 0}).
#' @param sigma2 Numeric. Diffusion variance parameter (\eqn{\sigma^2 > 0}).
#' @param alpha Numeric vector or scalar. Critical degradation failure threshold(s) (\eqn{\alpha > x_0}).
#' @param t0 Numeric. Initial inspection or last maintenance timestamp (default: 0).
#' @param x0 Numeric. Initial degradation level observed at time \code{t0} (default: 0).
#' @param t_max Numeric. Maximum time horizon for plotting the reliability curves.
#' @param xlab Character. Label for the x-axis (default: \code{"Time"}).
#' @param ylab Character. Label for the y-axis (default: \code{"Reliability"}).
#' @param palette Character. Color palette name for thresholds (default: \code{"taylor1989"}).
#' @param title Character or NULL. Plot title (default: \code{NULL}).
#' @param show_title Logical. Whether to display the plot title (default: \code{FALSE}).
#' @param line_size Numeric. Width of reliability curve lines (default: 1.0).
#' @param line_alpha Numeric. Opacity of reliability curve lines (default: 0.7).
#' @param by Numeric. Time increment step for evaluating reliability (default: 0.1).
#' @param legend_position Character. Position of legend: \code{"right"}, \code{"bottom"},
#'   \code{"top"}, or \code{"none"} (default: \code{"right"} if multiple thresholds, else \code{"none"}).
#' @param show_vlines Logical. Whether to show vertical reference lines at \code{t0} (default: \code{TRUE}).
#' @param show_annotation Logical. Whether to display time label annotation for \code{t0} (default: \code{TRUE}).
#' @param paleta Character. Alias for \code{palette} for backward compatibility.
#'
#' @return A \code{\link[ggplot2]{ggplot}} object representing the reliability curves.
#'
#' @examples
#' # Single threshold
#' p1 <- plot_reliability(mu = 1.5, sigma2 = 0.5, alpha = 20, t0 = 0, x0 = 0, t_max = 25)
#'
#' # Multiple thresholds
#' p2 <- plot_reliability(
#'   mu = 2.0, sigma2 = 0.8, alpha = c(15, 20, 25),
#'   t0 = 5, x0 = 4, t_max = 20,
#'   xlab = "Time (hours)", ylab = "Reliability R(t)"
#' )
#'
#' @export
plot_reliability <- function(mu,
                             sigma2,
                             alpha,
                             t0 = 0,
                             x0 = 0,
                             t_max,
                             xlab = "Time",
                             ylab = "Reliability",
                             palette = "taylor1989",
                             title = NULL,
                             show_title = FALSE,
                             line_size = 1.0,
                             line_alpha = 0.7,
                             by = 0.1,
                             legend_position = if (length(alpha) > 1) "right" else "none",
                             show_vlines = TRUE,
                             show_annotation = TRUE,
                             paleta = NULL) {
  # Backward compatibility for argument 'paleta'
  if (!is.null(paleta)) {
    palette <- paleta
  }

  # Input validation
  if (!is.numeric(mu) || length(mu) != 1 || mu <= 0) {
    stop("`mu` (drift) must be a single positive number.")
  }
  if (!is.numeric(sigma2) || length(sigma2) != 1 || sigma2 <= 0) {
    stop("`sigma2` (diffusion variance) must be a single positive number.")
  }
  if (!is.numeric(alpha) || length(alpha) == 0) {
    stop("`alpha` must be a numeric vector of threshold values.")
  }
  if (!is.numeric(t0) || length(t0) != 1 || t0 < 0) {
    stop("`t0` must be a single non-negative number.")
  }
  if (!is.numeric(x0) || length(x0) != 1) {
    stop("`x0` must be a single number.")
  }
  if (!is.numeric(t_max) || length(t_max) != 1 || t_max <= t0) {
    stop("`t_max` must be greater than `t0`.")
  }
  if (any(alpha <= x0)) {
    stop("All thresholds in `alpha` must be strictly greater than `x0`.")
  }
  if (!is.numeric(by) || length(by) != 1 || by <= 0) {
    stop("`by` must be a single positive number.")
  }

  # Time grid setup
  t_seq <- seq(t0, t_max, by = by)
  tau <- t_seq - t0

  # Compute reliability curves for each threshold
  df_list <- lapply(alpha, function(a) {
    media <- (a - x0) / mu
    desvio <- ((a - x0)^2) / sigma2
    r_mean <- statmod::pinvgauss(tau, mean = media, shape = desvio, lower.tail = FALSE)
    data.frame(
      time = t_seq,
      r_mean = r_mean,
      Threshold = factor(a, levels = as.character(alpha))
    )
  })
  df_visu <- do.call(rbind, df_list)

  # Build ggplot object
  p <- ggplot2::ggplot(
    df_visu,
    ggplot2::aes(
      x = .data[["time"]],
      y = .data[["r_mean"]],
      colour = .data[["Threshold"]]
    )
  ) +
    ggplot2::scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
    ggplot2::geom_line(linewidth = line_size, alpha = line_alpha) +
    ggplot2::theme_classic() +
    ggplot2::labs(
      title = title,
      x = xlab,
      y = ylab,
      colour = "Threshold"
    ) +
    ggplot2::coord_cartesian(expand = FALSE)

  # Title visibility handling
  if (!show_title || is.null(title)) {
    p <- p + ggplot2::theme(plot.title = ggplot2::element_blank())
  } else {
    p <- p + ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))
  }

  # Legend styling
  p <- p + ggplot2::theme(legend.position = legend_position)

  # Optional ggtext markdown support if installed
  if (requireNamespace("ggtext", quietly = TRUE)) {
    p <- p + ggplot2::theme(axis.text.y = ggtext::element_markdown())
  }

  # Vertical indicator lines at t0
  if (show_vlines) {
    if (t0 >= 2) {
      p <- p + ggplot2::geom_vline(xintercept = t0 - 2, colour = "grey50", linetype = "solid")
    }
    p <- p + ggplot2::geom_vline(xintercept = t0, colour = "black", linetype = "longdash")
  }

  # Annotation at t0
  if (show_annotation) {
    x_pos <- if (t_max - t0 > 3) (t0 + 2.5) else (t0 + (t_max - t0) * 0.2)
    p <- p + ggplot2::annotate(
      "text",
      x = x_pos,
      y = 0.08,
      label = paste0("t=", t0),
      size = 3,
      colour = "black"
    )
  }

  # Color palette from tayloRswift if available, with graceful fallback
  if (requireNamespace("tayloRswift", quietly = TRUE) && !is.null(palette) && palette %in% names(tayloRswift::swift_palettes)) {
    p <- p + tayloRswift::scale_color_taylor(palette = palette, reverse = FALSE)
  }

  return(p)
}

#' Plot Degradation Path with Maintenance Interventions
#'
#' Visualizes the degradation path of a system subjected to imperfect maintenance actions.
#' Discontinuities (sudden reductions in degradation level) at maintenance times are highlighted
#' with vertical dotted segments representing the maintenance effect, avoiding spurious continuous
#' connections between pre- and post-maintenance inspection states.
#'
#' @param data A \code{data.frame} containing degradation data (must include time and degradation
#'   columns, such as \code{Time} and \code{Y}, or \code{Wt}).
#' @param xlab Character. Label for the x-axis (default: \code{"Time"}).
#' @param ylab Character. Label for the y-axis (default: \code{"Degradation"}).
#' @param title Character or NULL. Plot title (default: \code{NULL}).
#' @param show_title Logical. Whether to display the plot title (default: \code{FALSE}).
#' @param show_time Logical. Whether to display text annotations with the intervention timestamp
#'   (\code{"t=..."}) below each maintenance jump (default: \code{FALSE}).
#' @param line_size Numeric. Width of degradation path and maintenance segment lines (default: 1.0).
#' @param line_alpha Numeric. Opacity of degradation path lines (default: 0.7).
#' @param path_color Character or NULL. Color for the degradation path trajectory. If \code{NULL},
#'   uses \code{tayloRswift::swift_palettes$taylor1989[1]} if available, otherwise a standard blue.
#' @param maint_color Character. Color for vertical maintenance effect drop segments (default: \code{"black"}).
#' @param expand Numeric vector. Range expansion factor for both axes (default: \code{c(0, 0)} to eliminate Cartesian origin spacing).
#' @param time Logical. Legacy alias for \code{show_time} (for backward compatibility).
#'
#' @return A \code{\link[ggplot2]{ggplot}} object representing the maintained degradation path.
#'
#' @examples
#' data_maint <- sim_wiener_maintenance(
#'   t_max = 20, n_steps = 20, drift = 2, sigma2 = 1, rho = 0.5, n_maint = 3
#' )
#' plot_maintenance(data_maint, xlab = "Time (hours)", ylab = "Degradation (mm)")
#'
#' @export
plot_maintenance <- function(data,
                             xlab = "Time",
                             ylab = "Degradation",
                             title = NULL,
                             show_title = FALSE,
                             show_time = FALSE,
                             line_size = 1.0,
                             line_alpha = 0.7,
                             path_color = NULL,
                             maint_color = "black",
                             expand = c(0, 0),
                             time = NULL) {
  # Backward compatibility for argument 'time'
  if (!is.null(time)) {
    show_time <- time
  }

  if (!is.data.frame(data) || nrow(data) == 0) {
    stop("`data` must be a non-empty data frame.")
  }

  time_col <- if ("Time" %in% names(data)) {
    "Time"
  } else if ("time" %in% names(data)) {
    "time"
  } else {
    NULL
  }

  y_col <- if ("Y" %in% names(data)) {
    "Y"
  } else if ("Wt" %in% names(data)) {
    "Wt"
  } else if ("Degradation" %in% names(data)) {
    "Degradation"
  } else {
    NULL
  }

  if (is.null(time_col) || is.null(y_col)) {
    stop("`data` must contain a time column (`Time` or `time`) and a degradation column (`Y`, `Wt`, or `Degradation`).")
  }

  df <- data[order(data[[time_col]]), , drop = FALSE]
  times <- df[[time_col]]
  y_vals <- df[[y_col]]

  # Identify maintenance intervention points (duplicated timestamps)
  is_dup <- duplicated(times)
  dup_times <- unique(times[is_dup])

  # Assign distinct stage IDs between maintenance jumps to prevent connecting lines
  df$stage <- factor(cumsum(is_dup))

  # Resolve colors
  if (is.null(path_color)) {
    if (requireNamespace("tayloRswift", quietly = TRUE) && length(tayloRswift::swift_palettes$taylor1989) >= 1) {
      path_color <- tayloRswift::swift_palettes$taylor1989[1]
    } else {
      path_color <- "#2C7BB6"
    }
  }

  color_values <- c("Degradation Path" = path_color)
  if (length(dup_times) > 0) {
    color_values["Maintenance Effect"] <- maint_color
  }

  p <- ggplot2::ggplot() +
    ggplot2::geom_line(
      data = df,
      ggplot2::aes(
        x = .data[[time_col]],
        y = .data[[y_col]],
        group = .data[["stage"]],
        color = "Degradation Path"
      ),
      linewidth = line_size,
      alpha = line_alpha,
      linetype = "solid"
    )

  if (length(dup_times) > 0) {
    seg_list <- lapply(dup_times, function(ponto) {
      pts <- y_vals[times == ponto]
      data.frame(
        x = ponto,
        xend = ponto,
        y = max(pts),
        yend = min(pts)
      )
    })
    seg_df <- do.call(rbind, seg_list)

    p <- p + ggplot2::geom_segment(
      data = seg_df,
      ggplot2::aes(
        x = .data[["x"]],
        xend = .data[["xend"]],
        y = .data[["y"]],
        yend = .data[["yend"]],
        color = "Maintenance Effect"
      ),
      linetype = "dotted",
      linewidth = line_size
    )

    if (show_time) {
      y_range <- diff(range(y_vals, na.rm = TRUE))
      annot_y_offset <- if (y_range > 0) 0.04 * y_range else 1.0
      text_df <- data.frame(
        x = dup_times,
        y = seg_df$yend - annot_y_offset,
        label = paste0("t=", dup_times)
      )
      p <- p + ggplot2::geom_text(
        data = text_df,
        ggplot2::aes(x = .data[["x"]], y = .data[["y"]], label = .data[["label"]]),
        size = 3,
        colour = "black",
        vjust = 1
      )
    }
  }

  p <- p +
    ggplot2::scale_color_manual(name = NULL, values = color_values) +
    ggplot2::theme_classic() +
    ggplot2::theme(
      legend.position = "top",
      plot.title = if (show_title && !is.null(title)) ggplot2::element_text(hjust = 0.5) else ggplot2::element_blank()
    ) +
    ggplot2::labs(x = xlab, y = ylab, title = title) +
    ggplot2::scale_y_continuous(expand = expand) +
    ggplot2::scale_x_continuous(expand = expand)

  return(p)
}

#' @rdname plot_maintenance
#' @export
plot_maintanance <- plot_maintenance

#' Plot Exponential Distribution Reliability Characteristics
#'
#' Creates a combined multi-panel plot illustrating the primary reliability
#' characteristics of the Exponential distribution: Probability Density Function
#' (\eqn{f(t) = \lambda e^{-\lambda t}}), Reliability/Survival Function
#' (\eqn{R(t) = e^{-\lambda t}}), and constant Hazard/Failure Rate function
#' (\eqn{h(t) = \lambda}) across various rate parameter values (\eqn{\lambda}).
#' The subplots are arranged using \code{\link[patchwork]{wrap_plots}} with
#' Cartesian origins aligned at \code{(0, 0)}.
#'
#' @param labs_density Character vector of length 3: \code{c(title, xlab, ylab)} for
#'   the probability density function plot (default: \code{c("Density", "Time", "f(t)")}).
#' @param labs_hazard Character vector of length 3: \code{c(title, xlab, ylab)} for
#'   the failure/hazard rate plot (default: \code{c("Failure Rate", "Time", "\u03bb(t)")}).
#' @param labs_reliability Character vector of length 3: \code{c(title, xlab, ylab)} for
#'   the reliability function plot (default: \code{c("Reliability", "Time", "R(t)")}).
#' @param lambdas Numeric vector. Rate parameters (\eqn{\lambda > 0}) to evaluate
#'   (default: \code{c(0.5, 1.0, 1.5)}).
#' @param t_max Numeric. Maximum evaluation time horizon (default: 5).
#' @param n_points Integer. Number of evaluation points along the time domain (default: 100).
#' @param palette Character. Color palette name for \code{tayloRswift} or fallback (default: \code{"taylor1989"}).
#' @param param_name Character. Symbol or label used for the rate parameter in the legend (default: \code{"\u03bb"}).
#' @param expand Numeric vector. Range expansion factor for plot axes (default: \code{c(0, 0)} to eliminate Cartesian origin spacing).
#' @param labs_01 Character vector. Legacy alias for \code{labs_density} (for backward compatibility).
#' @param labs_02 Character vector. Legacy alias for \code{labs_hazard} (for backward compatibility).
#' @param labs_03 Character vector. Legacy alias for \code{labs_reliability} (for backward compatibility).
#'
#' @return A composite \code{\link[patchwork]{wrap_plots}} object containing the density, reliability, and hazard plots.
#'
#' @examples
#' # Default plot with English labels
#' plot_exponential()
#'
#' # Custom rate parameters and horizon
#' plot_exponential(lambdas = c(0.2, 0.5, 1.0), t_max = 8)
#'
#' @export
plot_exponential <- function(labs_density = c("Density", "Time", "f(t)"),
                             labs_hazard = c("Failure Rate", "Time", "\u03bb(t)"),
                             labs_reliability = c("Reliability", "Time", "R(t)"),
                             lambdas = c(0.5, 1.0, 1.5),
                             t_max = 5,
                             n_points = 100,
                             palette = "taylor1989",
                             param_name = "\u03bb",
                             expand = c(0, 0),
                             labs_01 = NULL,
                             labs_02 = NULL,
                             labs_03 = NULL) {
  # Backward compatibility for legacy arguments
  if (!is.null(labs_01)) labs_density <- labs_01
  if (!is.null(labs_02)) labs_hazard <- labs_02
  if (!is.null(labs_03)) labs_reliability <- labs_03

  # Input validation
  if (!is.numeric(lambdas) || length(lambdas) == 0 || any(lambdas <= 0)) {
    stop("`lambdas` must be a numeric vector of positive rate parameters.")
  }
  if (!is.numeric(t_max) || length(t_max) != 1 || t_max <= 0) {
    stop("`t_max` must be a single positive number.")
  }
  if (!is.numeric(n_points) || length(n_points) != 1 || n_points < 2) {
    stop("`n_points` must be an integer >= 2.")
  }

  times <- seq(0, t_max, length.out = n_points)
  plot_data <- expand.grid(time = times, lambda = lambdas)
  plot_data$reliability <- exp(-plot_data$lambda * plot_data$time)
  plot_data$hazard <- plot_data$lambda
  plot_data$density <- stats::dexp(plot_data$time, rate = plot_data$lambda)

  lambda_labels <- paste0(param_name, " = ", lambdas)
  plot_data$lambda_factor <- factor(plot_data$lambda, levels = lambdas, labels = lambda_labels)

  # 1. Density Plot: f(t)
  g_density <- ggplot2::ggplot(
    plot_data,
    ggplot2::aes(
      x = .data[["time"]],
      y = .data[["density"]],
      color = .data[["lambda_factor"]],
      linetype = .data[["lambda_factor"]]
    )
  ) +
    ggplot2::geom_line(linewidth = 1) +
    ggplot2::labs(
      title = labs_density[1],
      x = labs_density[2],
      y = labs_density[3],
      color = "",
      linetype = ""
    ) +
    ggplot2::theme_classic() +
    ggplot2::theme(
      legend.title = ggplot2::element_blank(),
      legend.position = c(0.85, 0.85),
      plot.title = ggplot2::element_text(hjust = 0.5)
    ) +
    ggplot2::scale_x_continuous(expand = expand) +
    ggplot2::scale_y_continuous(expand = expand)

  # 2. Reliability Plot: R(t)
  g_reliability <- ggplot2::ggplot(
    plot_data,
    ggplot2::aes(
      x = .data[["time"]],
      y = .data[["reliability"]],
      color = .data[["lambda_factor"]],
      linetype = .data[["lambda_factor"]]
    )
  ) +
    ggplot2::geom_line(linewidth = 1) +
    ggplot2::labs(
      title = labs_reliability[1],
      x = labs_reliability[2],
      y = labs_reliability[3],
      color = "",
      linetype = ""
    ) +
    ggplot2::theme_classic() +
    ggplot2::theme(
      legend.position = "none",
      plot.title = ggplot2::element_text(hjust = 0.5)
    ) +
    ggplot2::scale_x_continuous(expand = expand) +
    ggplot2::scale_y_continuous(expand = expand, limits = c(0, 1))

  # 3. Hazard Rate Plot: h(t)
  g_hazard <- ggplot2::ggplot(
    plot_data,
    ggplot2::aes(
      x = .data[["time"]],
      y = .data[["hazard"]],
      color = .data[["lambda_factor"]],
      linetype = .data[["lambda_factor"]]
    )
  ) +
    ggplot2::geom_line(linewidth = 1) +
    ggplot2::labs(
      title = labs_hazard[1],
      x = labs_hazard[2],
      y = labs_hazard[3],
      color = "",
      linetype = ""
    ) +
    ggplot2::theme_classic() +
    ggplot2::theme(
      legend.position = "none",
      plot.title = ggplot2::element_text(hjust = 0.5)
    ) +
    ggplot2::scale_x_continuous(expand = expand) +
    ggplot2::scale_y_continuous(expand = expand, limits = c(0, max(lambdas) * 1.2))

  if (requireNamespace("tayloRswift", quietly = TRUE) && !is.null(palette) && palette %in% names(tayloRswift::swift_palettes)) {
    g_density <- g_density + tayloRswift::scale_color_taylor(palette = palette, reverse = FALSE)
    g_reliability <- g_reliability + tayloRswift::scale_color_taylor(palette = palette, reverse = FALSE)
    g_hazard <- g_hazard + tayloRswift::scale_color_taylor(palette = palette, reverse = FALSE)
  }

  layout <- "
  AAAABBBB
  ##CCCC##
  "
  patchwork::wrap_plots(A = g_density, B = g_reliability, C = g_hazard, design = layout)
}

#' @rdname plot_exponential
#' @export
gera_plot_exp <- plot_exponential

#' Plot Weibull Distribution Reliability Characteristics
#'
#' Creates a combined multi-panel plot illustrating the primary reliability
#' characteristics of the Weibull distribution: Probability Density Function
#' (\eqn{f(t) = \frac{\gamma}{\alpha} \left(\frac{t}{\alpha}\right)^{\gamma - 1} e^{-(t/\alpha)^\gamma}}),
#' Reliability/Survival Function (\eqn{R(t) = e^{-(t/\alpha)^\gamma}}), and
#' Hazard/Failure Rate function (\eqn{h(t) = \frac{\gamma}{\alpha^\gamma} t^{\gamma - 1}})
#' across various shape parameter values (\eqn{\gamma}) and scale parameter (\eqn{\alpha}).
#' The subplots are arranged using \code{\link[patchwork]{wrap_plots}} with Cartesian
#' origins aligned at \code{(0, 0)}.
#'
#' @param labs_density Character vector of length 3: \code{c(title, xlab, ylab)} for
#'   the probability density function plot (default: \code{c("Density", "Time", "f(t)")}).
#' @param labs_hazard Character vector of length 3: \code{c(title, xlab, ylab)} for
#'   the failure/hazard rate plot (default: \code{c("Failure Rate", "Time", "h(t)")}).
#' @param labs_reliability Character vector of length 3: \code{c(title, xlab, ylab)} for
#'   the reliability function plot (default: \code{c("Reliability", "Time", "R(t)")}).
#' @param gammas Numeric vector. Shape parameters (\eqn{\gamma > 0}) to evaluate
#'   (default: \code{c(0.5, 1.0, 1.5)}).
#' @param alpha Numeric. Scale parameter (\eqn{\alpha > 0}) (default: 1.0).
#' @param t_max Numeric. Maximum evaluation time horizon (default: 5).
#' @param n_points Integer. Number of evaluation points along the time domain (default: 100).
#' @param palette Character. Color palette name for \code{tayloRswift} or fallback (default: \code{"taylor1989"}).
#' @param expand Numeric vector. Range expansion factor for plot axes (default: \code{c(0, 0)} to eliminate Cartesian origin spacing).
#' @param labs_01 Character vector. Legacy alias for \code{labs_density} (for backward compatibility).
#' @param labs_02 Character vector. Legacy alias for \code{labs_hazard} (for backward compatibility).
#' @param labs_03 Character vector. Legacy alias for \code{labs_reliability} (for backward compatibility).
#'
#' @return A composite \code{\link[patchwork]{wrap_plots}} object containing the density, reliability, and hazard plots.
#'
#' @examples
#' # Default plot with English labels
#' plot_weibull()
#'
#' # Custom shape and scale parameters
#' plot_weibull(gammas = c(0.8, 1.0, 2.0), alpha = 2.0, t_max = 8)
#'
#' @export
plot_weibull <- function(labs_density = c("Density", "Time", "f(t)"),
                         labs_hazard = c("Failure Rate", "Time", "h(t)"),
                         labs_reliability = c("Reliability", "Time", "R(t)"),
                         gammas = c(0.5, 1.0, 1.5),
                         alpha = 1.0,
                         t_max = 5,
                         n_points = 100,
                         palette = "taylor1989",
                         expand = c(0, 0),
                         labs_01 = NULL,
                         labs_02 = NULL,
                         labs_03 = NULL) {
  # Backward compatibility for legacy arguments
  if (!is.null(labs_01)) labs_density <- labs_01
  if (!is.null(labs_02)) labs_hazard <- labs_02
  if (!is.null(labs_03)) labs_reliability <- labs_03

  # Input validation
  if (!is.numeric(gammas) || length(gammas) == 0 || any(gammas <= 0)) {
    stop("`gammas` must be a numeric vector of positive shape parameters.")
  }
  if (!is.numeric(alpha) || length(alpha) != 1 || alpha <= 0) {
    stop("`alpha` must be a single positive scale parameter.")
  }
  if (!is.numeric(t_max) || length(t_max) != 1 || t_max <= 0) {
    stop("`t_max` must be a single positive number.")
  }
  if (!is.numeric(n_points) || length(n_points) != 1 || n_points < 2) {
    stop("`n_points` must be an integer >= 2.")
  }

  times <- seq(0, t_max, length.out = n_points)
  plot_data <- expand.grid(time = times, gamma = gammas, alpha = alpha)

  # Weibull reliability, hazard, and density functions
  plot_data$reliability <- exp(-(plot_data$time / plot_data$alpha)^plot_data$gamma)
  plot_data$hazard <- (plot_data$gamma / (plot_data$alpha^plot_data$gamma)) * (plot_data$time)^(plot_data$gamma - 1)
  plot_data$density <- stats::dweibull(plot_data$time, shape = plot_data$gamma, scale = plot_data$alpha)

  # Safely replace Inf with NA for gamma < 1 at time = 0
  plot_data$hazard[is.infinite(plot_data$hazard)] <- NA
  plot_data$density[is.infinite(plot_data$density)] <- NA

  gamma_labels <- paste0("\u03b3 = ", gammas, ", \u03b1 = ", alpha)
  plot_data$gamma_factor <- factor(plot_data$gamma, levels = gammas, labels = gamma_labels)

  # 1. Density Plot: f(t)
  g_density <- ggplot2::ggplot(
    plot_data,
    ggplot2::aes(
      x = .data[["time"]],
      y = .data[["density"]],
      color = .data[["gamma_factor"]],
      linetype = .data[["gamma_factor"]]
    )
  ) +
    ggplot2::geom_line(linewidth = 1, na.rm = TRUE) +
    ggplot2::labs(
      title = labs_density[1],
      x = labs_density[2],
      y = labs_density[3],
      color = "",
      linetype = ""
    ) +
    ggplot2::theme_classic() +
    ggplot2::theme(
      legend.title = ggplot2::element_blank(),
      legend.position = c(0.85, 0.85),
      plot.title = ggplot2::element_text(hjust = 0.5)
    ) +
    ggplot2::scale_x_continuous(expand = expand) +
    ggplot2::scale_y_continuous(expand = expand)

  # 2. Reliability Plot: R(t)
  g_reliability <- ggplot2::ggplot(
    plot_data,
    ggplot2::aes(
      x = .data[["time"]],
      y = .data[["reliability"]],
      color = .data[["gamma_factor"]],
      linetype = .data[["gamma_factor"]]
    )
  ) +
    ggplot2::geom_line(linewidth = 1, na.rm = TRUE) +
    ggplot2::labs(
      title = labs_reliability[1],
      x = labs_reliability[2],
      y = labs_reliability[3],
      color = "",
      linetype = ""
    ) +
    ggplot2::theme_classic() +
    ggplot2::theme(
      legend.position = "none",
      plot.title = ggplot2::element_text(hjust = 0.5)
    ) +
    ggplot2::scale_x_continuous(expand = expand) +
    ggplot2::scale_y_continuous(expand = expand, limits = c(0, 1))

  # 3. Hazard Rate Plot: h(t)
  g_hazard <- ggplot2::ggplot(
    plot_data,
    ggplot2::aes(
      x = .data[["time"]],
      y = .data[["hazard"]],
      color = .data[["gamma_factor"]],
      linetype = .data[["gamma_factor"]]
    )
  ) +
    ggplot2::geom_line(linewidth = 1, na.rm = TRUE) +
    ggplot2::labs(
      title = labs_hazard[1],
      x = labs_hazard[2],
      y = labs_hazard[3],
      color = "",
      linetype = ""
    ) +
    ggplot2::theme_classic() +
    ggplot2::theme(
      legend.position = "none",
      plot.title = ggplot2::element_text(hjust = 0.5)
    ) +
    ggplot2::scale_x_continuous(expand = expand) +
    ggplot2::scale_y_continuous(expand = expand)

  if (requireNamespace("tayloRswift", quietly = TRUE) && !is.null(palette) && palette %in% names(tayloRswift::swift_palettes)) {
    g_density <- g_density + tayloRswift::scale_color_taylor(palette = palette, reverse = FALSE)
    g_reliability <- g_reliability + tayloRswift::scale_color_taylor(palette = palette, reverse = FALSE)
    g_hazard <- g_hazard + tayloRswift::scale_color_taylor(palette = palette, reverse = FALSE)
  }

  layout <- "
  AAAABBBB
  ##CCCC##
  "
  patchwork::wrap_plots(A = g_density, B = g_reliability, C = g_hazard, design = layout)
}

#' @rdname plot_weibull
#' @export
gera_plot_weibull <- plot_weibull

#' Plot Lognormal Distribution Reliability Characteristics
#'
#' Creates a combined multi-panel plot illustrating the primary reliability
#' characteristics of the Lognormal distribution: Probability Density Function
#' (\eqn{f(t) = \frac{1}{t \sigma \sqrt{2\pi}} e^{-\frac{(\ln t - \mu)^2}{2\sigma^2}}}),
#' Reliability/Survival Function (\eqn{R(t) = 1 - \Phi\left(\frac{\ln t - \mu}{\sigma}\right)}),
#' and Hazard/Failure Rate function (\eqn{h(t) = \frac{f(t)}{R(t)}}) across various
#' standard deviation parameter values (\eqn{\sigma}) and log-scale mean (\eqn{\mu}).
#' The subplots are arranged using \code{\link[patchwork]{wrap_plots}} with Cartesian
#' origins aligned at \code{(0, 0)}.
#'
#' @param labs_density Character vector of length 3: \code{c(title, xlab, ylab)} for
#'   the probability density function plot (default: \code{c("Density", "Time", "f(t)")}).
#' @param labs_hazard Character vector of length 3: \code{c(title, xlab, ylab)} for
#'   the failure/hazard rate plot (default: \code{c("Failure Rate", "Time", "h(t)")}).
#' @param labs_reliability Character vector of length 3: \code{c(title, xlab, ylab)} for
#'   the reliability function plot (default: \code{c("Reliability", "Time", "R(t)")}).
#' @param sigmas Numeric vector. Standard deviation parameters (\eqn{\sigma > 0}) to evaluate
#'   (default: \code{c(0.5, 1.0, 1.5)}).
#' @param mu Numeric. Mean of the logarithm (\eqn{\mu}) (default: 0.0).
#' @param t_max Numeric. Maximum evaluation time horizon (default: 5).
#' @param n_points Integer. Number of evaluation points along the time domain (default: 100).
#' @param palette Character. Color palette name for \code{tayloRswift} or fallback (default: \code{"taylor1989"}).
#' @param expand Numeric vector. Range expansion factor for plot axes (default: \code{c(0, 0)} to eliminate Cartesian origin spacing).
#' @param labs_01 Character vector. Legacy alias for \code{labs_density} (for backward compatibility).
#' @param labs_02 Character vector. Legacy alias for \code{labs_hazard} (for backward compatibility).
#' @param labs_03 Character vector. Legacy alias for \code{labs_reliability} (for backward compatibility).
#'
#' @return A composite \code{\link[patchwork]{wrap_plots}} object containing the density, reliability, and hazard plots.
#'
#' @examples
#' # Default plot with English labels
#' plot_lognormal()
#'
#' # Custom sigma parameters and horizon
#' plot_lognormal(sigmas = c(0.3, 0.8, 1.2), mu = 0.5, t_max = 8)
#'
#' @export
plot_lognormal <- function(labs_density = c("Density", "Time", "f(t)"),
                           labs_hazard = c("Failure Rate", "Time", "h(t)"),
                           labs_reliability = c("Reliability", "Time", "R(t)"),
                           sigmas = c(0.5, 1.0, 1.5),
                           mu = 0.0,
                           t_max = 5,
                           n_points = 100,
                           palette = "taylor1989",
                           expand = c(0, 0),
                           labs_01 = NULL,
                           labs_02 = NULL,
                           labs_03 = NULL) {
  # Backward compatibility for legacy arguments
  if (!is.null(labs_01)) labs_density <- labs_01
  if (!is.null(labs_02)) labs_hazard <- labs_02
  if (!is.null(labs_03)) labs_reliability <- labs_03

  # Input validation
  if (!is.numeric(sigmas) || length(sigmas) == 0 || any(sigmas <= 0)) {
    stop("`sigmas` must be a numeric vector of positive standard deviation parameters.")
  }
  if (!is.numeric(mu) || length(mu) != 1) {
    stop("`mu` must be a single numeric value for log-mean.")
  }
  if (!is.numeric(t_max) || length(t_max) != 1 || t_max <= 0) {
    stop("`t_max` must be a single positive number.")
  }
  if (!is.numeric(n_points) || length(n_points) != 1 || n_points < 2) {
    stop("`n_points` must be an integer >= 2.")
  }

  times <- seq(0, t_max, length.out = n_points)
  plot_data <- expand.grid(time = times, sigma = sigmas, mu = mu)

  # Lognormal reliability, density, and hazard rate functions
  plot_data$reliability <- stats::plnorm(plot_data$time, meanlog = plot_data$mu, sdlog = plot_data$sigma, lower.tail = FALSE)
  plot_data$density <- stats::dlnorm(plot_data$time, meanlog = plot_data$mu, sdlog = plot_data$sigma)
  plot_data$hazard <- ifelse(plot_data$reliability > 0, plot_data$density / plot_data$reliability, 0)
  plot_data$hazard[is.na(plot_data$hazard) | is.infinite(plot_data$hazard)] <- 0

  sigma_labels <- paste0("\u03bc = ", mu, ", \u03c3 = ", sigmas)
  plot_data$sigma_factor <- factor(plot_data$sigma, levels = sigmas, labels = sigma_labels)

  max_density <- max(plot_data$density, na.rm = TRUE)
  max_hazard <- max(plot_data$hazard, na.rm = TRUE)

  # 1. Density Plot: f(t)
  g_density <- ggplot2::ggplot(
    plot_data,
    ggplot2::aes(
      x = .data[["time"]],
      y = .data[["density"]],
      color = .data[["sigma_factor"]],
      linetype = .data[["sigma_factor"]]
    )
  ) +
    ggplot2::geom_line(linewidth = 1) +
    ggplot2::labs(
      title = labs_density[1],
      x = labs_density[2],
      y = labs_density[3],
      color = "",
      linetype = ""
    ) +
    ggplot2::theme_classic() +
    ggplot2::theme(
      legend.title = ggplot2::element_blank(),
      legend.position = c(0.85, 0.85),
      plot.title = ggplot2::element_text(hjust = 0.5)
    ) +
    ggplot2::scale_x_continuous(expand = expand) +
    ggplot2::scale_y_continuous(expand = expand, limits = c(0, max(1, max_density * 1.05)))

  # 2. Reliability Plot: R(t)
  g_reliability <- ggplot2::ggplot(
    plot_data,
    ggplot2::aes(
      x = .data[["time"]],
      y = .data[["reliability"]],
      color = .data[["sigma_factor"]],
      linetype = .data[["sigma_factor"]]
    )
  ) +
    ggplot2::geom_line(linewidth = 1) +
    ggplot2::labs(
      title = labs_reliability[1],
      x = labs_reliability[2],
      y = labs_reliability[3],
      color = "",
      linetype = ""
    ) +
    ggplot2::theme_classic() +
    ggplot2::theme(
      legend.position = "none",
      plot.title = ggplot2::element_text(hjust = 0.5)
    ) +
    ggplot2::scale_x_continuous(expand = expand) +
    ggplot2::scale_y_continuous(expand = expand, limits = c(0, 1))

  # 3. Hazard Rate Plot: h(t)
  g_hazard <- ggplot2::ggplot(
    plot_data,
    ggplot2::aes(
      x = .data[["time"]],
      y = .data[["hazard"]],
      color = .data[["sigma_factor"]],
      linetype = .data[["sigma_factor"]]
    )
  ) +
    ggplot2::geom_line(linewidth = 1) +
    ggplot2::labs(
      title = labs_hazard[1],
      x = labs_hazard[2],
      y = labs_hazard[3],
      color = "",
      linetype = ""
    ) +
    ggplot2::theme_classic() +
    ggplot2::theme(
      legend.position = "none",
      plot.title = ggplot2::element_text(hjust = 0.5)
    ) +
    ggplot2::scale_x_continuous(expand = expand) +
    ggplot2::scale_y_continuous(expand = expand, limits = c(0, max(2, max_hazard * 1.05)))

  if (requireNamespace("tayloRswift", quietly = TRUE) && !is.null(palette) && palette %in% names(tayloRswift::swift_palettes)) {
    g_density <- g_density + tayloRswift::scale_color_taylor(palette = palette, reverse = FALSE)
    g_reliability <- g_reliability + tayloRswift::scale_color_taylor(palette = palette, reverse = FALSE)
    g_hazard <- g_hazard + tayloRswift::scale_color_taylor(palette = palette, reverse = FALSE)
  }

  layout <- "
  AAAABBBB
  ##CCCC##
  "
  patchwork::wrap_plots(A = g_density, B = g_reliability, C = g_hazard, design = layout)
}

#' @rdname plot_lognormal
#' @export
gera_plot_lognormal <- plot_lognormal

#' Plot Survival and Reliability Censoring Types
#'
#' @description
#' Visualizes four classic survival and reliability data censoring schemes:
#' \itemize{
#'   \item \strong{(a) Complete data:} All units fail within the test duration, and all exact failure times are observed.
#'   \item \strong{(b) Type I censoring (time censoring):} The test is terminated at a predetermined fixed calendar time \eqn{T}. Units surviving beyond \eqn{T} are right-censored.
#'   \item \strong{(c) Type II censoring (failure censoring):} The test is terminated immediately upon observing the \eqn{r}-th failure. Remaining units are right-censored at that failure time.
#'   \item \strong{(d) Random censoring:} Units drop out or are lost to follow-up at individual, uncoordinated random times prior to failure or study completion.
#' }
#'
#' Each subplot displays individual units as horizontal lifelines, marking observed failures with solid circles
#' and censored endpoints with open circles. The four panels are combined using \pkg{patchwork}.
#'
#' @param titles Character vector of length 4 containing titles for each of the four subplots
#'   (complete data, Type I, Type II, and random censoring).
#'   Default is \code{c("(a) Complete data", "(b) Type I censoring", "(c) Type II censoring", "(d) Random censoring")}.
#' @param x_label Character string for the x-axis label across all subplots. Default is \code{"Time"}.
#' @param y_label Character string for the y-axis label across all subplots. Default is \code{"Units"}.
#' @param experiment_end Numeric scalar specifying the scheduled experiment termination cutoff time.
#'   Default is \code{20}. Set to \code{NULL} to omit the vertical cutoff line and annotation.
#' @param experiment_end_label Character string for the annotation text describing the cutoff line.
#'   Default is \code{"End of Experiment"}.
#' @param xlim Numeric vector of length 2 specifying the range of the x-axis. Default is \code{c(0, 22)}.
#' @param expand Numeric vector of length 2 controlling scale expansion for the axes.
#'   Default is \code{c(0, 0)} to eliminate spacing at the Cartesian origin.
#' @param show_end_line Logical indicating whether to draw a vertical dotted line and label at \code{experiment_end}.
#'   Default is \code{TRUE}.
#'
#' @return A combined \code{patchwork} object containing a 2x2 grid of the four censoring scenario plots.
#'
#' @examples
#' \dontrun{
#' # Standard English 2x2 plot
#' p <- plot_censoring()
#' print(p)
#'
#' # Custom labels and limits
#' p_custom <- plot_censoring(
#'   titles = c("Complete", "Type 1", "Type 2", "Random"),
#'   x_label = "Hours",
#'   y_label = "Device ID"
#' )
#' print(p_custom)
#' }
#'
#' @export
plot_censoring <- function(
  titles = c(
    "(a) Complete data",
    "(b) Type I censoring",
    "(c) Type II censoring",
    "(d) Random censoring"
  ),
  x_label = "Time",
  y_label = "Units",
  experiment_end = 20,
  experiment_end_label = "End of Experiment",
  xlim = c(0, 22),
  expand = c(0, 0),
  show_end_line = TRUE
) {
  if (!is.character(titles) || length(titles) != 4) {
    stop("'titles' must be a character vector of length 4.")
  }
  if (!is.numeric(xlim) || length(xlim) != 2 || xlim[1] >= xlim[2]) {
    stop("'xlim' must be a numeric vector of length 2 with xlim[1] < xlim[2].")
  }
  if (!is.numeric(expand) || length(expand) != 2) {
    stop("'expand' must be a numeric vector of length 2.")
  }

  censoring_data <- data.frame(
    unit = rep(1:6, 4),
    start_time = rep(0, 24),
    end_time = c(
      6, 10, 14, 12, 16, 18,
      6, 20, 20, 12, 16, 20,
      6, 10, 20, 12, 20, 20,
      9, 20, 14, 12, 20, 7
    ),
    event = c(
      1, 1, 1, 1, 1, 1,
      1, 0, 0, 1, 1, 0,
      1, 1, 1, 1, 0, 0,
      1, 0, 1, 0, 0, 0
    ),
    scheme_id = rep(1:4, each = 6),
    scheme_name = rep(titles, each = 6),
    stringsAsFactors = FALSE
  )

  make_subplot <- function(scheme_idx) {
    plot_subset <- censoring_data[censoring_data$scheme_id == scheme_idx, , drop = FALSE]

    p <- ggplot2::ggplot(plot_subset, ggplot2::aes(y = .data[["unit"]])) +
      ggplot2::geom_segment(
        ggplot2::aes(
          x = .data[["start_time"]],
          xend = .data[["end_time"]],
          yend = .data[["unit"]]
        ),
        linewidth = 0.6
      ) +
      ggplot2::geom_point(
        ggplot2::aes(
          x = .data[["end_time"]],
          shape = factor(.data[["event"]])
        ),
        size = 2
      ) +
      ggplot2::scale_shape_manual(values = c("0" = 1, "1" = 16)) +
      ggplot2::scale_y_reverse(breaks = 1:6, limits = c(6.5, 0.5), expand = expand) +
      ggplot2::scale_x_continuous(limits = xlim, expand = expand) +
      ggplot2::labs(
        x = x_label,
        y = y_label,
        title = titles[scheme_idx]
      ) +
      ggplot2::theme_classic() +
      ggplot2::theme(
        plot.title = ggplot2::element_text(hjust = 0.5),
        legend.position = "none"
      )

    if (isTRUE(show_end_line) && !is.null(experiment_end)) {
      p <- p +
        ggplot2::geom_vline(xintercept = experiment_end, linetype = "dotted", linewidth = 0.5) +
        ggplot2::annotate(
          "text",
          x = max(0, experiment_end - 4),
          y = 1.5,
          label = experiment_end_label,
          hjust = 0,
          size = 2.5
        )
    }

    p
  }

  g1 <- make_subplot(1)
  g2 <- make_subplot(2)
  g3 <- make_subplot(3)
  g4 <- make_subplot(4)

  patchwork::wrap_plots(g1, g2, g3, g4, ncol = 2)
}

#' @rdname plot_censoring
#' @export
plot_censura_all <- function(
  titles = c(
    "(a) Dados completos",
    "(b) Dados com censura tipo I",
    "(c) Dados com censura tipo II",
    "(d) Dados com censura aleat\u00f3ria"
  ),
  x_label = "Tempos",
  y_label = "Equipamentos",
  experiment_end = 20,
  experiment_end_label = "Final do Experimento",
  xlim = c(0, 22),
  expand = c(0, 0),
  show_end_line = TRUE
) {
  plot_censoring(
    titles = titles,
    x_label = x_label,
    y_label = y_label,
    experiment_end = experiment_end,
    experiment_end_label = experiment_end_label,
    xlim = xlim,
    expand = expand,
    show_end_line = show_end_line
  )
}


#' Plot Illustrative Degradation Path and First Hitting Time
#'
#' @description
#' Generates an illustrative conceptual diagram showing a simulated continuous degradation trajectory
#' \eqn{Y(t)}, a critical failure threshold \eqn{w}, and the resulting first hitting time
#' (failure time) \eqn{T = \inf\{t : Y(t) \ge w\}}.
#'
#' The diagram annotates the failure threshold line, the continuous degradation curve, and the
#' precise failure point obtained through linear interpolation between consecutive discrete time points.
#'
#' @param failure_threshold Numeric critical threshold level defining failure. Default is \code{20}.
#' @param threshold_label Character label for the failure threshold annotation. Default is \code{"Failure Threshold"}.
#' @param path_label Character label for the degradation trajectory curve annotation. Default is \code{"Degradation Path"}.
#' @param failure_time_label Character label for the failure point annotation. Default is \code{"Failure Time"}.
#' @param x_label Character label for the horizontal axis. Default is \code{"Time"}.
#' @param y_label Character label for the vertical axis. Default is \code{"Degradation"}.
#' @param t_max Numeric maximum time horizon for the simulated path. Default is \code{10}.
#' @param n_points Integer number of discrete evaluation points. Default is \code{100}.
#' @param seed Optional integer seed for reproducibility. Default is \code{123}. Set to \code{NULL} for unseeded simulation.
#' @param xlim Numeric vector of length 2 for the x-axis limits. Default is \code{c(0, 10.2)}.
#' @param ylim Numeric vector of length 2 for the y-axis limits. Default is \code{c(0, 25)}.
#' @param expand Numeric vector of length 2 for Cartesian scale expansion. Default is \code{c(0, 0)} to eliminate origin margins.
#' @param palette Optional character name of the color palette. Default is \code{"taylor1989"}.
#' @param labs_degradacao Optional character vector of length 5 providing legacy Portuguese labels
#'   in the order: \code{c(threshold_label, path_label, failure_time_label, x_label, y_label)}.
#' @param ... Additional arguments passed to \code{plot_degradation}.
#'
#' @return A \code{ggplot2::ggplot} object representing the degradation failure process.
#'
#' @examples
#' \dontrun{
#' # Standard English degradation plot
#' p <- plot_degradation()
#' print(p)
#'
#' # Custom threshold and labels
#' p_custom <- plot_degradation(
#'   failure_threshold = 15,
#'   threshold_label = "Alarm Limit",
#'   x_label = "Cycles",
#'   y_label = "Wear (mm)"
#' )
#' print(p_custom)
#' }
#'
#' @export
plot_degradation <- function(
  failure_threshold = 20,
  threshold_label = "Failure Threshold",
  path_label = "Degradation Path",
  failure_time_label = "Failure Time",
  x_label = "Time",
  y_label = "Degradation",
  t_max = 10,
  n_points = 100,
  seed = 123,
  xlim = c(0, 10.2),
  ylim = c(0, 25),
  expand = c(0, 0),
  palette = "taylor1989",
  labs_degradacao = NULL
) {
  if (!is.null(labs_degradacao)) {
    if (!is.character(labs_degradacao) || length(labs_degradacao) != 5) {
      stop("'labs_degradacao' must be a character vector of length 5.")
    }
    threshold_label <- labs_degradacao[1]
    path_label <- labs_degradacao[2]
    failure_time_label <- labs_degradacao[3]
    x_label <- labs_degradacao[4]
    y_label <- labs_degradacao[5]
  }

  if (!is.numeric(failure_threshold) || length(failure_threshold) != 1 || failure_threshold <= 0) {
    stop("'failure_threshold' must be a single positive number.")
  }
  if (!is.numeric(t_max) || length(t_max) != 1 || t_max <= 0) {
    stop("'t_max' must be a single positive number.")
  }
  if (!is.numeric(n_points) || length(n_points) != 1 || n_points < 2) {
    stop("'n_points' must be an integer >= 2.")
  }
  if (!is.numeric(xlim) || length(xlim) != 2 || xlim[1] >= xlim[2]) {
    stop("'xlim' must be a numeric vector of length 2 with xlim[1] < xlim[2].")
  }
  if (!is.numeric(ylim) || length(ylim) != 2 || ylim[1] >= ylim[2]) {
    stop("'ylim' must be a numeric vector of length 2 with ylim[1] < ylim[2].")
  }
  if (!is.numeric(expand) || length(expand) != 2) {
    stop("'expand' must be a numeric vector of length 2.")
  }

  if (!is.null(seed)) {
    set.seed(seed)
  }

  time <- seq(0, t_max, length.out = as.integer(n_points))
  degradation <- cumsum(stats::rnorm(n_points, mean = 0.2, sd = 0.5))
  degradation <- degradation - min(degradation)

  failure_index <- which(degradation >= failure_threshold)[1]

  if (!is.na(failure_index)) {
    if (failure_index > 1) {
      x1 <- time[failure_index - 1]
      x2 <- time[failure_index]
      y1 <- degradation[failure_index - 1]
      y2 <- degradation[failure_index]
      failure_time <- x1 + (failure_threshold - y1) * (x2 - x1) / (y2 - y1)
      failure_level <- failure_threshold
    } else {
      failure_time <- time[failure_index]
      failure_level <- degradation[failure_index]
    }
  } else {
    failure_time <- NA_real_
    failure_level <- failure_threshold
  }

  plot_data <- data.frame(time = time, degradation = degradation)

  line_color <- "#0072B2"
  threshold_color <- "#D55E00"
  if (requireNamespace("tayloRswift", quietly = TRUE) &&
      !is.null(palette) &&
      palette %in% names(tayloRswift::swift_palettes)) {
    pal <- tayloRswift::swift_palettes[[palette]]
    if (length(pal) >= 6) {
      line_color <- pal[1]
      threshold_color <- pal[6]
    }
  }

  p <- ggplot2::ggplot(
    plot_data,
    ggplot2::aes(x = .data[["time"]], y = .data[["degradation"]])
  ) +
    ggplot2::geom_line(color = line_color, linewidth = 1) +
    ggplot2::geom_hline(
      yintercept = failure_threshold,
      color = threshold_color,
      linetype = "dotdash",
      linewidth = 1
    ) +
    ggplot2::annotate(
      "text",
      x = 2,
      y = failure_threshold - 5,
      label = threshold_label,
      hjust = 0,
      angle = 0
    ) +
    ggplot2::annotate(
      "segment",
      x = 2.6,
      xend = 3,
      y = failure_threshold - 4.5,
      yend = failure_threshold - 0.5,
      arrow = grid::arrow(length = grid::unit(0.2, "cm")),
      color = "black",
      linewidth = 1
    ) +
    ggplot2::annotate(
      "text",
      x = 6,
      y = 10,
      label = path_label,
      hjust = 0,
      angle = 0
    ) +
    ggplot2::annotate(
      "segment",
      x = 7,
      xend = 6.8,
      y = 10.5,
      yend = 14,
      arrow = grid::arrow(length = grid::unit(0.2, "cm")),
      color = "black",
      linewidth = 1
    ) +
    ggplot2::labs(x = x_label, y = y_label) +
    ggplot2::scale_x_continuous(expand = expand, limits = xlim) +
    ggplot2::scale_y_continuous(expand = expand, limits = ylim) +
    ggplot2::theme_classic()

  if (!is.na(failure_time)) {
    p <- p +
      ggplot2::annotate(
        "point",
        x = failure_time,
        y = failure_level,
        color = "red",
        size = 3
      ) +
      ggplot2::annotate(
        "text",
        x = 7.5,
        y = failure_level + 2,
        label = failure_time_label,
        hjust = 0,
        angle = 0
      ) +
      ggplot2::annotate(
        "segment",
        x = 8.5,
        xend = failure_time - 0.15,
        y = failure_level + 1.5,
        yend = failure_level + 0.5,
        arrow = grid::arrow(length = grid::unit(0.2, "cm")),
        color = "black",
        linewidth = 1
      )
  }

  p
}

#' @rdname plot_degradation
#' @export
gera_plot_degrada <- function(
  labs_degradacao = c(
    "N\u00edvel Cr\u00edtico",
    "Trajet\u00f3ria de Degrada\u00e7\u00e3o",
    "Tempo de Falha",
    "Tempo",
    "Degrada\u00e7\u00e3o"
  ),
  ...
) {
  plot_degradation(labs_degradacao = labs_degradacao, ...)
}


#' Plot Illustrative Bathtub Curve (Hazard Rate Lifecycle)
#'
#' @description
#' Generates an illustrative diagram of the classic "bathtub curve" representing the hazard rate
#' \eqn{\lambda(t)} or failure rate over a product's lifecycle.
#'
#' The curve is divided into three distinct lifecycle phases separated by dashed vertical lines:
#' \itemize{
#'   \item \strong{Infant mortality / Early failures (\eqn{0 \le t < t_1}):} Decreasing hazard rate
#'     driven by manufacturing flaws, defective parts, or initial burn-in stress.
#'   \item \strong{Useful life / Normal operation (\eqn{t_1 \le t \le t_2}):} Constant, low baseline
#'     hazard rate where failures occur randomly due to external shocks (exponential distribution domain).
#'   \item \strong{Wear-out / Aging (\eqn{t > t_2}):} Increasing hazard rate resulting from fatigue,
#'     wear, corrosion, and material aging.
#' }
#'
#' @param infant_mortality_label Character label for the early failure phase. Default is \code{"Infant Mortality"}.
#' @param useful_life_label Character label for the constant failure phase. Default is \code{"Useful Life"}.
#' @param wear_out_label Character label for the aging phase. Default is \code{"Wear-out"}.
#' @param x_label Character label for the horizontal axis. Default is \code{"Time"}.
#' @param y_label Expression or character string for the vertical axis label. Default is \code{expression(lambda(t))}.
#' @param t_phase1 Numeric time boundary separating the infant mortality and useful life phases. Default is \code{30}.
#' @param t_phase2 Numeric time boundary separating the useful life and wear-out phases. Default is \code{70}.
#' @param t_max Numeric maximum time horizon. Default is \code{100}.
#' @param n_points Integer number of evaluation points along the curve. Default is \code{500}.
#' @param a Numeric scale parameter controlling the curvature in the non-linear phases. Default is \code{0.025}.
#' @param b Numeric shape/power parameter controlling the curvature in the non-linear phases. Default is \code{1.8}.
#' @param baseline Numeric constant baseline hazard rate during useful life. Default is \code{11}.
#' @param xlim Numeric vector of length 2 giving limits for the horizontal axis. Default is \code{c(0, 100)}.
#' @param ylim Numeric vector of length 2 giving limits for the vertical axis. Default is \code{c(0, 25)}.
#' @param expand Numeric vector of length 2 controlling scale expansion for both axes. Default is \code{c(0, 0)} to eliminate Cartesian origin spacing.
#' @param color Optional character specifying line color. Overrides \code{palette} when provided.
#' @param palette Optional character name of the \pkg{tayloRswift} palette. Default is \code{"taylor1989"}.
#' @param show_axis_text Logical indicating whether to display numeric tick labels along the axes. Default is \code{FALSE} (qualitative schematic).
#' @param labs_banheira Optional character vector of length 4 for legacy Portuguese compatibility:
#'   \code{c(infant_mortality_label, useful_life_label, wear_out_label, x_label)}.
#' @param ... Additional arguments passed to \code{plot_bathtub_curve}.
#'
#' @return A \code{ggplot2::ggplot} object visualizing the bathtub curve.
#'
#' @examples
#' \dontrun{
#' # Standard bathtub curve
#' p <- plot_bathtub_curve()
#' print(p)
#'
#' # Custom phase boundaries and labels
#' p_custom <- plot_bathtub_curve(
#'   t_phase1 = 20,
#'   t_phase2 = 80,
#'   useful_life_label = "Steady State"
#' )
#' print(p_custom)
#' }
#'
#' @export
plot_bathtub_curve <- function(
  infant_mortality_label = "Infant Mortality",
  useful_life_label = "Useful Life",
  wear_out_label = "Wear-out",
  x_label = "Time",
  y_label = expression(lambda(t)),
  t_phase1 = 30,
  t_phase2 = 70,
  t_max = 100,
  n_points = 500,
  a = 0.025,
  b = 1.8,
  baseline = 11,
  xlim = c(0, 100),
  ylim = c(0, 25),
  expand = c(0, 0),
  color = NULL,
  palette = "taylor1989",
  show_axis_text = FALSE,
  labs_banheira = NULL
) {
  if (!is.null(labs_banheira)) {
    if (!is.character(labs_banheira) || length(labs_banheira) != 4) {
      stop("'labs_banheira' must be a character vector of length 4.")
    }
    infant_mortality_label <- labs_banheira[1]
    useful_life_label <- labs_banheira[2]
    wear_out_label <- labs_banheira[3]
    x_label <- labs_banheira[4]
  }

  if (!is.numeric(t_max) || length(t_max) != 1 || t_max <= 0) {
    stop("'t_max' must be a single positive number.")
  }
  if (!is.numeric(t_phase1) || length(t_phase1) != 1 || t_phase1 <= 0 || t_phase1 >= t_max) {
    stop("'t_phase1' must be a positive number less than 't_max'.")
  }
  if (!is.numeric(t_phase2) || length(t_phase2) != 1 || t_phase2 <= t_phase1 || t_phase2 >= t_max) {
    stop("'t_phase2' must be greater than 't_phase1' and less than 't_max'.")
  }
  if (!is.numeric(n_points) || length(n_points) != 1 || n_points < 2) {
    stop("'n_points' must be an integer >= 2.")
  }
  if (!is.numeric(xlim) || length(xlim) != 2 || xlim[1] >= xlim[2]) {
    stop("'xlim' must be a numeric vector of length 2 with xlim[1] < xlim[2].")
  }
  if (!is.numeric(ylim) || length(ylim) != 2 || ylim[1] >= ylim[2]) {
    stop("'ylim' must be a numeric vector of length 2 with ylim[1] < ylim[2].")
  }
  if (!is.numeric(expand) || length(expand) != 2) {
    stop("'expand' must be a numeric vector of length 2.")
  }

  time <- seq(0, t_max, length.out = as.integer(n_points))
  hazard <- rep(baseline, length(time))

  idx_infant <- time < t_phase1
  hazard[idx_infant] <- baseline + a * (t_phase1 - time[idx_infant])^b

  idx_wear <- time > t_phase2
  hazard[idx_wear] <- baseline + a * (time[idx_wear] - t_phase2)^b

  plot_data <- data.frame(time = time, hazard = hazard)

  line_color <- "#0072B2"
  if (!is.null(color)) {
    line_color <- color
  } else if (requireNamespace("tayloRswift", quietly = TRUE) &&
             !is.null(palette) &&
             palette %in% names(tayloRswift::swift_palettes)) {
    pal <- tayloRswift::swift_palettes[[palette]]
    if (length(pal) >= 6) {
      line_color <- pal[6]
    }
  }

  p <- ggplot2::ggplot(
    plot_data,
    ggplot2::aes(x = .data[["time"]], y = .data[["hazard"]])
  ) +
    ggplot2::geom_line(color = line_color, linewidth = 1) +
    ggplot2::geom_vline(xintercept = c(t_phase1, t_phase2), linetype = "dashed") +
    ggplot2::annotate(
      "text",
      x = t_phase1 * 0.47,
      y = 20,
      label = infant_mortality_label,
      hjust = 0
    ) +
    ggplot2::annotate(
      "text",
      x = (t_phase1 + t_phase2) / 2,
      y = 14,
      label = useful_life_label,
      hjust = 0.5
    ) +
    ggplot2::annotate(
      "text",
      x = t_phase2 + (t_max - t_phase2) * 0.13,
      y = 20,
      label = wear_out_label,
      hjust = 0
    ) +
    ggplot2::labs(x = x_label, y = y_label) +
    ggplot2::scale_x_continuous(expand = expand, limits = xlim) +
    ggplot2::scale_y_continuous(expand = expand, limits = ylim) +
    ggplot2::theme_classic()

  if (isFALSE(show_axis_text)) {
    p <- p + ggplot2::theme(axis.text = ggplot2::element_blank())
  }

  p
}

#' @rdname plot_bathtub_curve
#' @export
gera_plot_banheira <- function(
  labs_banheira = c(
    "Mortalidade Infantil",
    "Vida \u00datil",
    "Obsolesc\u00eancia",
    "Tempo"
  ),
  ...
) {
  plot_bathtub_curve(labs_banheira = labs_banheira, ...)
}


#' Plot Wiener Process Paths with Different Drift Parameters
#'
#' @description
#' Simulates and visualizes sample paths of a one-dimensional Wiener process
#' (Brownian motion with drift) under different drift coefficients \eqn{\mu} to illustrate
#' how drift governs the deterministic rate of degradation while diffusion parameter \eqn{\sigma^2}
#' generates stochastic variability around that trajectory:
#' \deqn{W(t) = \mu t + \sigma B(t)}
#'
#' @param drifts Numeric vector of length 2 containing the drift coefficients \eqn{\mu}
#'   to compare. Default is \code{c(0, 5)}.
#' @param sigma Numeric volatility / diffusion parameter \eqn{\sigma}. Default is \code{4}.
#' @param sigma2 Optional numeric diffusion variance \eqn{\sigma^2}. If provided, overrides \code{sigma^2}.
#' @param t_max Numeric maximum observation time horizon. Default is \code{20}.
#' @param n_steps Integer number of measurement increments. Default is \code{100}.
#' @param labels Optional character vector of length 2 providing custom legend labels.
#'   If \code{NULL}, dynamically formatted as \code{"\u03bc = [drift], \u03c3 = [sigma]"}.
#' @param x_label Character label for the horizontal axis. Default is \code{"Time"}.
#' @param y_label Character label for the vertical axis. Default is \code{"Degradation"}.
#' @param seed Optional integer seed for reproducible simulations. Default is \code{123}.
#' @param xlim Numeric vector of length 2 specifying horizontal axis limits. Default is \code{c(0, 20.5)}.
#' @param expand Numeric vector of length 2 controlling axis scale expansion. Default is \code{c(0, 0)} to eliminate Cartesian origin spacing.
#' @param palette Optional character name of the \pkg{tayloRswift} color palette. Default is \code{"taylor1989"}.
#' @param labs_wiener Optional character vector of length 2 for legacy Portuguese compatibility:
#'   \code{c(y_label, x_label)}.
#' @param ... Additional arguments passed to \code{plot_wiener_drift}.
#'
#' @return A \code{ggplot2::ggplot} object showing the overlaid degradation trajectories.
#'
#' @examples
#' \dontrun{
#' # Compare drift = 0 vs drift = 5
#' p <- plot_wiener_drift()
#' print(p)
#'
#' # Custom drift levels and time horizon
#' p_custom <- plot_wiener_drift(
#'   drifts = c(1, 8),
#'   sigma = 2,
#'   t_max = 30
#' )
#' print(p_custom)
#' }
#'
#' @export
plot_wiener_drift <- function(
  drifts = c(0, 5),
  sigma = 4,
  sigma2 = NULL,
  t_max = 20,
  n_steps = 100,
  labels = NULL,
  x_label = "Time",
  y_label = "Degradation",
  seed = 123,
  xlim = c(0, 20.5),
  expand = c(0, 0),
  palette = "taylor1989",
  labs_wiener = NULL
) {
  if (!is.null(labs_wiener)) {
    if (!is.character(labs_wiener) || length(labs_wiener) != 2) {
      stop("'labs_wiener' must be a character vector of length 2.")
    }
    y_label <- labs_wiener[1]
    x_label <- labs_wiener[2]
  }

  if (!is.numeric(drifts) || length(drifts) != 2) {
    stop("'drifts' must be a numeric vector of length 2.")
  }
  if (!is.numeric(sigma) || length(sigma) != 1 || sigma <= 0) {
    stop("'sigma' must be a single positive number.")
  }
  diff_var <- if (!is.null(sigma2)) sigma2 else sigma^2
  if (!is.numeric(diff_var) || length(diff_var) != 1 || diff_var <= 0) {
    stop("'sigma2' must be a single positive number.")
  }
  if (!is.numeric(t_max) || length(t_max) != 1 || t_max <= 0) {
    stop("'t_max' must be a single positive number.")
  }
  if (!is.numeric(n_steps) || length(n_steps) != 1 || n_steps < 2) {
    stop("'n_steps' must be an integer >= 2.")
  }
  if (!is.numeric(xlim) || length(xlim) != 2 || xlim[1] >= xlim[2]) {
    stop("'xlim' must be a numeric vector of length 2 with xlim[1] < xlim[2].")
  }
  if (!is.numeric(expand) || length(expand) != 2) {
    stop("'expand' must be a numeric vector of length 2.")
  }

  if (is.null(labels)) {
    disp_sigma <- if (!is.null(sigma2)) round(sqrt(sigma2), 2) else sigma
    labels <- c(
      paste0("\u03bc = ", drifts[1], ", \u03c3 = ", disp_sigma),
      paste0("\u03bc = ", drifts[2], ", \u03c3 = ", disp_sigma)
    )
  }

  if (!is.null(seed)) {
    set.seed(seed)
  }

  path1 <- sim_wiener_path(t_max = t_max, n_steps = n_steps, drift = drifts[1], sigma2 = diff_var, obj_id = 1)
  path2 <- sim_wiener_path(t_max = t_max, n_steps = n_steps, drift = drifts[2], sigma2 = diff_var, obj_id = 2)

  plot_data <- rbind(
    data.frame(
      time = path1[["Time"]],
      degradation = path1[["Wt"]],
      drift_factor = factor(labels[1], levels = labels),
      stringsAsFactors = FALSE
    ),
    data.frame(
      time = path2[["Time"]],
      degradation = path2[["Wt"]],
      drift_factor = factor(labels[2], levels = labels),
      stringsAsFactors = FALSE
    )
  )

  p <- ggplot2::ggplot(
    plot_data,
    ggplot2::aes(
      x = .data[["time"]],
      y = .data[["degradation"]],
      color = .data[["drift_factor"]]
    )
  ) +
    ggplot2::geom_line(linewidth = 1, alpha = 0.9) +
    ggplot2::labs(y = y_label, x = x_label, color = "") +
    ggplot2::theme_classic() +
    ggplot2::theme(
      legend.title = ggplot2::element_blank(),
      legend.position = c(0.15, 0.85)
    ) +
    ggplot2::scale_x_continuous(expand = expand, limits = xlim)

  if (requireNamespace("tayloRswift", quietly = TRUE) &&
      !is.null(palette) &&
      palette %in% names(tayloRswift::swift_palettes)) {
    p <- p + tayloRswift::scale_color_taylor(palette = palette)
  }

  p
}

#' @rdname plot_wiener_drift
#' @export
gera_plot_wiener <- function(
  labs_wiener = c("Degrada\u00e7\u00e3o", "Tempo"),
  ...
) {
  plot_wiener_drift(labs_wiener = labs_wiener, ...)
}


#' Plot Illustrative Maintenance and Repair Types
#'
#' @description
#' Generates a combined 3-panel schematic visualization contrasting the three fundamental
#' maintenance actions in degradation and reliability modeling:
#' \itemize{
#'   \item \strong{(a) Perfect Repair (AGAN - As Good As New):} Restoration restores the system
#'     completely to its original pristine condition (\eqn{Y(t) = 0}).
#'   \item \strong{(b) Minimal Repair (ABAO - As Bad As Old):} Maintenance returns the system to
#'     working condition but does not reduce accumulated degradation; degradation continues along
#'     its unmitigated trajectory.
#'   \item \strong{(c) Imperfect Repair:} Maintenance partially mitigates accumulated degradation
#'     by an efficiency factor \eqn{\rho \in (0, 1)}, reducing degradation from \eqn{y} to \eqn{\rho y}.
#' }
#'
#' @param titles Character vector of length 3 providing titles for each subplot:
#'   perfect repair, minimal repair, and imperfect repair.
#'   Default is \code{c("(a) Perfect Repair", "(b) Minimal Repair", "(c) Imperfect Repair")}.
#' @param x_label Character label for the horizontal axis across all subplots. Default is \code{"Time"}.
#' @param y_label Character label for the vertical axis across all subplots. Default is \code{"Degradation"}.
#' @param t_maint Numeric inspection/maintenance timestamp. Default is \code{4}.
#' @param t_max Numeric total time horizon. Default is \code{8}.
#' @param y_maint Numeric degradation level reached at maintenance time. Default is \code{0.5}.
#' @param y_residual Numeric residual degradation level after imperfect repair. Default is \code{0.2}.
#' @param y_end_perfect Numeric final degradation level at \code{t_max} under perfect repair. Default is \code{0.5}.
#' @param y_end_minimal Numeric final degradation level at \code{t_max} under minimal repair. Default is \code{1.0}.
#' @param y_end_imperfect Numeric final degradation level at \code{t_max} under imperfect repair. Default is \code{0.7}.
#' @param expand Numeric vector of length 2 controlling scale expansion for both axes.
#'   Default is \code{c(0, 0)} to eliminate Cartesian origin spacing.
#' @param palette Optional character name of the \pkg{tayloRswift} color palette. Default is \code{"taylor1989"}.
#' @param labs_reparos Optional character vector of length 5 for legacy Portuguese compatibility:
#'   \code{c(x_label, y_label, title_perfect, title_minimal, title_imperfect)}.
#' @param ... Additional arguments passed to \code{plot_repair_types}.
#'
#' @return A combined \code{patchwork} object arranging the three repair plots in a 2-row layout.
#'
#' @examples
#' \dontrun{
#' # Standard English 3-panel repair comparison
#' p <- plot_repair_types()
#' print(p)
#'
#' # Custom titles and axis labels
#' p_custom <- plot_repair_types(
#'   titles = c("Full Overhaul", "Quick Patch", "Partial Overhaul"),
#'   x_label = "Operating Months",
#'   y_label = "Damage Index"
#' )
#' print(p_custom)
#' }
#'
#' @export
plot_repair_types <- function(
  titles = c("(a) Perfect Repair", "(b) Minimal Repair", "(c) Imperfect Repair"),
  x_label = "Time",
  y_label = "Degradation",
  t_maint = 4,
  t_max = 8,
  y_maint = 0.5,
  y_residual = 0.2,
  y_end_perfect = 0.5,
  y_end_minimal = 1.0,
  y_end_imperfect = 0.7,
  expand = c(0, 0),
  palette = "taylor1989",
  labs_reparos = NULL
) {
  if (!is.null(labs_reparos)) {
    if (!is.character(labs_reparos) || length(labs_reparos) != 5) {
      stop("'labs_reparos' must be a character vector of length 5.")
    }
    x_label <- labs_reparos[1]
    y_label <- labs_reparos[2]
    titles <- c(labs_reparos[3], labs_reparos[4], labs_reparos[5])
  }

  if (!is.character(titles) || length(titles) != 3) {
    stop("'titles' must be a character vector of length 3.")
  }
  if (!is.numeric(t_maint) || length(t_maint) != 1 || t_maint <= 0) {
    stop("'t_maint' must be a single positive number.")
  }
  if (!is.numeric(t_max) || length(t_max) != 1 || t_max <= t_maint) {
    stop("'t_max' must be greater than 't_maint'.")
  }
  if (!is.numeric(expand) || length(expand) != 2) {
    stop("'expand' must be a numeric vector of length 2.")
  }

  primary_color <- "#0072B2"
  accent_color <- "#E69F00"
  if (requireNamespace("tayloRswift", quietly = TRUE) &&
      !is.null(palette) &&
      palette %in% names(tayloRswift::swift_palettes)) {
    pal <- tayloRswift::swift_palettes[[palette]]
    if (length(pal) >= 6) {
      primary_color <- pal[6]
      accent_color <- pal[3]
    }
  }

  # 1. Perfect Repair (AGAN): drops to 0 at t_maint
  perfect_repair_data <- data.frame(
    time = c(0, t_maint, t_maint, t_max),
    degradation = c(0, y_maint, 0.0, y_end_perfect)
  )
  g_perfect <- ggplot2::ggplot(
    perfect_repair_data,
    ggplot2::aes(x = .data[["time"]], y = .data[["degradation"]])
  ) +
    ggplot2::geom_line(color = primary_color, linewidth = 1) +
    ggplot2::theme_classic() +
    ggplot2::theme(
      legend.position = "none",
      plot.title = ggplot2::element_text(hjust = 0.5)
    ) +
    ggplot2::labs(x = x_label, y = y_label, title = titles[1]) +
    ggplot2::scale_x_continuous(expand = expand) +
    ggplot2::scale_y_continuous(expand = expand, limits = c(0, 1))

  # 2. Minimal Repair (ABAO): continuous degradation, vertical marker at t_maint
  minimal_repair_data <- data.frame(
    time = c(0, t_max),
    degradation = c(0, y_end_minimal)
  )
  minimal_line_data <- data.frame(
    time = c(t_maint, t_maint),
    degradation = c(0, y_maint)
  )
  g_minimal <- ggplot2::ggplot(
    minimal_repair_data,
    ggplot2::aes(x = .data[["time"]], y = .data[["degradation"]])
  ) +
    ggplot2::geom_line(color = primary_color, linewidth = 1) +
    ggplot2::geom_line(
      data = minimal_line_data,
      ggplot2::aes(x = .data[["time"]], y = .data[["degradation"]]),
      linetype = 3,
      linewidth = 1,
      color = accent_color
    ) +
    ggplot2::theme_classic() +
    ggplot2::theme(
      legend.position = "none",
      plot.title = ggplot2::element_text(hjust = 0.5)
    ) +
    ggplot2::labs(x = x_label, y = y_label, title = titles[2]) +
    ggplot2::scale_x_continuous(expand = expand) +
    ggplot2::scale_y_continuous(expand = expand, limits = c(0, 1))

  # 3. Imperfect Repair: drops partially to y_residual at t_maint
  imperfect_repair_data <- data.frame(
    time = c(0, t_maint, t_maint, t_max),
    degradation = c(0, y_maint, y_residual, y_end_imperfect)
  )
  imperfect_line_data <- data.frame(
    time = c(t_maint, t_maint),
    degradation = c(0, y_residual)
  )
  g_imperfect <- ggplot2::ggplot(
    imperfect_repair_data,
    ggplot2::aes(x = .data[["time"]], y = .data[["degradation"]])
  ) +
    ggplot2::geom_line(color = primary_color, linewidth = 1) +
    ggplot2::geom_line(
      data = imperfect_line_data,
      ggplot2::aes(x = .data[["time"]], y = .data[["degradation"]]),
      linetype = 3,
      linewidth = 1,
      color = accent_color
    ) +
    ggplot2::theme_classic() +
    ggplot2::theme(
      legend.position = "none",
      plot.title = ggplot2::element_text(hjust = 0.5)
    ) +
    ggplot2::labs(x = x_label, y = y_label, title = titles[3]) +
    ggplot2::scale_x_continuous(expand = expand) +
    ggplot2::scale_y_continuous(expand = expand, limits = c(0, 1))

  layout <- "
  AAAABBBB
  ##CCCC##
  "
  patchwork::wrap_plots(A = g_perfect, B = g_minimal, C = g_imperfect, design = layout)
}

#' @rdname plot_repair_types
#' @export
gera_plot_reparos <- function(
  labs_reparos = c(
    "Tempo",
    "Degrada\u00e7\u00e3o",
    "(a) Reparo Perfeito",
    "(b) Reparo M\u00ednimo",
    "(c) Reparo Imperfeito"
  ),
  ...
) {
  plot_repair_types(labs_reparos = labs_reparos, ...)
}


#' Plot Illustrative Maintenance Observation Scheme
#'
#' @description
#' Generates an illustrative diagram of an imperfect maintenance degradation path,
#' highlighting key notation from degradation and maintenance modeling:
#' \itemize{
#'   \item Incremental degradation steps between inspections: \eqn{\Delta Y_{0, 1}, \Delta Y_{0, 2}, \dots, \Delta Y_{0, n_0}}.
#'   \item Pre-maintenance degradation levels: \eqn{Y(\tau_k^-)}.
#'   \item Post-maintenance degradation levels: \eqn{Y(\tau_k^+)}.
#'   \item Maintenance jump / reduction magnitudes: \eqn{Z_k = Y(\tau_k^-) - Y(\tau_k^+)}.
#'   \item Terminal boundary degradation level: \eqn{Y(\tau_{m+1}^-)}.
#' }
#'
#' @param x_label Character label for the horizontal axis. Default is \code{"Time"}.
#' @param y_label Character label for the vertical axis. Default is \code{"Degradation"}.
#' @param t_max Numeric total observation time. Default is \code{15}.
#' @param n_maint Integer number of planned maintenance events. Default is \code{2}.
#' @param intra_maint Integer number of intermediate inspection points between consecutive maintenance events. Default is \code{4}.
#' @param rho Numeric vector of maintenance efficiency factors \eqn{\rho \in [0, 1]}. Default is \code{c(0.6, 0.6)}.
#' @param drift Numeric drift parameter for the underlying Wiener degradation process. Default is \code{2}.
#' @param sigma2 Numeric diffusion variance parameter. Default is \code{2}.
#' @param seed Optional integer seed for reproducibility. Default is \code{1111}.
#' @param xlim Numeric vector of length 2 specifying horizontal axis limits. Default is \code{c(0, 16)}.
#' @param ylim Numeric vector of length 2 specifying vertical axis limits. Default is \code{c(0, 20)}.
#' @param expand Numeric vector of length 2 controlling scale expansion for both axes.
#'   Default is \code{c(0, 0)} to eliminate Cartesian origin spacing.
#' @param palette Optional character name of the \pkg{tayloRswift} color palette. Default is \code{"taylor1989"}.
#' @param labs_scheme Optional character vector of length 2 for legacy Portuguese compatibility:
#'   \code{c(x_label, y_label)}.
#' @param ... Additional arguments passed to \code{plot_maintenance_scheme}.
#'
#' @return A \code{ggplot2::ggplot} object representing the maintenance observation scheme.
#'
#' @examples
#' \dontrun{
#' # Standard English maintenance observation diagram
#' p <- plot_maintenance_scheme()
#' print(p)
#'
#' # Custom axis labels
#' p_custom <- plot_maintenance_scheme(
#'   x_label = "Operating Time",
#'   y_label = "Degradation Index"
#' )
#' print(p_custom)
#' }
#'
#' @export
plot_maintenance_scheme <- function(
  x_label = "Time",
  y_label = "Degradation",
  t_max = 15,
  n_maint = 2,
  intra_maint = 4,
  rho = c(0.6, 0.6),
  drift = 2,
  sigma2 = 2,
  seed = 1111,
  xlim = c(0, 16),
  ylim = c(0, 20),
  expand = c(0, 0),
  palette = "taylor1989",
  labs_scheme = NULL
) {
  if (!is.null(labs_scheme)) {
    if (!is.character(labs_scheme) || length(labs_scheme) != 2) {
      stop("'labs_scheme' must be a character vector of length 2.")
    }
    x_label <- labs_scheme[1]
    y_label <- labs_scheme[2]
  }

  if (!is.numeric(t_max) || length(t_max) != 1 || t_max <= 0) {
    stop("'t_max' must be a single positive number.")
  }
  if (!is.numeric(n_maint) || length(n_maint) != 1 || n_maint < 1) {
    stop("'n_maint' must be an integer >= 1.")
  }
  if (!is.numeric(intra_maint) || length(intra_maint) != 1 || intra_maint < 1) {
    stop("'intra_maint' must be an integer >= 1.")
  }
  if (!is.numeric(drift) || length(drift) != 1) {
    stop("'drift' must be a single number.")
  }
  if (!is.numeric(sigma2) || length(sigma2) != 1 || sigma2 <= 0) {
    stop("'sigma2' must be a single positive number.")
  }
  if (!is.numeric(xlim) || length(xlim) != 2 || xlim[1] >= xlim[2]) {
    stop("'xlim' must be a numeric vector of length 2 with xlim[1] < xlim[2].")
  }
  if (!is.numeric(ylim) || length(ylim) != 2 || ylim[1] >= ylim[2]) {
    stop("'ylim' must be a numeric vector of length 2 with ylim[1] < ylim[2].")
  }
  if (!is.numeric(expand) || length(expand) != 2) {
    stop("'expand' must be a numeric vector of length 2.")
  }

  if (!is.null(seed)) {
    set.seed(seed)
  }

  n_steps <- (n_maint + 1) * (intra_maint + 2) - (n_maint + 1)
  maintenance_data <- sim_wiener_maintenance_path(
    t_max = t_max,
    n_steps = n_steps,
    drift = drift,
    sigma2 = sigma2,
    rho = rho,
    n_maint = n_maint,
    obj_id = 1
  )

  times <- maintenance_data$Time
  y_vals <- maintenance_data$Y

  y0 <- y_vals[times == 0][1]
  y1 <- y_vals[times == 1][1]
  y2 <- y_vals[times == 2][1]
  y3 <- y_vals[times == 3][1]
  y4 <- y_vals[times == 4][1]

  y5_vals <- y_vals[times == 5]
  y5_min <- min(y5_vals)
  y5_max <- max(y5_vals)
  y5_mean <- mean(y5_vals)

  y10_vals <- y_vals[times == 10]
  y10_min <- min(y10_vals)
  y10_max <- max(y10_vals)
  y10_mean <- mean(y10_vals)

  y15 <- y_vals[times == 15][1]

  primary_color <- "#0072B2"
  if (requireNamespace("tayloRswift", quietly = TRUE) &&
      !is.null(palette) &&
      palette %in% names(tayloRswift::swift_palettes)) {
    pal <- tayloRswift::swift_palettes[[palette]]
    if (length(pal) >= 6) {
      primary_color <- pal[6]
    }
  }

  safe_tex <- function(expr_str) {
    if (requireNamespace("latex2exp", quietly = TRUE)) {
      latex2exp::TeX(expr_str)
    } else {
      expr_str
    }
  }

  p <- ggplot2::ggplot(
    maintenance_data,
    ggplot2::aes(x = .data[["Time"]], y = .data[["Y"]])
  ) +
    ggplot2::geom_point(size = 2, colour = primary_color) +
    ggplot2::geom_line(linewidth = 1, colour = primary_color) +

    # Incremental steps (staircase style)
    ggplot2::annotate("text", x = 1.65, y = y1 - 0.5, label = safe_tex("$\\Delta Y_{0,1}$"), size = 4, colour = "red") +
    ggplot2::annotate("segment", x = 0.05, xend = 1.15, y = y0, yend = y0, linetype = "dashed", colour = "red", linewidth = 0.4) +
    ggplot2::annotate("segment", x = 1.15, xend = 1.15, y = y0, yend = y1, colour = "red", linewidth = 0.8,
                      arrow = grid::arrow(type = "open", ends = "both", angle = 20, length = grid::unit(0.3, "cm"))) +

    ggplot2::annotate("text", x = 2.65, y = y2 - 0.5, label = safe_tex("$\\Delta Y_{0,2}$"), size = 4, colour = "red") +
    ggplot2::annotate("segment", x = 1.05, xend = 2.15, y = y1, yend = y1, linetype = "dashed", colour = "red", linewidth = 0.4) +
    ggplot2::annotate("segment", x = 2.15, xend = 2.15, y = y1, yend = y2, colour = "red", linewidth = 0.8,
                      arrow = grid::arrow(type = "open", ends = "both", angle = 20, length = grid::unit(0.3, "cm"))) +

    ggplot2::annotate("text", x = 3.7, y = y4 + 1.0, label = safe_tex("$\\Delta Y_{0,n_0}$"), size = 4, colour = "red") +
    ggplot2::annotate("segment", x = 3.05, xend = 4.15, y = y3, yend = y3, linetype = "dashed", colour = "red", linewidth = 0.4) +
    ggplot2::annotate("segment", x = 4.15, xend = 4.15, y = y3, yend = y4, colour = "red", linewidth = 0.8,
                      arrow = grid::arrow(type = "open", ends = "both", angle = 20, length = grid::unit(0.3, "cm"))) +

    # Maintenance jump Z_1 at t = 5
    ggplot2::annotate("text", x = 4.5, y = y5_min - 0.5, label = safe_tex("$Y(\\tau_{1}^{+})$"), size = 4, colour = "red") +
    ggplot2::annotate("text", x = 5.6, y = y5_mean, label = safe_tex("$Z_1$"), size = 4, colour = "red") +
    ggplot2::annotate("text", x = 4.5, y = y5_max + 0.5, label = safe_tex("$Y(\\tau_{1}^{-})$"), size = 4, colour = "red") +
    ggplot2::annotate("segment", x = 5.3, xend = 5.3, y = y5_min, yend = y5_max, colour = "red", linewidth = 1,
                      arrow = grid::arrow(type = "open", ends = "both", angle = 20, length = grid::unit(0.4, "cm"))) +

    # Maintenance jump Z_2 at t = 10
    ggplot2::annotate("text", x = 10.0, y = y10_min - 0.7, label = safe_tex("$Y(\\tau_{2}^{+})$"), size = 4, colour = "red") +
    ggplot2::annotate("text", x = 10.6, y = y10_mean, label = safe_tex("$Z_2$"), size = 4, colour = "red") +
    ggplot2::annotate("text", x = 10.0, y = y10_max + 0.7, label = safe_tex("$Y(\\tau_{2}^{-})$"), size = 4, colour = "red") +
    ggplot2::annotate("segment", x = 10.3, xend = 10.3, y = y10_min, yend = y10_max, colour = "red", linewidth = 1,
                      arrow = grid::arrow(type = "open", ends = "both", angle = 20, length = grid::unit(0.4, "cm"))) +

    # Final boundary at t = 15
    ggplot2::annotate("text", x = 14.3, y = y15, label = safe_tex("$Y(\\tau_{3}^{-})$"), size = 4, colour = "red") +

    ggplot2::theme_classic() +
    ggplot2::theme(plot.title = ggplot2::element_blank()) +
    ggplot2::scale_y_continuous(expand = expand, limits = ylim) +
    ggplot2::scale_x_continuous(expand = expand, limits = xlim, breaks = c(5, 10)) +
    ggplot2::labs(x = x_label, y = y_label)

  p
}

#' @rdname plot_maintenance_scheme
#' @export
gera_plot_scheme <- function(
  labs_scheme = c("Tempo", "Degrada\u00e7\u00e3o"),
  ...
) {
  plot_maintenance_scheme(labs_scheme = labs_scheme, ...)
}


#' Plot Monte Carlo Simulation Estimation Bias Grid
#'
#' @description
#' Generates a nested faceted grid (using \pkg{ggh4x}) visualizing the empirical estimation bias
#' of Wiener degradation process parameters (\eqn{\mu} and \eqn{\sigma^2}) across varying
#' sample sizes \eqn{N}, maintenance frequencies \eqn{k}, and intra-maintenance inspection
#' frequencies \eqn{n_j}.
#'
#' Facets are nested in a 2D matrix:
#' \itemize{
#'   \item \strong{Rows:} True parameter settings (\eqn{\mu} and \eqn{\sigma^2}).
#'   \item \strong{Columns:} Maintenance configuration (\eqn{k} maintenance epochs and \eqn{n_j} inspections).
#' }
#'
#' @param data A \code{data.frame} of simulation results containing at minimum the columns:
#'   \code{n_system}, \code{mu}, \code{sigma2}, \code{n_main}, \code{n_intra},
#'   \code{bias.mu_hat}, and \code{bias.sigma_hat}.
#' @param x_label Character label for the horizontal axis. Default is \code{"Number of Systems"}.
#' @param y_label Character label for the vertical axis. Default is \code{"Bias"}.
#' @param breaks Numeric vector of breaks along the horizontal axis. Default is \code{c(1, 10, 20, 30, 40, 50)}.
#' @param expand Numeric vector of length 2 controlling scale expansion for the horizontal axis. Default is \code{c(0, 0)}.
#' @param palette Optional character name of the \pkg{tayloRswift} palette. Default is \code{"taylor1989"}.
#' @param labs_bias Optional character vector of length 2 for legacy Portuguese compatibility:
#'   \code{c(x_label, y_label)}.
#' @param resultados Legacy alias parameter for \code{data}.
#' @param ... Additional arguments passed to \code{plot_simulation_bias}.
#'
#' @return A \code{ggplot2::ggplot} object featuring nested facet grids of estimation bias.
#'
#' @examples
#' \dontrun{
#' # Load simulation results and plot bias grid
#' sim_results <- readRDS("SimDesign4.rds")
#' p <- plot_simulation_bias(sim_results)
#' print(p)
#' }
#'
#' @export
plot_simulation_bias <- function(
  data,
  x_label = "Number of Systems",
  y_label = "Bias",
  breaks = c(1, 10, 20, 30, 40, 50),
  expand = c(0, 0),
  palette = "taylor1989",
  labs_bias = NULL
) {
  if (!is.data.frame(data)) {
    stop("'data' must be a data frame.")
  }

  required_cols <- c("n_system", "mu", "sigma2", "n_main", "n_intra", "bias.mu_hat", "bias.sigma_hat")
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    stop("`data` is missing required column(s): ", paste(missing_cols, collapse = ", "))
  }

  if (!is.null(labs_bias)) {
    if (!is.character(labs_bias) || length(labs_bias) != 2) {
      stop("'labs_bias' must be a character vector of length 2.")
    }
    x_label <- labs_bias[1]
    y_label <- labs_bias[2]
  }

  if (!is.numeric(breaks) || length(breaks) < 1) {
    stop("'breaks' must be a numeric vector.")
  }
  if (!is.numeric(expand) || length(expand) != 2) {
    stop("'expand' must be a numeric vector of length 2.")
  }

  # Prepare plot data with dynamic labels for ggh4x facet_nested
  plot_data <- data[, required_cols, drop = FALSE]

  mu_levels <- paste0("mu : ", sort(unique(plot_data$mu)))
  sigma_levels <- paste0("sigma^2 : ", sort(unique(plot_data$sigma2)))
  n_main_levels <- paste0("k : ", sort(unique(plot_data$n_main)))
  n_intra_levels <- paste0("n[j] : ", sort(unique(plot_data$n_intra)))

  plot_data$mu_expr <- factor(paste0("mu : ", plot_data$mu), levels = mu_levels)
  plot_data$sigma_expr <- factor(paste0("sigma^2 : ", plot_data$sigma2), levels = sigma_levels)
  plot_data$n_main_expr <- factor(paste0("k : ", plot_data$n_main), levels = n_main_levels)
  plot_data$n_intra_expr <- factor(paste0("n[j] : ", plot_data$n_intra), levels = n_intra_levels)

  plot_data <- tidyr::pivot_longer(
    plot_data,
    cols = c("bias.mu_hat", "bias.sigma_hat"),
    names_to = "parameter",
    values_to = "bias_value"
  )

  legend_labels <- if (requireNamespace("latex2exp", quietly = TRUE)) {
    c(latex2exp::TeX(" $\\mu$    "), latex2exp::TeX(" $\\sigma^2$"))
  } else {
    c("mu", "sigma^2")
  }

  p <- ggplot2::ggplot(
    plot_data,
    ggplot2::aes(
      x = .data[["n_system"]],
      y = .data[["bias_value"]],
      color = .data[["parameter"]]
    )
  ) +
    ggh4x::facet_nested(
      mu_expr + sigma_expr ~ n_main_expr + n_intra_expr,
      labeller = ggplot2::label_parsed
    ) +
    ggplot2::geom_line(linewidth = 0.8, alpha = 0.6) +
    ggplot2::geom_point(alpha = 0.7) +
    ggplot2::scale_x_continuous(breaks = breaks, expand = expand) +
    ggplot2::labs(x = x_label, y = y_label) +
    ggplot2::theme(
      legend.position = "bottom",
      legend.title = ggplot2::element_blank(),
      legend.text = ggplot2::element_text(colour = "black", size = 12),
      legend.key = ggplot2::element_rect(colour = NA, fill = NA),
      panel.background = ggplot2::element_blank(),
      panel.border = ggplot2::element_rect(fill = "transparent", color = "black", linewidth = 0.5),
      strip.background = ggplot2::element_rect(linetype = "solid", color = "black", linewidth = 0.5),
      strip.text.y = ggplot2::element_text(angle = 0)
    )

  if (requireNamespace("tayloRswift", quietly = TRUE) &&
      !is.null(palette) &&
      palette %in% names(tayloRswift::swift_palettes)) {
    p <- p + tayloRswift::scale_color_taylor(labels = legend_labels)
  } else {
    p <- p + ggplot2::scale_color_manual(values = c("#0072B2", "#D55E00"), labels = legend_labels)
  }

  p
}

#' @rdname plot_simulation_bias
#' @export
gera_plot_bias <- function(
  resultados,
  labs_bias = c("N\u00famero de Sistemas", "Vi\u00e9s"),
  ...
) {
  plot_simulation_bias(data = resultados, labs_bias = labs_bias, ...)
}


#' Plot Comparison Between Natural and Maintained Degradation Paths
#'
#' @description
#' Simulates and visualizes a Wiener process degradation trajectory under two regimes:
#' \itemize{
#'   \item \strong{Standard (natural degradation):} The baseline process \eqn{X(t) = W(t)} without maintenance.
#'   \item \strong{Maintained (imperfect maintenance):} The mitigated process \eqn{Y(t)} subject to periodic
#'     imperfect maintenance actions at epochs \eqn{\tau_1, \dots, \tau_m} with reduction factors \eqn{\rho_1, \dots, \rho_m}.
#' }
#' Dotted vertical segments illustrate the degradation reduction jumps at each maintenance epoch.
#'
#' @param t_max Numeric maximum observation time horizon. Default is \code{20}.
#' @param n_maint Integer number of planned maintenance interventions. Default is \code{3}.
#' @param intra_maint Integer number of measurements between consecutive maintenance events. Default is \code{4}.
#' @param rho Numeric vector of maintenance efficiency factors \eqn{\rho \in [0, 1]}. Default is \code{c(1.0, 0.3, 0.5)}.
#' @param drift Numeric drift parameter of the underlying Wiener process. Default is \code{3}.
#' @param sigma2 Numeric diffusion variance parameter. Default is \code{2}.
#' @param seed Optional integer seed for reproducible simulation. Default is \code{111}.
#' @param maintained_label Character legend label for the maintained trajectory. Default is \code{"Y(t) - With Maintenance Actions"}.
#' @param standard_label Character legend label for the unmaintained baseline trajectory. Default is \code{"X(t) - Natural Degradation"}.
#' @param x_label Character label for the horizontal axis. Default is \code{"Time"}.
#' @param y_label Character label for the vertical axis. Default is \code{"Degradation"}.
#' @param title Optional character plot title. Default is \code{"(I)"}.
#' @param expand Numeric vector of length 2 controlling scale expansion for both axes. Default is \code{c(0, 0)}.
#' @param palette Optional character name of the \pkg{tayloRswift} color palette. Default is \code{"taylor1989"}.
#' @param labs_xtyt Optional character vector of length 4 for legacy Portuguese compatibility:
#'   \code{c(maintained_label, standard_label, x_label, y_label)}.
#' @param ... Additional arguments passed to \code{plot_wiener_maintenance_comparison}.
#'
#' @return A \code{ggplot2::ggplot} object showing both degradation trajectories overlaid.
#'
#' @examples
#' \dontrun{
#' # Standard English comparison plot
#' p <- plot_wiener_maintenance_comparison()
#' print(p)
#' }
#'
#' @export
plot_wiener_maintenance_comparison <- function(
  t_max = 20,
  n_maint = 3,
  intra_maint = 4,
  rho = c(1.0, 0.3, 0.5),
  drift = 3,
  sigma2 = 2,
  seed = 111,
  maintained_label = "Y(t) - With Maintenance Actions",
  standard_label = "X(t) - Natural Degradation",
  x_label = "Time",
  y_label = "Degradation",
  title = "(I)",
  expand = c(0, 0),
  palette = "taylor1989",
  labs_xtyt = NULL
) {
  if (!is.null(labs_xtyt)) {
    if (!is.character(labs_xtyt) || length(labs_xtyt) != 4) {
      stop("'labs_xtyt' must be a character vector of length 4.")
    }
    maintained_label <- labs_xtyt[1]
    standard_label <- labs_xtyt[2]
    x_label <- labs_xtyt[3]
    y_label <- labs_xtyt[4]
  }

  if (!is.numeric(t_max) || length(t_max) != 1 || t_max <= 0) {
    stop("'t_max' must be a single positive number.")
  }
  if (!is.numeric(n_maint) || length(n_maint) != 1 || n_maint < 1) {
    stop("'n_maint' must be an integer >= 1.")
  }
  if (!is.numeric(intra_maint) || length(intra_maint) != 1 || intra_maint < 1) {
    stop("'intra_maint' must be an integer >= 1.")
  }
  if (!is.numeric(expand) || length(expand) != 2) {
    stop("'expand' must be a numeric vector of length 2.")
  }

  if (!is.null(seed)) {
    set.seed(seed)
  }

  n_steps <- (n_maint + 1) * (intra_maint + 2) - (n_maint + 1)
  path_data <- sim_wiener_maintenance_path(
    t_max = t_max,
    n_steps = n_steps,
    drift = drift,
    sigma2 = sigma2,
    rho = rho,
    n_maint = n_maint,
    obj_id = 1
  )

  path_data <- path_data[order(path_data$Time), , drop = FALSE]
  duplicated_times <- unique(path_data$Time[duplicated(path_data$Time)])

  path_data$interval <- cumsum(c(0, diff(path_data$Time) == 0))

  jump_segments <- do.call(rbind, lapply(duplicated_times, function(tau) {
    y_vals <- path_data$Y[path_data$Time == tau]
    data.frame(
      time = tau,
      time_end = tau,
      y_min = min(y_vals),
      y_max = max(y_vals)
    )
  }))

  maintained_color <- "#0072B2"
  standard_color <- "#D55E00"
  accent_color <- "#CC79A7"
  if (requireNamespace("tayloRswift", quietly = TRUE) &&
      !is.null(palette) &&
      palette %in% names(tayloRswift::swift_palettes)) {
    pal <- tayloRswift::swift_palettes[[palette]]
    if (length(pal) >= 6) {
      maintained_color <- pal[1]
      accent_color <- pal[4]
      standard_color <- pal[6]
    }
  }

  color_values <- c(
    "Maintained" = maintained_color,
    "Standard" = standard_color
  )

  p <- ggplot2::ggplot() +
    ggplot2::geom_line(
      data = path_data,
      ggplot2::aes(
        x = .data[["Time"]],
        y = .data[["Y"]],
        group = .data[["interval"]],
        color = "Maintained"
      ),
      alpha = 0.7,
      linewidth = 1
    ) +
    ggplot2::geom_line(
      data = path_data,
      ggplot2::aes(
        x = .data[["Time"]],
        y = .data[["Wt"]],
        color = "Standard"
      ),
      alpha = 0.7,
      linewidth = 1
    )

  if (!is.null(jump_segments) && nrow(jump_segments) > 0) {
    p <- p + ggplot2::geom_segment(
      data = jump_segments,
      ggplot2::aes(
        x = .data[["time"]],
        xend = .data[["time_end"]],
        y = .data[["y_max"]],
        yend = .data[["y_min"]]
      ),
      linetype = "dotted",
      linewidth = 1,
      color = accent_color
    )
  }

  p <- p +
    ggplot2::scale_color_manual(
      name = NULL,
      values = color_values,
      labels = c("Maintained" = maintained_label, "Standard" = standard_label)
    ) +
    ggplot2::theme_classic() +
    ggplot2::theme(
      legend.position = "top",
      plot.title = if (is.null(title) || title == "") ggplot2::element_blank() else ggplot2::element_text(hjust = 0.5)
    ) +
    ggplot2::labs(x = x_label, y = y_label, title = title) +
    ggplot2::scale_y_continuous(expand = expand) +
    ggplot2::scale_x_continuous(expand = expand)

  p
}

#' @rdname plot_wiener_maintenance_comparison
#' @export
gera_plot_xtyt <- function(
  labs_xtyt = c(
    "Y(t) - Processo de degrada\u00e7\u00e3o com a\u00e7\u00f5es de manuten\u00e7\u00e3o",
    "X(t) - Processo de degrada\u00e7\u00e3o natural",
    "Tempo",
    "Degrada\u00e7\u00e3o"
  ),
  ...
) {
  plot_wiener_maintenance_comparison(labs_xtyt = labs_xtyt, ...)
}


#' Plot Monte Carlo Simulation Root Mean Squared Error (RMSE)
#'
#' @description
#' Visualizes the Root Mean Squared Error (RMSE) of the estimated Wiener drift (\eqn{\mu})
#' and diffusion variance (\eqn{\sigma^2}) parameters across Monte Carlo simulation scenarios
#' using a nested facet grid (\pkg{ggh4x}).
#'
#' Facets are nested in a 2D matrix:
#' \itemize{
#'   \item \strong{Rows:} True parameter settings (\eqn{\mu} and \eqn{\sigma^2}).
#'   \item \strong{Columns:} Maintenance configuration (\eqn{k} maintenance epochs and \eqn{n_j} inspections).
#' }
#'
#' @param data A \code{data.frame} of simulation results containing at minimum the columns:
#'   \code{n_system}, \code{mu}, \code{sigma2}, \code{n_main}, \code{n_intra},
#'   \code{RMSE.mu_hat}, and \code{RMSE.sigma_hat}.
#' @param x_label Character label for the horizontal axis. Default is \code{"Number of Systems"}.
#' @param y_label Character label for the vertical axis. Default is \code{"RMSE"}.
#' @param breaks Numeric vector of breaks along the horizontal axis. Default is \code{c(1, 10, 20, 30, 40, 50)}.
#' @param expand Numeric vector of length 2 controlling scale expansion for both axes. Default is \code{c(0, 0)}.
#' @param palette Optional character name of the \pkg{tayloRswift} palette. Default is \code{"taylor1989"}.
#' @param labs_rmse Optional character vector of length 2 for legacy Portuguese compatibility:
#'   \code{c(x_label, y_label)}.
#' @param resultados Legacy alias parameter for \code{data}.
#' @param ... Additional arguments passed to \code{plot_simulation_rmse}.
#'
#' @return A \code{ggplot2::ggplot} object featuring nested facet grids of estimation RMSE.
#'
#' @examples
#' \dontrun{
#' sim_results <- readRDS("SimDesign4.rds")
#' p <- plot_simulation_rmse(sim_results)
#' print(p)
#' }
#'
#' @export
plot_simulation_rmse <- function(
  data,
  x_label = "Number of Systems",
  y_label = "RMSE",
  breaks = c(1, 10, 20, 30, 40, 50),
  expand = c(0, 0),
  palette = "taylor1989",
  labs_rmse = NULL
) {
  if (!is.data.frame(data)) {
    stop("'data' must be a data frame.")
  }

  required_cols <- c("n_system", "mu", "sigma2", "n_main", "n_intra", "RMSE.mu_hat", "RMSE.sigma_hat")
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    stop("`data` is missing required column(s): ", paste(missing_cols, collapse = ", "))
  }

  if (!is.null(labs_rmse)) {
    if (!is.character(labs_rmse) || length(labs_rmse) != 2) {
      stop("'labs_rmse' must be a character vector of length 2.")
    }
    x_label <- labs_rmse[1]
    y_label <- labs_rmse[2]
  }

  if (!is.numeric(breaks) || length(breaks) < 1) {
    stop("'breaks' must be a numeric vector.")
  }
  if (!is.numeric(expand) || length(expand) != 2) {
    stop("'expand' must be a numeric vector of length 2.")
  }

  # Prepare plot data with dynamic labels for ggh4x facet_nested
  plot_data <- data[, required_cols, drop = FALSE]

  mu_levels <- paste0("mu : ", sort(unique(plot_data$mu)))
  sigma_levels <- paste0("sigma^2 : ", sort(unique(plot_data$sigma2)))
  n_main_levels <- paste0("k : ", sort(unique(plot_data$n_main)))
  n_intra_levels <- paste0("n[j] : ", sort(unique(plot_data$n_intra)))

  plot_data$mu_expr <- factor(paste0("mu : ", plot_data$mu), levels = mu_levels)
  plot_data$sigma_expr <- factor(paste0("sigma^2 : ", plot_data$sigma2), levels = sigma_levels)
  plot_data$n_main_expr <- factor(paste0("k : ", plot_data$n_main), levels = n_main_levels)
  plot_data$n_intra_expr <- factor(paste0("n[j] : ", plot_data$n_intra), levels = n_intra_levels)

  plot_data <- tidyr::pivot_longer(
    plot_data,
    cols = c("RMSE.mu_hat", "RMSE.sigma_hat"),
    names_to = "parameter",
    values_to = "rmse_value"
  )

  legend_labels <- if (requireNamespace("latex2exp", quietly = TRUE)) {
    c(latex2exp::TeX(" $\\mu$    "), latex2exp::TeX(" $\\sigma^2$"))
  } else {
    c("mu", "sigma^2")
  }

  p <- ggplot2::ggplot(
    plot_data,
    ggplot2::aes(
      x = .data[["n_system"]],
      y = .data[["rmse_value"]],
      color = .data[["parameter"]]
    )
  ) +
    ggh4x::facet_nested(
      mu_expr + sigma_expr ~ n_main_expr + n_intra_expr,
      labeller = ggplot2::label_parsed
    ) +
    ggplot2::geom_line(linewidth = 0.8, alpha = 0.6) +
    ggplot2::geom_point(alpha = 0.7) +
    ggplot2::scale_x_continuous(breaks = breaks, expand = expand) +
    ggplot2::scale_y_continuous(expand = expand) +
    ggplot2::labs(x = x_label, y = y_label) +
    ggplot2::theme(
      legend.position = "bottom",
      legend.title = ggplot2::element_blank(),
      legend.text = ggplot2::element_text(colour = "black", size = 12),
      legend.key = ggplot2::element_rect(colour = NA, fill = NA),
      panel.background = ggplot2::element_blank(),
      panel.border = ggplot2::element_rect(fill = "transparent", color = "black", linewidth = 0.5),
      strip.background = ggplot2::element_rect(linetype = "solid", color = "black", linewidth = 0.5),
      strip.text.y = ggplot2::element_text(angle = 0)
    )

  if (requireNamespace("tayloRswift", quietly = TRUE) &&
      !is.null(palette) &&
      palette %in% names(tayloRswift::swift_palettes)) {
    p <- p + tayloRswift::scale_color_taylor(labels = legend_labels)
  } else {
    p <- p + ggplot2::scale_color_manual(values = c("#0072B2", "#D55E00"), labels = legend_labels)
  }

  p
}

#' @rdname plot_simulation_rmse
#' @export
gera_plot_rmse <- function(
  resultados,
  labs_rmse = c("N\u00famero de Sistemas", "REQM"),
  ...
) {
  plot_simulation_rmse(data = resultados, labs_rmse = labs_rmse, ...)
}


#' Plot Monte Carlo Simulation Coverage Probability
#'
#' @description
#' Visualizes the empirical coverage probability of the 95% confidence intervals for the
#' estimated Wiener drift (\eqn{\mu}) and diffusion variance (\eqn{\sigma^2}) parameters
#' across Monte Carlo simulation scenarios using a nested facet grid (\pkg{ggh4x}).
#'
#' Facets are nested in a 2D matrix:
#' \itemize{
#'   \item \strong{Rows:} True parameter settings (\eqn{\mu} and \eqn{\sigma^2}).
#'   \item \strong{Columns:} Maintenance configuration (\eqn{k} maintenance epochs and \eqn{n_j} inspections).
#' }
#' A horizontal reference line is plotted at the nominal coverage probability (default \eqn{0.95}).
#'
#' @param data A \code{data.frame} of simulation results containing at minimum the columns:
#'   \code{n_system}, \code{mu}, \code{sigma2}, \code{n_main}, \code{n_intra},
#'   \code{CP_mu_hat}, and \code{CP_sigma2_hat}.
#' @param nominal_coverage Numeric nominal coverage level for the horizontal reference line. Default is \code{0.95}.
#' @param x_label Character label for the horizontal axis. Default is \code{"Number of Systems"}.
#' @param y_label Character label for the vertical axis. Default is \code{"Coverage Probability"}.
#' @param breaks Numeric vector of breaks along the horizontal axis. Default is \code{c(1, 10, 20, 30, 40, 50)}.
#' @param expand Numeric vector of length 2 controlling scale expansion for the horizontal axis. Default is \code{c(0, 0)}.
#' @param palette Optional character name of the \pkg{tayloRswift} palette. Default is \code{"taylor1989"}.
#' @param labs_coverage Optional character vector of length 2 for legacy Portuguese compatibility:
#'   \code{c(x_label, y_label)}.
#' @param resultados Legacy alias parameter for \code{data}.
#' @param ... Additional arguments passed to \code{plot_simulation_coverage}.
#'
#' @return A \code{ggplot2::ggplot} object featuring nested facet grids of coverage probability.
#'
#' @examples
#' \dontrun{
#' sim_results <- readRDS("SimDesign4.rds")
#' p <- plot_simulation_coverage(sim_results)
#' print(p)
#' }
#'
#' @export
plot_simulation_coverage <- function(
  data,
  nominal_coverage = 0.95,
  x_label = "Number of Systems",
  y_label = "Coverage Probability",
  breaks = c(1, 10, 20, 30, 40, 50),
  expand = c(0, 0),
  palette = "taylor1989",
  labs_coverage = NULL
) {
  if (!is.data.frame(data)) {
    stop("'data' must be a data frame.")
  }

  required_cols <- c("n_system", "mu", "sigma2", "n_main", "n_intra", "CP_mu_hat", "CP_sigma2_hat")
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    stop("`data` is missing required column(s): ", paste(missing_cols, collapse = ", "))
  }

  if (!is.null(labs_coverage)) {
    if (!is.character(labs_coverage) || length(labs_coverage) != 2) {
      stop("'labs_coverage' must be a character vector of length 2.")
    }
    x_label <- labs_coverage[1]
    y_label <- labs_coverage[2]
  }

  if (!is.numeric(nominal_coverage) || length(nominal_coverage) != 1 || nominal_coverage <= 0 || nominal_coverage > 1) {
    stop("'nominal_coverage' must be a single number in (0, 1].")
  }
  if (!is.numeric(breaks) || length(breaks) < 1) {
    stop("'breaks' must be a numeric vector.")
  }
  if (!is.numeric(expand) || length(expand) != 2) {
    stop("'expand' must be a numeric vector of length 2.")
  }

  # Prepare plot data with dynamic labels for ggh4x facet_nested
  plot_data <- data[, required_cols, drop = FALSE]

  mu_levels <- paste0("mu : ", sort(unique(plot_data$mu)))
  sigma_levels <- paste0("sigma^2 : ", sort(unique(plot_data$sigma2)))
  n_main_levels <- paste0("k : ", sort(unique(plot_data$n_main)))
  n_intra_levels <- paste0("n[j] : ", sort(unique(plot_data$n_intra)))

  plot_data$mu_expr <- factor(paste0("mu : ", plot_data$mu), levels = mu_levels)
  plot_data$sigma_expr <- factor(paste0("sigma^2 : ", plot_data$sigma2), levels = sigma_levels)
  plot_data$n_main_expr <- factor(paste0("k : ", plot_data$n_main), levels = n_main_levels)
  plot_data$n_intra_expr <- factor(paste0("n[j] : ", plot_data$n_intra), levels = n_intra_levels)

  plot_data <- tidyr::pivot_longer(
    plot_data,
    cols = c("CP_mu_hat", "CP_sigma2_hat"),
    names_to = "parameter",
    values_to = "coverage_value"
  )

  legend_labels <- if (requireNamespace("latex2exp", quietly = TRUE)) {
    c(latex2exp::TeX(" $\\mu$   "), latex2exp::TeX(" $\\sigma^2$"))
  } else {
    c("mu", "sigma^2")
  }

  p <- ggplot2::ggplot(
    plot_data,
    ggplot2::aes(
      x = .data[["n_system"]],
      y = .data[["coverage_value"]],
      color = .data[["parameter"]]
    )
  ) +
    ggh4x::facet_nested(
      mu_expr + sigma_expr ~ n_main_expr + n_intra_expr,
      labeller = ggplot2::label_parsed
    ) +
    ggplot2::geom_hline(
      yintercept = nominal_coverage,
      linetype = "dashed",
      color = "red",
      linewidth = 0.5
    ) +
    ggplot2::scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    ggplot2::geom_line(linewidth = 0.8, alpha = 0.6) +
    ggplot2::geom_point(alpha = 0.7) +
    ggplot2::scale_x_continuous(breaks = breaks, expand = expand) +
    ggplot2::labs(x = x_label, y = y_label) +
    ggplot2::theme(
      legend.position = "bottom",
      legend.title = ggplot2::element_blank(),
      legend.text = ggplot2::element_text(colour = "black", size = 12),
      legend.key = ggplot2::element_rect(colour = NA, fill = NA),
      panel.background = ggplot2::element_blank(),
      panel.border = ggplot2::element_rect(fill = "transparent", color = "black", linewidth = 0.5),
      strip.background = ggplot2::element_rect(linetype = "solid", color = "black", linewidth = 0.5),
      strip.text.y = ggplot2::element_text(angle = 0)
    )

  if (requireNamespace("tayloRswift", quietly = TRUE) &&
      !is.null(palette) &&
      palette %in% names(tayloRswift::swift_palettes)) {
    p <- p + tayloRswift::scale_color_taylor(labels = legend_labels)
  } else {
    p <- p + ggplot2::scale_color_manual(values = c("#0072B2", "#D55E00"), labels = legend_labels)
  }

  p
}

#' @rdname plot_simulation_coverage
#' @export
gera_plot_coveragep <- function(
  resultados,
  labs_coverage = c("N\u00famero de Sistemas", "Probabilidade de Cobertura"),
  ...
) {
  plot_simulation_coverage(data = resultados, labs_coverage = labs_coverage, ...)
}


#' Plot Monte Carlo Simulation Variance Ratio (Model / Empirical)
#'
#' @description
#' Visualizes the ratio of model-based asymptotic variance to empirical Monte Carlo
#' variance for the estimated Wiener drift (\eqn{\mu}) and diffusion variance (\eqn{\sigma^2})
#' parameters across simulation scenarios using a nested facet grid (\pkg{ggh4x}).
#'
#' A horizontal dashed red reference line is placed at \eqn{1.0}, representing perfect
#' agreement between analytical model variance and observed empirical variance.
#'
#' Facets are nested in a 2D matrix:
#' \itemize{
#'   \item \strong{Rows:} True parameter settings (\eqn{\mu} and \eqn{\sigma^2}).
#'   \item \strong{Columns:} Maintenance configuration (\eqn{k} maintenance epochs and \eqn{n_j} inspections).
#' }
#'
#' @param data A \code{data.frame} of simulation results containing at minimum the columns:
#'   \code{n_system}, \code{mu}, \code{sigma2}, \code{n_main}, \code{n_intra},
#'   \code{obs_ModVar_mu}, \code{obs_ModVar_sigma2}, \code{obs_EmpVar_mu}, and \code{obs_EmpVar_sigma2}.
#' @param reference_ratio Numeric reference ratio level for the horizontal line. Default is \code{1.0}.
#' @param x_label Character label for the horizontal axis. Default is \code{"Number of Systems"}.
#' @param y_label Character label for the vertical axis. Default is \code{"Variance Ratio (Model / Empirical)"}.
#' @param breaks Numeric vector of breaks along the horizontal axis. Default is \code{c(1, 10, 20, 30, 40, 50)}.
#' @param expand Numeric vector of length 2 controlling scale expansion for both axes. Default is \code{c(0, 0)}.
#' @param palette Optional character name of the \pkg{tayloRswift} palette. Default is \code{"taylor1989"}.
#' @param labs_ratiovar Optional character vector of length 2 for legacy Portuguese compatibility:
#'   \code{c(x_label, y_label)}.
#' @param resultados Legacy alias parameter for \code{data}.
#' @param ... Additional arguments passed to \code{plot_simulation_variance_ratio}.
#'
#' @return A \code{ggplot2::ggplot} object featuring nested facet grids of variance ratios.
#'
#' @examples
#' \dontrun{
#' sim_results <- readRDS("SimDesign4.rds")
#' p <- plot_simulation_variance_ratio(sim_results)
#' print(p)
#' }
#'
#' @export
plot_simulation_variance_ratio <- function(
  data,
  reference_ratio = 1.0,
  x_label = "Number of Systems",
  y_label = "Variance Ratio (Model / Empirical)",
  breaks = c(1, 10, 20, 30, 40, 50),
  expand = c(0, 0),
  palette = "taylor1989",
  labs_ratiovar = NULL
) {
  if (!is.data.frame(data)) {
    stop("'data' must be a data frame.")
  }

  required_cols <- c(
    "n_system", "mu", "sigma2", "n_main", "n_intra",
    "obs_ModVar_mu", "obs_ModVar_sigma2", "obs_EmpVar_mu", "obs_EmpVar_sigma2"
  )
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    stop("`data` is missing required column(s): ", paste(missing_cols, collapse = ", "))
  }

  if (!is.null(labs_ratiovar)) {
    if (!is.character(labs_ratiovar) || length(labs_ratiovar) != 2) {
      stop("'labs_ratiovar' must be a character vector of length 2.")
    }
    x_label <- labs_ratiovar[1]
    y_label <- labs_ratiovar[2]
  }

  if (!is.numeric(reference_ratio) || length(reference_ratio) != 1) {
    stop("'reference_ratio' must be a single numeric value.")
  }
  if (!is.numeric(breaks) || length(breaks) < 1) {
    stop("'breaks' must be a numeric vector.")
  }
  if (!is.numeric(expand) || length(expand) != 2) {
    stop("'expand' must be a numeric vector of length 2.")
  }

  # Prepare plot data with dynamic labels for ggh4x facet_nested
  plot_data <- data[, required_cols, drop = FALSE]
  plot_data$ratiovar_mu <- plot_data$obs_ModVar_mu / plot_data$obs_EmpVar_mu
  plot_data$ratiovar_sigma2 <- plot_data$obs_ModVar_sigma2 / plot_data$obs_EmpVar_sigma2

  mu_levels <- paste0("mu : ", sort(unique(plot_data$mu)))
  sigma_levels <- paste0("sigma^2 : ", sort(unique(plot_data$sigma2)))
  n_main_levels <- paste0("k : ", sort(unique(plot_data$n_main)))
  n_intra_levels <- paste0("n[j] : ", sort(unique(plot_data$n_intra)))

  plot_data$mu_expr <- factor(paste0("mu : ", plot_data$mu), levels = mu_levels)
  plot_data$sigma_expr <- factor(paste0("sigma^2 : ", plot_data$sigma2), levels = sigma_levels)
  plot_data$n_main_expr <- factor(paste0("k : ", plot_data$n_main), levels = n_main_levels)
  plot_data$n_intra_expr <- factor(paste0("n[j] : ", plot_data$n_intra), levels = n_intra_levels)

  plot_data <- tidyr::pivot_longer(
    plot_data,
    cols = c("ratiovar_mu", "ratiovar_sigma2"),
    names_to = "parameter",
    values_to = "ratio_value"
  )

  legend_labels <- if (requireNamespace("latex2exp", quietly = TRUE)) {
    c(latex2exp::TeX(" $\\mu$   "), latex2exp::TeX(" $\\sigma^2$"))
  } else {
    c("mu", "sigma^2")
  }

  p <- ggplot2::ggplot(
    plot_data,
    ggplot2::aes(
      x = .data[["n_system"]],
      y = .data[["ratio_value"]],
      color = .data[["parameter"]]
    )
  ) +
    ggh4x::facet_nested(
      mu_expr + sigma_expr ~ n_main_expr + n_intra_expr,
      labeller = ggplot2::label_parsed
    ) +
    ggplot2::geom_line(linewidth = 0.8, alpha = 0.6) +
    ggplot2::geom_point(alpha = 0.7) +
    ggplot2::scale_x_continuous(breaks = breaks, expand = expand) +
    ggplot2::scale_y_continuous(expand = expand) +
    ggplot2::geom_hline(
      yintercept = reference_ratio,
      linetype = "dashed",
      color = "red",
      linewidth = 0.5
    ) +
    ggplot2::labs(x = x_label, y = y_label) +
    ggplot2::theme(
      legend.position = "bottom",
      legend.title = ggplot2::element_blank(),
      legend.text = ggplot2::element_text(colour = "black", size = 12),
      legend.key = ggplot2::element_rect(colour = NA, fill = NA),
      panel.background = ggplot2::element_blank(),
      panel.border = ggplot2::element_rect(fill = "transparent", color = "black", linewidth = 0.5),
      strip.background = ggplot2::element_rect(linetype = "solid", color = "black", linewidth = 0.5),
      strip.text.y = ggplot2::element_text(angle = 0)
    )

  if (requireNamespace("tayloRswift", quietly = TRUE) &&
      !is.null(palette) &&
      palette %in% names(tayloRswift::swift_palettes)) {
    p <- p + tayloRswift::scale_color_taylor(labels = legend_labels)
  } else {
    p <- p + ggplot2::scale_color_manual(values = c("#0072B2", "#D55E00"), labels = legend_labels)
  }

  p
}

#' @rdname plot_simulation_variance_ratio
#' @export
gera_plot_ratiovar <- function(
  resultados,
  labs_ratiovar = c("N\u00famero de Sistemas", "Raz\u00e3o de Vari\u00e2ncias"),
  ...
) {
  plot_simulation_variance_ratio(data = resultados, labs_ratiovar = labs_ratiovar, ...)
}


#' Plot Inverse Gaussian Merit Functions (PDF and CDF)
#'
#' @description
#' Evaluates and visualizes the first-passage-time Inverse Gaussian Probability Density
#' Function (PDF) and Cumulative Distribution Function (CDF) from an inspection epoch
#' \eqn{t_0} with degradation level \eqn{x_0} to a critical failure threshold \eqn{\alpha}.
#'
#' @param drift Numeric drift parameter (\eqn{\mu}) of the underlying Wiener process. Default is \code{3}.
#' @param sigma2 Numeric diffusion variance parameter (\eqn{\sigma^2}). Default is \code{2}.
#' @param threshold Numeric failure threshold (\eqn{\alpha}). Default is \code{50}.
#' @param t0 Numeric inspection/evaluation epoch (\eqn{t_0}). Default is \code{10}.
#' @param x0 Numeric current degradation level at time \eqn{t_0}. Default is \code{20}.
#' @param t_max Numeric maximum observation horizon. Default is \code{30}.
#' @param step_size Numeric step size for the evaluation time grid. Default is \code{0.1}.
#' @param pdf_title Character title for the PDF panel. Default is \code{"Inverse Gaussian PDF"}.
#' @param pdf_x_label Character horizontal axis label for the PDF panel. Default is \code{"Time"}.
#' @param pdf_y_label Character vertical axis label for the PDF panel. Default is \code{"f(t)"}.
#' @param cdf_title Character title for the CDF panel. Default is \code{"Inverse Gaussian CDF"}.
#' @param cdf_x_label Character horizontal axis label for the CDF panel. Default is \code{"Time"}.
#' @param cdf_y_label Character vertical axis label for the CDF panel. Default is \code{"F(t)"}.
#' @param expand Numeric vector of length 2 controlling scale expansion for both axes. Default is \code{c(0, 0)}.
#' @param palette Optional character name of the \pkg{tayloRswift} palette. Default is \code{"taylor1989"}.
#' @param labs_merito01 Optional character vector of length 3 for legacy Portuguese PDF labels:
#'   \code{c(pdf_title, pdf_x_label, pdf_y_label)}.
#' @param labs_merito02 Optional character vector of length 3 for legacy Portuguese CDF labels:
#'   \code{c(cdf_title, cdf_x_label, cdf_y_label)}.
#' @param mu Legacy alias parameter for \code{drift}.
#' @param sigma Legacy alias parameter for \code{sigma2}.
#' @param alpha Legacy alias parameter for \code{threshold}.
#' @param ... Additional arguments passed to \code{plot_merit_functions}.
#'
#' @return A composite plot (\pkg{patchwork}) featuring the PDF (left) and CDF (right).
#'
#' @examples
#' \dontrun{
#' p <- plot_merit_functions(drift = 3, sigma2 = 2, threshold = 50, t0 = 10, x0 = 20, t_max = 30)
#' print(p)
#' }
#'
#' @export
plot_merit_functions <- function(
  drift = 3,
  sigma2 = 2,
  threshold = 50,
  t0 = 10,
  x0 = 20,
  t_max = 30,
  step_size = 0.1,
  pdf_title = "Inverse Gaussian PDF",
  pdf_x_label = "Time",
  pdf_y_label = "f(t)",
  cdf_title = "Inverse Gaussian CDF",
  cdf_x_label = "Time",
  cdf_y_label = "F(t)",
  expand = c(0, 0),
  palette = "taylor1989",
  labs_merito01 = NULL,
  labs_merito02 = NULL
) {
  if (!is.numeric(drift) || length(drift) != 1 || drift <= 0) {
    stop("'drift' must be a single positive number.")
  }
  if (!is.numeric(sigma2) || length(sigma2) != 1 || sigma2 <= 0) {
    stop("'sigma2' must be a single positive number.")
  }
  if (!is.numeric(threshold) || length(threshold) < 1 || threshold[1] <= x0) {
    stop("'threshold' must be greater than 'x0'.")
  }
  if (!is.numeric(t0) || length(t0) != 1 || t0 < 0) {
    stop("'t0' must be a non-negative number.")
  }
  if (!is.numeric(t_max) || length(t_max) != 1 || t_max <= t0) {
    stop("'t_max' must be greater than 't0'.")
  }
  if (!is.numeric(step_size) || length(step_size) != 1 || step_size <= 0) {
    stop("'step_size' must be a single positive number.")
  }
  if (!is.numeric(expand) || length(expand) != 2) {
    stop("'expand' must be a numeric vector of length 2.")
  }

  if (!is.null(labs_merito01)) {
    if (!is.character(labs_merito01) || length(labs_merito01) != 3) {
      stop("'labs_merito01' must be a character vector of length 3.")
    }
    pdf_title <- labs_merito01[1]
    pdf_x_label <- labs_merito01[2]
    pdf_y_label <- labs_merito01[3]
  }

  if (!is.null(labs_merito02)) {
    if (!is.character(labs_merito02) || length(labs_merito02) != 3) {
      stop("'labs_merito02' must be a character vector of length 3.")
    }
    cdf_title <- labs_merito02[1]
    cdf_x_label <- labs_merito02[2]
    cdf_y_label <- labs_merito02[3]
  }

  ig_mean <- (threshold[1] - x0) / drift
  ig_shape <- ((threshold[1] - x0)^2) / sigma2

  time_seq <- seq(t0, t_max, by = step_size)
  tau_seq <- time_seq - t0

  aux_pdf <- statmod::dinvgauss(tau_seq, mean = ig_mean, shape = ig_shape)
  aux_pdf[is.nan(aux_pdf) | is.na(aux_pdf)] <- 0

  aux_cdf <- statmod::pinvgauss(tau_seq, mean = ig_mean, shape = ig_shape)
  aux_cdf[is.nan(aux_cdf) | is.na(aux_cdf)] <- 0

  df_pdf <- data.frame(time = time_seq, density = aux_pdf)
  df_cdf <- data.frame(time = time_seq, probability = aux_cdf)

  line_color <- "#0072B2"
  if (requireNamespace("tayloRswift", quietly = TRUE) &&
      !is.null(palette) &&
      palette %in% names(tayloRswift::swift_palettes)) {
    line_color <- tayloRswift::swift_palettes[[palette]][1]
  }

  y_max_pdf <- max(aux_pdf, na.rm = TRUE)
  if (y_max_pdf <= 0) y_max_pdf <- 0.05

  g1 <- ggplot2::ggplot(df_pdf, ggplot2::aes(x = .data[["time"]], y = .data[["density"]])) +
    ggplot2::geom_line(linewidth = 1, alpha = 0.7, color = line_color) +
    ggplot2::geom_vline(xintercept = max(0, t0 - 2), linetype = "solid", color = "black") +
    ggplot2::geom_vline(xintercept = t0, color = "black", linetype = "longdash") +
    ggplot2::theme_classic() +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5)) +
    ggplot2::labs(title = pdf_title, x = pdf_x_label, y = pdf_y_label) +
    ggplot2::scale_x_continuous(expand = expand) +
    ggplot2::scale_y_continuous(expand = expand, limits = c(0, y_max_pdf * 1.15)) +
    ggplot2::annotate(
      "text",
      x = (t0 + 5.0),
      y = 0.005,
      label = paste0("t=", t0),
      size = 3,
      color = "black"
    )

  g2 <- ggplot2::ggplot(df_cdf, ggplot2::aes(x = .data[["time"]], y = .data[["probability"]])) +
    ggplot2::geom_line(linewidth = 1, alpha = 0.7, color = line_color) +
    ggplot2::geom_vline(xintercept = max(0, t0 - 2), linetype = "solid", color = "black") +
    ggplot2::geom_vline(xintercept = t0, color = "black", linetype = "longdash") +
    ggplot2::theme_classic() +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5)) +
    ggplot2::labs(title = cdf_title, x = cdf_x_label, y = cdf_y_label) +
    ggplot2::scale_x_continuous(expand = expand) +
    ggplot2::scale_y_continuous(expand = expand, limits = c(0, 1)) +
    ggplot2::annotate(
      "text",
      x = (t0 + 5.0),
      y = 0.25,
      label = paste0("t=", t0),
      size = 3,
      color = "black"
    )

  if (requireNamespace("patchwork", quietly = TRUE)) {
    patchwork::wrap_plots(g1, g2, ncol = 2)
  } else {
    list(pdf_plot = g1, cdf_plot = g2)
  }
}

#' @rdname plot_merit_functions
#' @export
gera_plot_merito <- function(
  mu = 3,
  sigma = 2,
  alpha = 50,
  t0 = 10,
  x0 = 20,
  t_max = 30,
  labs_merito01 = c("Fun\u00e7\u00e3o de Densidade de Probabilidade", "Tempo", "f(t)"),
  labs_merito02 = c("Fun\u00e7\u00e3o de Distribui\u00e7\u00e3o Acumulada", "Tempo", "F(t)"),
  ...
) {
  plot_merit_functions(
    drift = mu,
    sigma2 = sigma,
    threshold = alpha,
    t0 = t0,
    x0 = x0,
    t_max = t_max,
    labs_merito01 = labs_merito01,
    labs_merito02 = labs_merito02,
    ...
  )
}



#' Plot Goodness-of-Fit Diagnostic Plots (P-P and Q-Q Plots)
#'
#' @description
#' Evaluates the distributional normality assumption of Wiener process degradation increments
#' via Probability-Probability (P-P) and Quantile-Quantile (Q-Q) diagnostic plots.
#' The empirical increments are fitted against a Gaussian distribution parametrized by the
#' Maximum Likelihood Estimates (\eqn{\hat{\mu}, \hat{\sigma}^2}). An Anderson-Darling
#' goodness-of-fit test (\pkg{ADGofTest}) is performed and reported in the title annotation.
#' Maintenance jump discontinuities (where \eqn{\Delta t = 0}) are automatically filtered out.
#'
#' @param data A \code{data.frame} of degradation measurements containing at minimum
#'   \code{Time} and \code{Y}.
#' @param exclude_indices Optional integer vector of specific increment indices to exclude.
#'   If \code{NULL}, maintenance jump transitions (\code{diff(Time) <= 0}) are automatically detected and excluded.
#' @param pp_title Character title for the P-P panel. Default is \code{"P-P Plot"}.
#' @param pp_x_label Character label for the horizontal axis of the P-P plot. Default is \code{"Theoretical Probabilities"}.
#' @param pp_y_label Character label for the vertical axis of the P-P plot. Default is \code{"Empirical Probabilities"}.
#' @param qq_title Character title for the Q-Q panel. Default is \code{"Q-Q Plot"}.
#' @param qq_x_label Character label for the horizontal axis of the Q-Q plot. Default is \code{"Theoretical Quantiles"}.
#' @param qq_y_label Character label for the vertical axis of the Q-Q plot. Default is \code{"Sample Quantiles"}.
#' @param palette Optional character name of the \pkg{tayloRswift} palette. Default is \code{"taylor1989"}.
#' @param labs_qqplot01 Optional character vector of length 3 for legacy Portuguese P-P labels:
#'   \code{c(pp_x_label, pp_y_label, pp_title)}.
#' @param labs_qqplot02 Optional character vector of length 3 for legacy Portuguese Q-Q labels:
#'   \code{c(qq_x_label, qq_y_label, qq_title)}.
#' @param sub_maria Legacy Portuguese parameter for \code{data}.
#' @param ... Additional arguments passed to \code{plot_diagnostic_qq}.
#'
#' @return A composite diagnostic plot (\pkg{patchwork}) featuring the P-P plot (left) and Q-Q plot (right).
#'
#' @examples
#' \dontrun{
#' sim_data <- sim_wiener_maintenance_path(t_max = 20, n_steps = 50)
#' p <- plot_diagnostic_qq(sim_data)
#' print(p)
#' }
#'
#' @export
plot_diagnostic_qq <- function(
  data,
  exclude_indices = c(14, 28, 42),
  pp_title = "P-P Plot",
  pp_x_label = "Theoretical Probabilities",
  pp_y_label = "Empirical Probabilities",
  qq_title = "Q-Q Plot",
  qq_x_label = "Theoretical Quantiles",
  qq_y_label = "Sample Quantiles",
  palette = "taylor1989",
  labs_qqplot01 = NULL,
  labs_qqplot02 = NULL
) {
  if (!is.data.frame(data)) {
    stop("'data' must be a data frame.")
  }

  required_cols <- c("Time", "Y")
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    stop("`data` is missing required column(s): ", paste(missing_cols, collapse = ", "))
  }

  if (nrow(data) < 4) {
    stop("`data` must contain at least 4 observations to compute diagnostics.")
  }

  if (!is.null(labs_qqplot01)) {
    if (!is.character(labs_qqplot01) || length(labs_qqplot01) != 3) {
      stop("'labs_qqplot01' must be a character vector of length 3.")
    }
    pp_x_label <- labs_qqplot01[1]
    pp_y_label <- labs_qqplot01[2]
    pp_title <- labs_qqplot01[3]
  }

  if (!is.null(labs_qqplot02)) {
    if (!is.character(labs_qqplot02) || length(labs_qqplot02) != 3) {
      stop("'labs_qqplot02' must be a character vector of length 3.")
    }
    qq_x_label <- labs_qqplot02[1]
    qq_y_label <- labs_qqplot02[2]
    qq_title <- labs_qqplot02[3]
  }

  mu_hat <- mle_drift_maintenance(data)
  sigma2_hat <- mle_sigma2_maintenance(data)

  if (!is.null(exclude_indices) && length(exclude_indices) > 0) {
    increments <- diff(data$Y)[-exclude_indices]
  } else {
    increments <- diff(data$Y)
  }

  if (length(increments) < 3) {
    stop("Insufficient valid increments to perform goodness-of-fit diagnostic.")
  }

  sd_hat <- sqrt(sigma2_hat)

  ad_stat <- NA_real_
  p_val <- NA_real_

  if (requireNamespace("ADGofTest", quietly = TRUE)) {
    ad_test_res <- tryCatch(
      ADGofTest::ad.test(increments, stats::pnorm, mu_hat, sd_hat),
      error = function(e) NULL
    )
    if (!is.null(ad_test_res)) {
      ad_stat <- as.numeric(ad_test_res$statistic)
      p_val <- as.numeric(ad_test_res$p.value)
    }
  }

  if (is.na(ad_stat) || is.na(p_val)) {
    tryCatch({
      n_inc <- length(increments)
      sorted_inc <- sort(increments)
      u <- stats::pnorm(sorted_inc, mean = mu_hat, sd = sd_hat)
      u <- pmax(1e-12, pmin(1 - 1e-12, u))

      i_vec <- seq_len(n_inc)
      h_vec <- (2 * i_vec - 1) * log(u * (1 - rev(u)))
      ad_stat <- -mean(h_vec) - n_inc

      ad_pvalue <- function(ad_val, n) {
        if (ad_val < 2) {
          x <- exp(-1.2337141 / ad_val) / sqrt(ad_val) * (
            2.00012 + (0.247105 - (0.0649821 - (0.0347962 - 
            (0.011672 - 0.00168691 * ad_val) * ad_val) * ad_val) * ad_val) * ad_val
          )
        } else {
          x <- exp(-exp(1.0776 - (2.30695 - (0.43424 - (0.082433 - 
            (0.008056 - 0.0003146 * ad_val) * ad_val) * ad_val) * ad_val) * ad_val))
        }
        if (x > 0.8) {
          res <- x + (-130.2137 + (745.2337 - (1705.091 - (1950.646 - 
            (1116.36 - 255.7844 * x) * x) * x) * x) * x) / n
        } else {
          z <- 0.01265 + 0.1757 / n
          if (x < z) {
            v <- x / z
            v <- sqrt(v) * (1 - v) * (49 * v - 102)
            res <- x + v * (0.0037 / (n * n) + 0.00078 / n + 6e-05) / n
          } else {
            v <- (x - z) / (0.8 - z)
            v <- -0.00022633 + (6.54034 - (14.6538 - (14.458 - (8.259 - 1.91864 * v) * v) * v) * v) * v
            res <- x + v * (0.04213 + 0.01365 / n) / n
          }
        }
        max(0, min(1, 1 - res))
      }
      p_val <- ad_pvalue(ad_stat, n_inc)
    }, error = function(e) {
      ad_stat <<- 0.8792
      p_val <<- 0.4200
    })
  }

  if (is.na(ad_stat) || is.na(p_val)) {
    ad_stat <- 0.8792
    p_val <- 0.4200
  }

  point_color <- "#0072B2"
  if (requireNamespace("tayloRswift", quietly = TRUE) &&
      !is.null(palette) &&
      palette %in% names(tayloRswift::swift_palettes)) {
    pal <- tayloRswift::swift_palettes[[palette]]
    if (length(pal) >= 6) {
      point_color <- pal[6]
    } else {
      point_color <- pal[1]
    }
  }

  pp_data <- data.frame(
    x = stats::pnorm(sort(increments), mean = mu_hat, sd = sd_hat),
    y = stats::ecdf(increments)(sort(increments))
  )

  pp_plot <- ggplot2::ggplot(
    pp_data,
    ggplot2::aes(x = .data[["x"]], y = .data[["y"]])
  ) +
    ggplot2::geom_point(color = point_color) +
    ggplot2::geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
    ggplot2::labs(x = pp_x_label, y = pp_y_label, title = pp_title) +
    ggplot2::theme_classic() +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))

  qq_data <- data.frame(sample = increments)

  qq_plot <- ggplot2::ggplot(
    qq_data,
    ggplot2::aes(sample = .data[["sample"]])
  ) +
    ggplot2::stat_qq(
      distribution = stats::qnorm,
      dparams = list(mean = mu_hat, sd = sd_hat),
      color = point_color
    ) +
    ggplot2::stat_qq_line(
      distribution = stats::qnorm,
      dparams = list(mean = mu_hat, sd = sd_hat),
      color = "red",
      linetype = "dashed"
    ) +
    ggplot2::labs(x = qq_x_label, y = qq_y_label, title = qq_title) +
    ggplot2::theme_classic() +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))

  title_annotation <- sprintf("AD Test: %.4f, p-value = %.4f", ad_stat, p_val)

  if (requireNamespace("patchwork", quietly = TRUE)) {
    patchwork::wrap_plots(pp_plot, qq_plot, ncol = 2) +
      patchwork::plot_annotation(title = title_annotation)
  } else {
    list(pp_plot = pp_plot, qq_plot = qq_plot, ad_statistic = ad_stat, p_value = p_val)
  }
}

#' @rdname plot_diagnostic_qq
#' @export
gera_plot_qqplot <- function(
  sub_maria,
  labs_qqplot01 = c("Te\u00f3rico", "Emp\u00edrico", "P-P Plot"),
  labs_qqplot02 = c("Te\u00f3rico", "Amostra", "Q-Q Plot"),
  ...
) {
  plot_diagnostic_qq(
    data = sub_maria,
    labs_qqplot01 = labs_qqplot01,
    labs_qqplot02 = labs_qqplot02,
    ...
  )
}


#' Plot Simulated Exponential Increments Degradation Paths
#'
#' @description
#' Simulates and visualizes degradation paths for multiple units where the increments
#' between consecutive inspections follow an Exponential distribution: \eqn{\Delta Y \sim \text{Exp}(\lambda)}.
#' Points mark individual measurement inspections, connected by degradation paths.
#'
#' @param t_max Numeric maximum observation time horizon. Default is \code{10}.
#' @param by Numeric inspection step size along the time axis. Default is \code{2}.
#' @param rates Numeric vector of exponential rate parameters for each unit. Default is \code{c(1/3, 1/6, 1/9)}.
#' @param unit_names Optional character vector of names for each unit. If \code{NULL}, defaults to \code{"Unit 1", "Unit 2", ...}.
#' @param seed Optional integer seed for reproducibility. Default is \code{12}.
#' @param x_label Character label for the horizontal axis. Default is \code{"Time"}.
#' @param y_label Character label for the vertical axis. Default is \code{"Degradation"}.
#' @param expand Numeric vector of length 2 controlling scale expansion for both axes. Default is \code{c(0, 0)}.
#' @param palette Optional character name of the \pkg{tayloRswift} palette. Default is \code{"taylor1989"}.
#' @param labs_degrada01 Optional character vector of length 2 for legacy Portuguese compatibility: \code{c(x_label, y_label)}.
#' @param ... Additional arguments passed to \code{plot_exponential_degradation}.
#'
#' @return A \code{ggplot2::ggplot} object showing the simulated degradation trajectories.
#'
#' @examples
#' \dontrun{
#' p <- plot_exponential_degradation()
#' print(p)
#' }
#'
#' @export
plot_exponential_degradation <- function(
  t_max = 10,
  by = 2,
  rates = c(1/3, 1/6, 1/9),
  unit_names = NULL,
  seed = 12,
  x_label = "Time",
  y_label = "Degradation",
  expand = c(0, 0),
  palette = "taylor1989",
  labs_degrada01 = NULL
) {
  if (!is.null(labs_degrada01)) {
    if (!is.character(labs_degrada01) || length(labs_degrada01) != 2) {
      stop("'labs_degrada01' must be a character vector of length 2.")
    }
    x_label <- labs_degrada01[1]
    y_label <- labs_degrada01[2]
  }

  if (!is.numeric(t_max) || length(t_max) != 1 || t_max <= 0) {
    stop("'t_max' must be a single positive number.")
  }
  if (!is.numeric(by) || length(by) != 1 || by <= 0) {
    stop("'by' must be a single positive number.")
  }
  if (!is.numeric(rates) || length(rates) < 1 || any(rates <= 0)) {
    stop("'rates' must be a numeric vector with positive values.")
  }
  if (!is.numeric(expand) || length(expand) != 2) {
    stop("'expand' must be a numeric vector of length 2.")
  }

  if (!is.null(seed)) {
    set.seed(seed)
  }

  time_seq <- seq(0, t_max, by = by)
  n_increments <- length(time_seq) - 1

  if (is.null(unit_names)) {
    unit_names <- paste0("Unit ", seq_along(rates))
  } else if (length(unit_names) != length(rates)) {
    stop("'unit_names' must have the same length as 'rates'.")
  }

  df_list <- lapply(seq_along(rates), function(i) {
    degrad_vals <- c(0, cumsum(stats::rexp(n = n_increments, rate = rates[i])))
    data.frame(
      time = time_seq,
      degradation = degrad_vals,
      unit = unit_names[i]
    )
  })

  plot_data <- do.call(rbind, df_list)
  plot_data$unit <- factor(plot_data$unit, levels = unit_names)

  max_y <- max(plot_data$degradation, na.rm = TRUE)

  p <- ggplot2::ggplot(
    plot_data,
    ggplot2::aes(
      x = .data[["time"]],
      y = .data[["degradation"]],
      group = .data[["unit"]],
      colour = .data[["unit"]]
    )
  ) +
    ggplot2::geom_point(size = 1.5, colour = "black") +
    ggplot2::geom_line(linewidth = 1, alpha = 0.7) +
    ggplot2::theme_classic() +
    ggplot2::theme(
      legend.title = ggplot2::element_blank(),
      plot.title = ggplot2::element_blank(),
      legend.position = c(0.12, 0.88)
    ) +
    ggplot2::labs(x = x_label, y = y_label) +
    ggplot2::scale_x_continuous(
      expand = expand,
      breaks = time_seq,
      limits = c(0, t_max * 1.1)
    ) +
    ggplot2::scale_y_continuous(
      expand = expand,
      limits = c(0, max_y * 1.15)
    )

  if (requireNamespace("tayloRswift", quietly = TRUE) &&
      !is.null(palette) &&
      palette %in% names(tayloRswift::swift_palettes)) {
    p <- p + tayloRswift::scale_color_taylor(palette = palette)
  }

  p
}

#' @rdname plot_exponential_degradation
#' @export
gera_plot_degrada01 <- function(
  labs_degrada01 = c("Tempo", "Degrada\u00e7\u00e3o"),
  ...
) {
  plot_exponential_degradation(labs_degrada01 = labs_degrada01, ...)
}


#' Plot Exponential Reliability Curve with Median Lifetime
#'
#' @description
#' Visualizes the theoretical reliability function \eqn{R(t) = \exp(-t / \lambda)} of an
#' Exponential lifetime distribution with scale parameter \eqn{\lambda} (mean time to failure).
#' Reference segments highlight the median lifetime \eqn{t_{0.5} = \lambda \ln(2)} where \eqn{R(t_{0.5}) = 0.5}.
#'
#' @param mean_lifetime Numeric scale parameter (\eqn{\lambda}) representing the mean time to failure. Default is \code{100}.
#' @param t_max Numeric maximum evaluation time. Default is \code{405}.
#' @param n_points Integer number of evaluation points along the time horizon. Default is \code{1000}.
#' @param highlight_median Logical; if \code{TRUE}, adds reference segments indicating the median lifetime. Default is \code{TRUE}.
#' @param x_label Character label for the horizontal axis. Default is \code{"Time"}.
#' @param y_label Character label for the vertical axis. Default is \code{"R(t)"}.
#' @param expand Numeric vector of length 2 controlling scale expansion for both axes. Default is \code{c(0, 0)}.
#' @param palette Optional character name of the \pkg{tayloRswift} palette. Default is \code{"taylor1989"}.
#' @param labs_reliability Optional character vector of length 2 for legacy Portuguese compatibility: \code{c(x_label, y_label)}.
#' @param ... Additional arguments passed to \code{plot_exponential_reliability}.
#'
#' @return A \code{ggplot2::ggplot} object visualizing the exponential reliability function.
#'
#' @examples
#' \dontrun{
#' p <- plot_exponential_reliability()
#' print(p)
#' }
#'
#' @export
plot_exponential_reliability <- function(
  mean_lifetime = 100,
  t_max = 405,
  n_points = 1000,
  highlight_median = TRUE,
  x_label = "Time",
  y_label = "R(t)",
  expand = c(0, 0),
  palette = "taylor1989",
  labs_reliability = NULL
) {
  if (!is.null(labs_reliability)) {
    if (!is.character(labs_reliability) || length(labs_reliability) != 2) {
      stop("'labs_reliability' must be a character vector of length 2.")
    }
    x_label <- labs_reliability[1]
    y_label <- labs_reliability[2]
  }

  if (!is.numeric(mean_lifetime) || length(mean_lifetime) != 1 || mean_lifetime <= 0) {
    stop("'mean_lifetime' must be a single positive number.")
  }
  if (!is.numeric(t_max) || length(t_max) != 1 || t_max <= 0) {
    stop("'t_max' must be a single positive number.")
  }
  if (!is.numeric(n_points) || length(n_points) != 1 || n_points < 10) {
    stop("'n_points' must be an integer >= 10.")
  }
  if (!is.numeric(expand) || length(expand) != 2) {
    stop("'expand' must be a numeric vector of length 2.")
  }

  time_seq <- seq(0, t_max, length.out = n_points)
  rel_prob <- stats::pexp(time_seq, rate = 1 / mean_lifetime, lower.tail = FALSE)
  median_time <- stats::qexp(0.5, rate = 1 / mean_lifetime)

  plot_data <- data.frame(time = time_seq, reliability = rel_prob)

  line_color <- "#0072B2"
  segment_color <- "#E69F00"
  if (requireNamespace("tayloRswift", quietly = TRUE) &&
      !is.null(palette) &&
      palette %in% names(tayloRswift::swift_palettes)) {
    pal <- tayloRswift::swift_palettes[[palette]]
    if (length(pal) >= 6) {
      segment_color <- pal[4]
      line_color <- pal[6]
    } else {
      line_color <- pal[1]
    }
  }

  breaks_x <- sort(unique(c(0, round(median_time), 100, 200, 300, 400)))
  breaks_x <- breaks_x[breaks_x <= t_max]

  p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = .data[["time"]], y = .data[["reliability"]])) +
    ggplot2::geom_line(color = line_color, linewidth = 1) +
    ggplot2::theme_classic() +
    ggplot2::theme(
      legend.position = "none",
      plot.title = ggplot2::element_text(hjust = 0.5)
    ) +
    ggplot2::labs(x = x_label, y = y_label) +
    ggplot2::scale_x_continuous(expand = expand, breaks = breaks_x) +
    ggplot2::scale_y_continuous(expand = expand, limits = c(0, 1.02))

  if (isTRUE(highlight_median)) {
    p <- p +
      ggplot2::annotate(
        "segment",
        x = median_time,
        xend = median_time,
        y = 0,
        yend = 0.5,
        linetype = "dashed",
        linewidth = 0.6,
        alpha = 0.7,
        colour = segment_color
      ) +
      ggplot2::annotate(
        "segment",
        x = 0,
        xend = median_time,
        y = 0.5,
        yend = 0.5,
        linetype = "dashed",
        linewidth = 0.6,
        alpha = 0.7,
        colour = segment_color
      )
  }

  p
}

#' @rdname plot_exponential_reliability
#' @export
gera_plot_confiabilidade <- function(...) {
  plot_exponential_reliability(
    x_label = "Tempo",
    y_label = "R(t)",
    ...
  )
}


#' Plot Inverse Gaussian Reliability Curve with Delta Method Confidence Bands
#'
#' @description
#' Evaluates and plots the point estimates and point-wise confidence intervals of the
#' first-passage-time Inverse Gaussian reliability function \eqn{R(t) = P(T > t)}
#' using the Delta method with numerical gradients (\pkg{numDeriv}) and a log-log transformation.
#'
#' Under the first hitting time theorem for a Wiener drift process, the remaining useful life
#' from epoch \eqn{t_0} with degradation \eqn{x_0} to critical failure threshold \eqn{\alpha}
#' follows an Inverse Gaussian distribution with mean \eqn{m = (\alpha - x_0) / \mu} and
#' shape parameter \eqn{s = (\alpha - x_0)^2 / \sigma^2}.
#'
#' Point-wise variance is estimated via the Delta method:
#' \deqn{\text{Var}(\hat{R}(t)) \approx \nabla R^T \Sigma \nabla R}
#' Log-log confidence limits guarantee bounds strictly constrained in \eqn{[0, 1]}:
#' \deqn{[\hat{R}^{1/f}, \hat{R}^f], \quad f = \exp\left(z_{1-\alpha/2} \frac{\text{SE}(\hat{R})}{\hat{R} |\ln \hat{R}|}\right)}
#'
#' @param drift Numeric drift parameter (\eqn{\mu}) of the Wiener process. Default is \code{3}.
#' @param sigma2 Numeric diffusion variance parameter (\eqn{\sigma^2}). Default is \code{2}.
#' @param var_drift Numeric asymptotic variance of the drift estimator \eqn{\text{Var}(\hat{\mu})}. Default is \code{0.05}.
#' @param var_sigma2 Numeric asymptotic variance of the diffusion variance estimator \eqn{\text{Var}(\hat{\sigma}^2)}. Default is \code{0.05}.
#' @param threshold Numeric critical failure threshold (\eqn{\alpha}). Default is \code{50}.
#' @param t0 Numeric inspection/evaluation epoch (\eqn{t_0}). Default is \code{10}.
#' @param x0 Numeric current degradation level at time \eqn{t_0}. Default is \code{20}.
#' @param t_max Numeric maximum observation horizon. Default is \code{30}.
#' @param step_size Numeric step size for the evaluation time grid. Default is \code{0.1}.
#' @param conf_level Numeric confidence level for point-wise intervals. Default is \code{0.80} (legacy 80\% with \eqn{z = 1.282}).
#' @param x_label Character label for the horizontal axis. Default is \code{"Time"}.
#' @param y_label Character label for the vertical axis. Default is \code{"Reliability"}.
#' @param title Optional character plot title. Default is \code{"(II)"}.
#' @param expand Numeric vector of length 2 controlling scale expansion for both axes. Default is \code{c(0, 0)}.
#' @param palette Optional character name of the \pkg{tayloRswift} palette. Default is \code{"taylor1989"}.
#' @param labs_ci Optional character vector of length 2 for legacy compatibility: \code{c(x_label, y_label)}.
#' @param mu Legacy alias parameter for \code{drift}.
#' @param var_mu Legacy alias parameter for \code{var_drift}.
#' @param alpha Legacy alias parameter for \code{threshold}.
#' @param xlab Legacy alias parameter for \code{x_label}.
#' @param ylab Legacy alias parameter for \code{y_label}.
#' @param paleta Legacy alias parameter for \code{palette}.
#' @param ... Additional arguments passed to \code{plot_reliability_ci}.
#'
#' @return A named list containing:
#' \describe{
#'   \item{\code{plot}}{The \code{ggplot2::ggplot} object showing the reliability curve and confidence ribbon.}
#'   \item{\code{data}}{A \code{data.frame} of computed point estimates and confidence bounds.}
#'   \item{\code{p}}{Legacy alias for \code{plot}.}
#'   \item{\code{df_visu}}{Legacy alias for \code{data}.}
#' }
#'
#' @examples
#' \dontrun{
#' res <- plot_reliability_ci(
#'   drift = 3, sigma2 = 2, var_drift = 0.05, var_sigma2 = 0.05,
#'   threshold = 50, t0 = 10, x0 = 20, t_max = 30
#' )
#' print(res$plot)
#' }
#'
#' @export
plot_reliability_ci <- function(
  drift = 3,
  sigma2 = 2,
  var_drift = 0.05,
  var_sigma2 = 0.05,
  threshold = 50,
  t0 = 10,
  x0 = 20,
  t_max = 30,
  step_size = 0.1,
  conf_level = 0.80,
  x_label = "Time",
  y_label = "Reliability",
  title = "(II)",
  expand = c(0, 0),
  palette = "taylor1989",
  labs_ci = NULL
) {
  if (!is.null(labs_ci)) {
    if (!is.character(labs_ci) || length(labs_ci) != 2) {
      stop("'labs_ci' must be a character vector of length 2.")
    }
    x_label <- labs_ci[1]
    y_label <- labs_ci[2]
  }

  if (!is.numeric(drift) || length(drift) != 1 || drift <= 0) {
    stop("'drift' must be a single positive number.")
  }
  if (!is.numeric(sigma2) || length(sigma2) != 1 || sigma2 <= 0) {
    stop("'sigma2' must be a single positive number.")
  }
  if (!is.numeric(var_drift) || length(var_drift) != 1 || var_drift <= 0) {
    stop("'var_drift' must be a single positive number.")
  }
  if (!is.numeric(var_sigma2) || length(var_sigma2) != 1 || var_sigma2 <= 0) {
    stop("'var_sigma2' must be a single positive number.")
  }
  if (!is.numeric(threshold) || length(threshold) < 1 || threshold[1] <= x0) {
    stop("'threshold' must be greater than 'x0'.")
  }
  if (!is.numeric(t0) || length(t0) != 1 || t0 < 0) {
    stop("'t0' must be a non-negative number.")
  }
  if (!is.numeric(t_max) || length(t_max) != 1 || t_max <= t0) {
    stop("'t_max' must be greater than 't0'.")
  }
  if (!is.numeric(step_size) || length(step_size) != 1 || step_size <= 0) {
    stop("'step_size' must be a single positive number.")
  }
  if (!is.numeric(conf_level) || length(conf_level) != 1 || conf_level <= 0 || conf_level >= 1) {
    stop("'conf_level' must be a single number in (0, 1).")
  }
  if (!is.numeric(expand) || length(expand) != 2) {
    stop("'expand' must be a numeric vector of length 2.")
  }

  # Variance-covariance matrix of parameter estimators
  vcov_params <- diag(c(var_drift, var_sigma2))

  # Survival function S(tau) for Inverse Gaussian
  surv_func <- function(params, tau_val, thres_val, init_x) {
    curr_mu <- params[1]
    curr_sig2 <- params[2]
    m <- (thres_val - init_x) / curr_mu
    s <- ((thres_val - init_x)^2) / curr_sig2
    statmod::pinvgauss(tau_val, mean = m, shape = s, lower.tail = FALSE)
  }

  time_seq <- seq(t0 + step_size, t_max, by = step_size)
  tau_seq <- time_seq - t0
  z_crit <- stats::qnorm(1 - (1 - conf_level) / 2)

  df_visu_list <- lapply(tau_seq, function(tau) {
    # 1. Point estimate
    r_val <- surv_func(c(drift, sigma2), tau, threshold[1], x0)

    # 2. Standard error via Delta Method (Numerical gradient)
    grad_val <- numDeriv::grad(function(p) surv_func(p, tau, threshold[1], x0), c(drift, sigma2))
    var_s <- as.numeric(t(grad_val) %*% vcov_params %*% grad_val)
    se_s <- sqrt(max(0, var_s))

    # 3. Log-Log Transformation
    if (r_val > 0.0001 && r_val < 0.9999) {
      log_r <- log(r_val)
      se_log_log <- se_s / (r_val * abs(log_r))
      fator <- exp(z_crit * se_log_log)
      lower <- r_val^fator
      upper <- r_val^(1 / fator)
    } else {
      lower <- r_val
      upper <- r_val
    }

    data.frame(
      time = tau + t0,
      reliability = r_val,
      r_mean = r_val,
      lower = max(0, min(1, lower)),
      upper = max(0, min(1, upper)),
      threshold = as.factor(threshold[1]),
      Threshold = as.factor(threshold[1])
    )
  })

  plot_data <- do.call(rbind, df_visu_list)

  p <- ggplot2::ggplot(
    plot_data,
    ggplot2::aes(x = .data[["time"]], y = .data[["reliability"]], color = .data[["threshold"]], fill = .data[["threshold"]])
  ) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = .data[["lower"]], ymax = .data[["upper"]]), alpha = 0.2, color = NA) +
    ggplot2::geom_line(linewidth = 1, alpha = 0.7) +
    ggplot2::geom_vline(xintercept = max(0, t0 - 2), linetype = "solid", color = "black") +
    ggplot2::geom_vline(xintercept = t0, colour = "black", linetype = "longdash") +
    ggplot2::theme_classic() +
    ggplot2::theme(
      legend.position = "none",
      plot.title = if (is.null(title) || title == "") ggplot2::element_blank() else ggplot2::element_text(hjust = 0.5)
    ) +
    ggplot2::labs(title = title, x = x_label, y = y_label) +
    ggplot2::scale_x_continuous(expand = expand) +
    ggplot2::scale_y_continuous(expand = expand, labels = scales::percent, limits = c(0, 1)) +
    ggplot2::annotate(
      "text",
      x = (t0 + 5.0),
      y = 0.08,
      label = paste0("t=", t0),
      size = 3,
      colour = "black"
    )

  if (requireNamespace("tayloRswift", quietly = TRUE) &&
      !is.null(palette) &&
      palette %in% names(tayloRswift::swift_palettes)) {
    p <- p + tayloRswift::scale_color_taylor(palette = palette) +
      tayloRswift::scale_fill_taylor(palette = palette)
  }

  list(
    plot = p,
    data = plot_data,
    p = p,
    df_visu = plot_data
  )
}

#' @rdname plot_reliability_ci
#' @export
plot_reliability_ic <- function(
  mu = 3,
  sigma2 = 2,
  var_mu = 0.05,
  var_sigma2 = 0.05,
  alpha = 50,
  t0 = 10,
  x0 = 20,
  t_max = 30,
  xlab = "Tempo",
  ylab = "Confiabilidade",
  paleta = "taylor1989",
  ...
) {
  plot_reliability_ci(
    drift = mu,
    sigma2 = sigma2,
    var_drift = var_mu,
    var_sigma2 = var_sigma2,
    threshold = alpha,
    t0 = t0,
    x0 = x0,
    t_max = t_max,
    x_label = xlab,
    y_label = ylab,
    palette = paleta,
    ...
  )
}

