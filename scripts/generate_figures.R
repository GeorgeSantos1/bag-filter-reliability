# ==============================================================================
# Script de Reprodução: Geração de Todas as Figuras da Dissertação / Artigo
#
# Pacote: WienerRS
# Autor: George Anderson A. dos Santos
# Descrição: Este script gera e exporta todas as figuras conceituais, empíricas
#            e de simulação Monte Carlo para o diretório 'figures/'.
# ==============================================================================

# 1. Carregamento de Pacotes
if (requireNamespace("WienerRS", quietly = TRUE)) {
  library(WienerRS)
} else {
  source("R/utils.R")
}

library(ggplot2)

# Garantir existência do diretório de saída
if (!dir.exists("figures")) {
  dir.create("figures")
}

# Função auxiliar para salvar figuras em múltiplos formatos (.svg, .pdf, .eps)
save_plot <- function(plot_obj, base_name, width, height, formats = c("svg", "pdf")) {
  svg_path <- file.path("figures", paste0(base_name, ".svg"))
  
  # Salvar SVG inicial
  ggplot2::ggsave(
    filename = svg_path,
    plot     = plot_obj,
    width    = width,
    height   = height,
    units    = "in",
    dpi      = 300
  )
  message("Figura salva: ", svg_path)
  
  # Conversão para PDF e EPS usando rsvg se disponível
  if (requireNamespace("rsvg", quietly = TRUE)) {
    if ("pdf" %in% formats) {
      pdf_path <- file.path("figures", paste0(base_name, ".pdf"))
      rsvg::rsvg_pdf(svg_path, pdf_path)
      message("  -> Convertido para PDF: ", pdf_path)
    }
    if ("eps" %in% formats) {
      eps_path <- file.path("figures", paste0(base_name, ".eps"))
      rsvg::rsvg_eps(svg_path, eps_path)
      message("  -> Convertido para EPS: ", eps_path)
    }
  }
}

# ==============================================================================
# PARTE 1: Modelos Teóricos de Confiabilidade e Sobrevivência
# ==============================================================================

message("\n[1/4] Gerando figuras conceituais de confiabilidade...")

labs_density     <- c("Densidade", "Tempo", "f(t)")
labs_hazard      <- c("Taxa de Falha", "Tempo", "\u03bb(t)")
labs_reliability <- c("Confiabilidade", "Tempo", "R(t)")

# 1.1 Distribuição Exponencial
p_exp <- plot_exponential(
  labs_density     = labs_density,
  labs_reliability = labs_reliability,
  labs_hazard      = labs_hazard
)
save_plot(p_exp, "PLOT_EXP", width = 9, height = 6, formats = c("svg", "pdf"))

# 1.2 Distribuição Weibull
p_weibull <- plot_weibull(
  labs_density     = labs_density,
  labs_reliability = labs_reliability,
  labs_hazard      = labs_hazard
)
save_plot(p_weibull, "PLOT_WEIBULL", width = 9, height = 6, formats = c("svg", "pdf"))

# 1.3 Distribuição Lognormal
p_lognormal <- plot_lognormal(
  labs_density     = labs_density,
  labs_reliability = labs_reliability,
  labs_hazard      = labs_hazard
)
save_plot(p_lognormal, "PLOT_LOGNORMAL", width = 9, height = 6, formats = c("svg", "pdf"))

# 1.4 Esquemas de Censura
p_censura <- plot_censoring()
save_plot(p_censura, "PLOT_CENSURA", width = 10, height = 6, formats = c("svg", "pdf"))

# 1.5 Curva da Banheira (Bathtub Curve)
labs_banheira <- c("Mortalidade \nInfantil", "Vida Operacional", "Obsolesc\u00eancia", "Tempo")
p_banheira <- plot_bathtub_curve(labs_banheira = labs_banheira)
save_plot(p_banheira, "PLOT_BANHEIRA", width = 6, height = 3.5, formats = c("svg", "pdf"))

# 1.6 Degradação e Limiar Crítico de Falha
labs_degradacao <- c("Limiar de Falha", "Caminho de Degrada\u00e7\u00e3o", "Tempo de Falha", "Tempo", "N\u00edvel de Degrada\u00e7\u00e3o")
p_degrada001 <- plot_degradation(labs_degradacao = labs_degradacao)
save_plot(p_degrada001, "PLOT_DEGRADA001", width = 8, height = 4, formats = c("svg", "pdf"))

# 1.7 Processo de Wiener com Diferentes Drifts
labs_wiener <- c("Degrada\u00e7\u00e3o", "Tempo")
p_wiener <- plot_wiener_drift(labs_wiener = labs_wiener)
save_plot(p_wiener, "PLOT_WIENER", width = 6, height = 3.5, formats = c("svg", "pdf"))

# 1.8 Comparação dos Tipos de Reparo (Perfeito, Mínimo, Imperfeito)
labs_reparos <- c("Time", "Degradation", "(a)", "(b)", "(c)")
p_reparos <- plot_repair_types(labs_reparos = labs_reparos)
save_plot(p_reparos, "PLOT_REPARO", width = 9, height = 6, formats = c("svg", "pdf", "eps"))

# 1.9 Esquema de Observação com Manutenções Imperfeitas
labs_scheme <- c("Time", "Degradation")
p_scheme <- plot_maintenance_scheme(labs_scheme = labs_scheme)
save_plot(p_scheme, "PLOT_SCHEMA", width = 8, height = 4, formats = c("svg", "pdf", "eps"))
save_plot(p_scheme, "PLOT_SCHEMA_R1", width = 8, height = 4, formats = c("svg", "pdf", "eps"))

# 1.10 Degradação Exponencial
p_degrada01 <- plot_exponential_degradation(labs_degrada01 = c("Time", "Degradation"))
save_plot(p_degrada01, "PLOT_DEGRADA01", width = 8, height = 4, formats = c("svg", "pdf"))

# 1.11 Confiabilidade Exponencial com Tempo Mediano
p_conf_exp <- plot_exponential_reliability(x_label = "Tempo", y_label = "R(t)")
save_plot(p_conf_exp, "CONFIABILIDADE_001", width = 6, height = 3.5, formats = c("svg", "pdf"))

# ==============================================================================
# PARTE 2: Estudo de Simulação Monte Carlo (SimDesign4.rds)
# ==============================================================================

message("\n[2/4] Gerando figuras do estudo de simulação...")

sim_file <- if (file.exists("simulations/SimDesign4.rds")) {
  "simulations/SimDesign4.rds"
} else if (file.exists("SimDesign4.rds")) {
  "SimDesign4.rds"
} else {
  NULL
}

if (!is.null(sim_file)) {
  resultados_sim <- readRDS(sim_file)

  # 2.1 Raiz do Erro Quadrático Médio (RMSE)
  p_rmse <- plot_simulation_rmse(data = resultados_sim, labs_rmse = c("Number of Systems", "RMSE"))
  save_plot(p_rmse, "PLOT_RMSE", width = 11, height = 6, formats = c("svg", "pdf", "eps"))

  # 2.2 Viés de Estimação (Bias)
  p_bias <- plot_simulation_bias(data = resultados_sim, labs_bias = c("Number of Systems", "Bias"))
  save_plot(p_bias, "PLOT_BIAS", width = 11, height = 6, formats = c("svg", "pdf", "eps"))

  # 2.3 Probabilidade de Cobertura (CP 95%)
  p_cp <- plot_simulation_coverage(data = resultados_sim, labs_coverage = c("N\u00famero de Sistemas", "Probabilidade de Cobertura (95%)"))
  save_plot(p_cp, "PLOT_CP", width = 11, height = 6, formats = c("svg", "pdf"))

  # 2.4 Razão de Variâncias (Modelo / Empírica)
  p_ratiovar <- plot_simulation_variance_ratio(data = resultados_sim, labs_ratiovar = c("N\u00famero de Sistemas", "Raz\u00e3o de Vari\u00e2ncias"))
  save_plot(p_ratiovar, "PLOT_RATIOVAR", width = 11, height = 6, formats = c("svg", "pdf"))
} else {
  warning("Arquivo 'simulations/SimDesign4.rds' não encontrado. Figuras de simulação ignoradas.")
}

# ==============================================================================
# PARTE 3: Análise Empírica - Filtro de Mangas (Dataset 'bagfilter')
# ==============================================================================

message("\n[3/4] Gerando figuras da aplicação empírica (Filtro de Mangas)...")

# Carregar dados empíricos do pacote WienerRS
data(bagfilter, package = "WienerRS", envir = environment())
if (!exists("bagfilter")) {
  load("data/bagfilter.rda")
}

# Estimação dos parâmetros do processo empírico
mu_est     <- mle_drift_maintenance(bagfilter)
sigma2_est <- mle_sigma2_maintenance(bagfilter)
rho_est    <- calc_rho(bagfilter)

t0_eval <- 39
x0_eval <- min(bagfilter$Y[bagfilter$Time == t0_eval])

# 3.1 Trajetória Empírica de Degradação do Filtro de Mangas
p_result001 <- plot_maintenance(
  data                   = bagfilter,
  y_label                = "Differential [mmWC] ",
  x_label                = "Time",
  show_maintenance_times = TRUE
)
save_plot(p_result001, "RESULT_001", width = 8, height = 4, formats = c("svg", "pdf", "eps"))

# 3.2 Comparação entre Processo Observado Y(t) e Trajetória Teórica Natural X(t)
labs_xtyt <- c(
  "Y(t) - Processo de degrada\u00e7\u00e3o com a\u00e7\u00f5es de manuten\u00e7\u00e3o",
  "X(t) - Processo de degrada\u00e7\u00e3o natural",
  "Tempo",
  "Degrada\u00e7\u00e3o"
)
p_xtyt <- plot_wiener_maintenance_comparison(labs_xtyt = labs_xtyt)
save_plot(p_xtyt, "PLOT_XTYT", width = 8, height = 4, formats = c("svg", "pdf"))

# 3.3 Funções de Mérito: PDF e CDF do Primeiro Tempo de Atingimento (FHT)
p_merito <- plot_merit_functions(
  drift         = mu_est,
  sigma2        = sigma2_est,
  threshold     = 150,
  t0            = t0_eval,
  x0            = x0_eval,
  t_max         = 155,
  labs_merito01 = c("Densidade", "Tempo", "f(t)"),
  labs_merito02 = c("Acumulada", "Tempo", "F(t)")
)
save_plot(p_merito, "PLOT_MERITO", width = 8, height = 4, formats = c("svg", "pdf"))

# 3.4 Diagnóstico de Aderência P-P Plot e Q-Q Plot com Teste Anderson-Darling
labs_qq01 <- c("Theoretical Cumulative Distribution", "Empirical Cumulative Distribution", "P-P Plot")
labs_qq02 <- c("Theoretical Quantiles", "Empirical Quantiles", "Q-Q Plot")
p_qqplot <- plot_diagnostic_qq(
  data          = bagfilter,
  labs_qqplot01 = labs_qq01,
  labs_qqplot02 = labs_qq02
)
save_plot(p_qqplot, "PLOT_QQPLOT", width = 8, height = 4, formats = c("svg", "pdf", "eps"))

# 3.5 Curva Teórica de Confiabilidade Gaussiana Inversa
p_result002 <- plot_reliability(
  drift     = mu_est,
  sigma2    = sigma2_est,
  threshold = 150,
  t0        = t0_eval,
  x0        = x0_eval,
  t_max     = 155,
  x_label   = "Tempo",
  y_label   = "Confiabilidade (%)",
  palette   = "taylor1989"
)
save_plot(p_result002, "RESULT_002", width = 11, height = 5, formats = c("svg", "pdf"))

# 3.6 Curva de Confiabilidade com Intervalo de Confiança assintótico (Delta Method)
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

message("\n[4/4] Todas as figuras foram geradas e salvas com sucesso em 'figures/'!")
