# ---------------------------------------------------
# Arquivo: scripts/case_study_analysis.R
# Descrição: Estudo de caso empírico e modelagem estatística do artigo
# Autor: George Anderson A. dos Santos
# ---------------------------------------------------
# ---------------------------------------------------
# Carregamento de Pacotes
pacotes <- c("tidyr", "ggplot2", "gridExtra", "viridisLite", "SimDesign", "ggh4x",
  "latex2exp", "scales", "ggthemes", "viridis", "ggrepel", "readxl",
  "lubridate", "statmod", "tayloRswift", "WriteXLS", "gt", "forcats",
  "patchwork", "grid","fitdistrplus","dplyr"
)

# Instala apenas os pacotes que ainda não estão instalados
pacotes_nao_instalados <- pacotes[!pacotes %in% installed.packages()[, "Package"]]

if (length(pacotes_nao_instalados) > 0) {
  install.packages(pacotes_nao_instalados)
}

# Carrega todos os pacotes
invisible(lapply(pacotes, library, character.only = TRUE))

# ---------------------------------------------------
## Carregamento de funções
if (requireNamespace("WienerRS", quietly = TRUE)) {
  library(WienerRS)
} else {
  source("R/utils.R")
}

# ---------------------------------------------------
## Gerando dados
set.seed(111)
df_degradacao1 <- sim_wiener_paths(n_units = 1, t_max = 20, n_steps = 20, drift = 2, sigma2 = 2)
df_degradacao2 <- sim_wiener_paths(n_units = 1, t_max = 20, n_steps = 100, drift = 0, sigma2 = 4)
df_degradacao3 <- sim_wiener_paths(n_units = 10, t_max = 20, n_steps = 200, drift = 10, sigma2 = 3)
df_degradacao4 <- sim_wiener_paths(n_units = 10, t_max = 20, n_steps = 200, drift = 10, sigma2 = 1)

aux <- diff(df_degradacao2$Wt)
mean(aux)
var(aux)

## Plotando curvas de degradação
plot_degradation_paths(df_degradacao1)
plot_degradation_paths(df_degradacao2)


# --------------------------------------------------------------------------------
# Caso 1 do Artigo:  Statistical inference for a Wiener-based degradation model
# with imperfect maintenance actions under different
# observation schemes
# --------------------------------------------------------------------------------

# rho=0.5
set.seed(111)
df_degradacao1 <- sim_wiener_maintenance(t_max = 20, n_steps = 20, drift = 2, sigma2 = 2,
                                         rho = 0.5, n_maint = 3)
calc_rho(df_degradacao1)
plot_wiener_maintenance(df_degradacao1)


# rho=1
set.seed(111)
df_degradacao1 <- sim_wiener_maintenance(t_max = 20, n_steps = 20, drift = 2, sigma2 = 2,
                                         rho = 1, n_maint = 3)
calc_rho(df_degradacao1)
plot_wiener_maintenance(df_degradacao1)


# rho=0
set.seed(111)
df_degradacao1 <- sim_wiener_maintenance(t_max = 20, n_steps = 20, drift = 2, sigma2 = 2,
                                         rho = 0, n_maint = 3)
calc_rho(df_degradacao1)
plot_wiener_maintenance(df_degradacao1)


# rho=0.8
set.seed(111)
df_degradacao1 <- sim_wiener_maintenance(t_max = 40, n_steps = 20, drift = 2, sigma2 = 2,
                                         rho = 0.8, n_maint = 9)
calc_rho(df_degradacao1)
plot_wiener_maintenance(df_degradacao1)

# ------------------------------------------------------------------------
### Different rho's per maintenance: 30-04-2024
### ARD1 (Arithmetic Reduction of Degradation)
### Using sim_wiener_maintenance_path
# ------------------------------------------------------------------------

set.seed(111)
n_manu <- 3
rho <- rep(1, n_manu) # Perfect repair in all maintenance.
df_degradacao1 <- sim_wiener_maintenance_path(t_max = 20, n_steps = 20, drift = 2, sigma2 = 2,
                                              rho = rho, n_maint = n_manu)
calc_rho(df_degradacao1)
plot_wiener_maintenance(df_degradacao1)


set.seed(111)
rho <- rep(0, n_manu) # Minimum repair in all maintenance
df_degradacao1 <- sim_wiener_maintenance_path(t_max = 20, n_steps = 20, drift = 2, sigma2 = 2,
                                              rho = rho, n_maint = n_manu)
calc_rho(df_degradacao1)
plot_wiener_maintenance(df_degradacao1)


set.seed(111)
rho <- runif(n_manu) # Maintenance effects follow a uniform distribution
set.seed(111)
df_degradacao1 <- sim_wiener_maintenance_path(t_max = 20, n_steps = 20, drift = 2, sigma2 = 2,
                                              rho = rho, n_maint = n_manu)
calc_rho(df_degradacao1)
plot_wiener_maintenance(df_degradacao1)


set.seed(111)
rho <- c(1, 0, 1)
df_degradacao1 <- sim_wiener_maintenance_path(t_max = 20, n_steps = 20, drift = 2, sigma2 = 2,
                                              rho = rho, n_maint = n_manu)
calc_rho(df_degradacao1)
plot_wiener_maintenance(df_degradacao1)


set.seed(111)
rho <- c(0.5, 0.5, 1)
df_degradacao2 <- sim_wiener_maintenance_path(t_max = 20, n_steps = 20, drift = 2, sigma2 = 2,
                                              rho = rho, n_maint = n_manu)
plot_wiener_maintenance(df_degradacao2)

# -------------------------------------------------------------------------
### Multiple systems with Different rho's per maintenance: 10-05-2024
### ARD1 (Arithmetic Reduction of Degradation)
### Usar funçao: gera_dados3, gera_plot2, mle_drift1, mle_sigma1
# -------------------------------------------------------------------------

# Geraçao e estimativas dos parâmetros considerando 4 Sistemas (n_s)
set.seed(111)
rho<- c(0.1, 0.3, 0.5)
n_manu <- 3
intra_manu <- 4
n_med <- (n_manu + 1) * (intra_manu + 2) - (n_manu + 1)
df_degradacao1 <- sim_wiener_maintenance_paths(n_units = 1, t_max = 20, n_steps = n_med,
                                               drift = 3, sigma2 = 2, rho = rho, n_maint = n_manu)

plot_wiener_maintenance_grid(df_degradacao1)
mle_drift_standard(df_degradacao1)
mle_sigma2_standard(df_degradacao1)

mle_drift_maintenance(df_degradacao1)
mle_sigma2_maintenance(df_degradacao1)
calc_rho(df_degradacao1)

# Geraçao e estimativas dos parâmetros considerando 1000 Sistemas (n_s)
set.seed(111)
df_degradacao1 <- sim_wiener_maintenance_paths(n_units = 1000, t_max = 20, n_steps = n_med,
                                               drift = 4, sigma2 = 4, rho = rho, n_maint = n_manu)
mle_drift_standard(df_degradacao1)
mle_sigma2_standard(df_degradacao1) #  entre 1 e 2 minutos para rodar
mle_drift_maintenance(df_degradacao1)
mle_sigma2_maintenance(df_degradacao1)

# ---------------------------------------------------------------------
#################### Estudo de Simulação #############################
# ---------------------------------------------------------------------
Design <- SimDesign::createDesign(n_system = c(1,10,20,50),
                                  mu = c(4,16),
                                  sigma2 = c(1,25),
                                  n_main = c(3,4,5),
                                  n_intra = c(0,2,4),
                                  tau = 20)


Generate <- function(condition,fixed_objects){
  n_med <- (condition$n_main+1)*(condition$n_intra+2)-(condition$n_main+1)
  if (condition$n_main==3){
    rho = c(0.1,0.3,0.5)
  }
  if (condition$n_main==4){
    rho = c(0.1,0.3,0.5,0.7)
  }
  if (condition$n_main==5){
    rho = c(0.1,0.3,0.5,0.7,0.9)
  }
  dat <- sim_wiener_maintenance_paths(
    n_units = condition$n_system,
    t_max = condition$tau,
    n_steps = n_med,
    drift = condition$mu,
    sigma2 = condition$sigma2,
    rho = rho,
    n_maint = condition$n_main
  )
  dat
}

Analyse <- function(condition, dat, fixed_objects) {
  n_med <- (condition$n_main+1)*(condition$n_intra+2)-(condition$n_main+1)
  
  mu_hat <- mle_drift_standard(dat)
  sigma2_hat <- mle_sigma2_standard(dat)
  
  # CP95% mu
  erro_padrao <- sqrt(sigma2_hat) / sqrt(condition$n_system * condition$tau)
  t_crit <- qt(1 - 0.05/2, df = condition$n_system*(n_med+condition$n_main +1) - 1)
  IC_mu_hat <- c(mu_hat - t_crit * erro_padrao, mu_hat + t_crit * erro_padrao)
  CP_mu_hat <- ECR(IC_mu_hat, condition$mu)
  
  # CP95% sigma^2
  id_col <- if ("Object" %in% names(dat)) "Object" else if ("Objeto" %in% names(dat)) "Objeto" else names(dat)[1]
  s <- unique(dat[[id_col]])
  k <- dat %>% filter(.data[[id_col]] == s[1], duplicated(Time)) %>% pull(Time)
  nj <- dat %>% filter(.data[[id_col]] == s[1], Time > k[1], Time < k[2]) %>% nrow()
  N <- nj * (length(k) + 1)
  df <- length(s) * (N + length(k) + 1) - 1
  
  chi_low <- qchisq(1 - 0.05/2, df)
  chi_up <- qchisq(0.05/2, df)
  
  IC_sigma_hat <- c(
    (df * sigma2_hat / chi_low),
    (df * sigma2_hat / chi_up)
  )
  CP_sigma_hat <- ECR(IC_sigma_hat, condition$sigma2)
  
  # Estimativa media da variancia
  mod_var_mu <- erro_padrao^2
  mod_var_sigma2 <- (2*sigma2_hat^2)/df
  
  
  ret <- c(mu_hat = mu_hat, sigma_hat = sigma2_hat,
           cp_mu_hat = CP_mu_hat, cp_sigma_hat = CP_sigma_hat,
           mod_var_mu = mod_var_mu, mod_var_sigma2 = mod_var_sigma2)
  
  return(ret)
}

Summarise <- function(condition, results, fixed_objects) {
  obs_bias <- bias(results[, c("mu_hat", "sigma_hat")],
                   parameter = c(condition$mu, condition$sigma2))
  obs_RMSE <- RMSE(results[, c("mu_hat", "sigma_hat")],
                   parameter = c(condition$mu, condition$sigma2))
  obs_MAE <- SimDesign::MAE(results[, c("mu_hat", "sigma_hat")],
                            parameter = c(condition$mu, condition$sigma2))
  obs_CP_mu_hat <- mean(results$cp_mu_hat)
  obs_cp_sigma_hat <- mean(results$cp_sigma_hat)
  
  obs_EmpVar_mu <- var(results$mu_hat)
  obs_EmpVar_sigma2 <- var(results$sigma_hat)
  
  obs_ModVar_mu <- mean(results$mod_var_mu)
  obs_ModVar_sigma2 <- mean(results$mod_var_sigma2)
  
  # obs_MSRSE <- MSRSE(obs_ModVar_mu,obs_EmpVar_mu)
  
  
  ret <- c(bias = obs_bias, RMSE = obs_RMSE, MAE = obs_MAE, 
           CP_mu_hat = obs_CP_mu_hat, CP_sigma2_hat = obs_cp_sigma_hat,
           obs_EmpVar_mu = obs_EmpVar_mu, obs_EmpVar_sigma2 = obs_EmpVar_sigma2,
           obs_ModVar_mu = obs_ModVar_mu, obs_ModVar_sigma2 = obs_ModVar_sigma2)
  ret
}

# resultados <- runSimulation(design=Design, replications=1000,
#                             generate=Generate, analyse=Analyse, summarise=Summarise)
# 
# saveRDS(resultados,file = "SimDesign4.rds")
# resultados <- readRDS("SimDesign4.rds")


# ---------------------------------------------------------------------
############################ Graficos #################################
# Grafico: Caminhos de degradação
# ---------------------------------------------------------------------						

# png("figures/myplot.png",width = 9,height = 3.5,units = "in")
# print(g1)
# dev.off()
# ggsave("figures/test_001.png", width = 9, height = 3.5, units = "cm")

# ---------------------------------------------------------------------
############### Dados - Banco Prof. Maria Luíza #######################
# Recorte 01
# ---------------------------------------------------------------------

# Leitura dos dados (omitindo a primeira linha, possivelmente cabeçalho repetido)
df <- read_excel("Copy of Filtro Manga - Dados de processo.xlsx")
df <- df[-1,]

# Define intervalo de tempo total de interesse
i_time <- ymd_hms("2024-05-04 10:50:00")
f_time <- ymd_hms("2024-05-04 20:00:00")

# Subconjunto inicial: pontos com diferencial = 0 dentro do intervalo
df_aux <- df %>%
  filter(Diferencial_mmCa_800dPT8102 == 0,
         `Data Hora` >= i_time & `Data Hora` < f_time)

# Visualiza esses pontos
plot(df_aux$`Data Hora`,df_aux$Diferencial_mmCa_800dPT8102)

# Define tempos de corte intermediário
f_time_0 <- ymd_hms("2024-05-04 13:00:00")

# Último instante com pressão = 0 antes de f_time_0
x_0 <- df %>%
  filter(Diferencial_mmCa_800dPT8102 == 0,
         `Data Hora` >= i_time & `Data Hora` < f_time_0) %>% 
  select(`Data Hora`) %>%
  pull() %>% max()

# Primeiro instante com pressão = 0 após f_time_0
i_time_1 <- ymd_hms("2024-05-04 13:00:00")
x_1 <- df %>%
  filter(Diferencial_mmCa_800dPT8102 == 0,
         `Data Hora` >= i_time_1 & `Data Hora` < f_time) %>% 
  select(`Data Hora`) %>%
  pull() %>% 
  min()

# Subconjunto entre dois instantes definidos
df_aux_1 <- df %>%
  filter(`Data Hora` >= x_0 & `Data Hora` < x_1)

# Finning (amostragem) a cada 8 observações
index <- seq(1,nrow(df_aux_1),by=8)
df_thin_1 <- df_aux_1[index,]  

# Define nova fronteira temporal
x_2 <- df_thin_1 %>%
  filter(`Data Hora` >= i_time_1 & `Data Hora` < f_time) %>% 
  select(`Data Hora`) %>%
  pull() %>% 
  max()

# Novo subconjunto após x_2 até o final
df_aux_2 <- df %>%
  filter(`Data Hora` >= x_2 & `Data Hora` < f_time)

# Retira valores iguais a zero com índice específico
indaux <- c(3,142:873)
df_aux_2 <- df_aux_2[indaux,]

# Novo finning a cada 8 linhas, com alguns ajustes manuais
index2 <- seq(1,nrow(df_aux_2),by=8)
# index2[seq(1,length(index2),by=13)]
cx <- c(1,105,106,209,210,313,314,417,418,520,521,625,626,729,730)
index2 <- c(index2,cx) |> unique() |> sort()
df_aux_2 <- df_aux_2[index2,]

# Verifica número de blocos de 14 observações
nrow(df_aux_2) / 14  # Deve dar 7

# Cria vetor de tempos para df_aux_2
s1 <- seq(1,14)
for (i in 1:8) {
  s2 <- seq(max(s1),max(s1) + 13)
  s1 <- c(s1,s2)
}

# Garante que o vetor tenha o comprimento certo para o número de linhas
length(s1[15:(15+98)])  # 99 valores
length(s1)  # Total

# Adiciona coluna de tempo
df_thin_1 <- df_thin_1 %>%
  mutate(Time = seq(1,nrow(df_thin_1)))

df_aux_2 <- df_aux_2 %>%
  mutate(Time = s1[15:(15+98)])

# Junta os dois subconjuntos
df_aux_maria <- rbind(df_thin_1,df_aux_2)

# Recorte para análise de exemplo
sub_maria <- df_aux_maria[1:50,]
plot(sub_maria$Time,sub_maria$Diferencial_mmCa_800dPT8102,type = "l")

# Renomeia variável e adiciona identificador de objeto
names(sub_maria)[5] <- "Y"
sub_maria <- sub_maria %>%
  mutate(Time = Time -1,
         Objeto = "OBJ_001")

# Visualização e estimação dos parâmetros de degradação
plot_maintenance(sub_maria, y_label = "Degradation", x_label = "Time", show_maintenance_times = TRUE)
mle_drift_maintenance(sub_maria)
mle_sigma2_maintenance(sub_maria)
calc_rho(sub_maria)

# Exportação opcional
# write.csv2(
#   sub_maria %>% dplyr::select(-Objeto),
#   "subsets/recorte_01_thetaneg.csv",
#   row.names = FALSE
# )

# Observação: tempo real entre observações é 3,5 minutos (original = 26s)
# Número de observações entre manutenções: 12

# ---------------------------------------------------------------------
############### Dados - Banco Prof. Maria Luíza #######################
# Recorte 02
# ---------------------------------------------------------------------

# Leitura dos dados
df <- read_excel("data-raw/Bagfilter_Dataset.xlsx")
df <- df[-1,]

# Tempo inicial e tempo final
i_time <- ymd_hms("2024-05-04 14:40:08")
f_time <- ymd_hms("2024-05-05 00:00:00")

# Filtrando banco considerando tempo inicial e tempo final
df_aux <- df %>%
  filter(#Diferencial_mmCa_800dPT8102 == 0,
         `Data Hora` >= i_time & `Data Hora` < f_time)
plot(df_aux$`Data Hora`,df_aux$Diferencial_mmCa_800dPT8102,type = "l")

# Definição de passos para finning e número de medidas entre ações de manutenção
step = 30
n_intra_manu = 12

# verificando index de todas as medidas de degradação (sem ações após manuntenção)
index <- seq(1,nrow(df_aux),by=step)

# df_aux[c(781,783),c(1,5)]

# Index de medidas exatamente antes e exatamente após ação de manutenção
cx <- c(1,391,392,781,783,1171,1173)

# Unindo index
index <- c(index,cx)
index <- unique(index)
index <- sort(index)

# filtrando base considerando index criado
df_aux <- df_aux[index,]

# Preparando tempos
s1 <- seq(1,14)
for (i in 1:3) {
  s2 <- seq(max(s1),max(s1) + 13)
  s1 <- c(s1,s2)
}

# Adicionando tempo a base de dados
df_aux <- df_aux %>%
  mutate(Time = s1[1:nrow(df_aux)])
  
# Modificando Base Criada
subset_bagfilter <- df_aux
names(subset_bagfilter)[5] <- "Y"
subset_bagfilter <- subset_bagfilter %>%
  mutate(Time = Time -1,
         Objeto = "OBJ_001")

# gerando grafico e estimando parametros
plot_maintenance(subset_bagfilter, y_label = "Diferencial", x_label = "Tempo", show_maintenance_times = TRUE)
mu <- mle_drift_maintenance(subset_bagfilter)
sigma2 <- mle_sigma2_maintenance(subset_bagfilter)
calc_rho(subset_bagfilter)

#################
#################
#################

mu_hat <- mle_drift_maintenance(subset_bagfilter)
sigma2_hat <- mle_sigma2_maintenance(subset_bagfilter)
# CP95% sigma^2
dat = subset_bagfilter
s <- 1
k <- dat %>% filter(duplicated(Time)) %>% pull(Time)
nj <- dat %>% filter(Time > k[1], Time < k[2]) %>% nrow()
N <- nj * (length(k) + 1)
N <- 38
df <- length(s) * (N + length(k) + 1) - 1

# CP95% mu
erro_padrao <- sqrt(sigma2_hat) / sqrt(42)
t_crit <- qt(1 - 0.05/2, df = df)
IC_mu_hat <- c(mu_hat - t_crit * erro_padrao, mu_hat + t_crit * erro_padrao)
dat = subset_bagfilter


chi_low <- qchisq(1 - 0.05/2, df)
chi_up <- qchisq(0.05/2, df)

IC_sigma_hat <- c(
  (df * sigma2_hat / chi_low),
  (df * sigma2_hat / chi_up)
)

sqrt((sigma2_hat^2)*2/df)

var_mu <- (sqrt(sigma2_hat) / sqrt(42))^2
var_sigma2 <- (sqrt((sigma2_hat^2)*2/df))^2

# sub_maria_1 <- sub_maria
# write.csv2(sub_maria_1 %>%
#              select(-Objeto),"subsets/recorte_02_thetaposi.csv",
#            row.names = FALSE)

# 13 minutos (tempo original: 26 segundos)
# numero de medidas entre ações de manutenção: 12

# ------------------------------------------------
######## Gerando Curvas de Confiabilidade ########
# ------------------------------------------------

# gera curvas de confiabilidade considerando fdp do first passage time (fpt) como gaussiana inversa
# media: media
# variancia: (media^3)/desvio


# --------------------------------------------------------
########### Gera Tabela de Confiabilidade ################
# --------------------------------------------------------

reliability <- df_visu %>%
  mutate(r_mean = paste(round(r_mean, digits = 4) * 100, "%")) %>%
  spread(key = "Threshold", value = "r_mean") %>%
  filter(time %in% c(39, 50, 70, 90, 110, 130, 150, 170))

library(grid)
library(gridExtra)

myTable <- tableGrob(reliability, rows = NULL)
grid.draw(myTable)

# --------------------------------------------------------------------------------
# NOTA: O código de geração e salvamento de todas as figuras do artigo foi
# modularizado e transferido para o script dedicado: scripts/generate_figures.R
# --------------------------------------------------------------------------------

###########################################################
### Comparação modelo wiener original - modelo proposto ###
###########################################################

k <- subset_bagfilter |>
  filter(duplicated(Time)) |>
  select(Time) %>% pull()

rho = calc_rho(subset_bagfilter)
for (j in 1:(length(k)+1)) {
  if (j==1){
    ti <- subset_bagfilter %>% filter(Time <= k[j]) %>% 
      filter(row_number() <= n()-1) %>% select(Time) %>% pull()
    complete_y <- mu*ti
  }
  if (j==2){
    ti <- subset_bagfilter %>% filter(Time >= k[j-1],Time <= k[j]) %>% slice(3:n()-1) %>% 
      select(Time) %>% pull()
    complete_y <- c(complete_y,mu*ti-rho[1]*mu*13)
  }
  if (j==3){
    ti <- subset_bagfilter %>% filter(Time >= k[j-1],Time <= k[j]) %>% slice(3:n()-1) %>% 
      select(Time) %>% pull()
    complete_y <- c(complete_y,mu*ti-rho[1]*mu*13-rho[2]*(mu*26-mu*13))
  }
  if (j==4){
    ti <- subset_bagfilter %>% filter(Time >= k[j-1]) %>% slice(2:n()) %>% 
      select(Time) %>% pull()
    complete_y <- c(complete_y,mu*ti-rho[1]*mu*13-rho[2]*(mu*26-mu*13)-rho[3]*(mu*39-mu*26))
  }
}
modelo_completo <- complete_y


rho = rep(mean(calc_rho(subset_bagfilter)),3)
# rho = rep(0.5,3)
for (j in 1:(length(k)+1)) {
  if (j==1){
    ti <- subset_bagfilter %>% filter(Time <= k[j]) %>% 
      filter(row_number() <= n()-1) %>% select(Time) %>% pull()
    complete_y <- mu*ti
  }
  if (j==2){
    ti <- subset_bagfilter %>% filter(Time >= k[j-1],Time <= k[j]) %>% slice(3:n()-1) %>% 
      select(Time) %>% pull()
    complete_y <- c(complete_y,mu*ti-rho[1]*mu*13)
  }
  if (j==3){
    ti <- subset_bagfilter %>% filter(Time >= k[j-1],Time <= k[j]) %>% slice(3:n()-1) %>% 
      select(Time) %>% pull()
    complete_y <- c(complete_y,mu*ti-rho[1]*mu*13-rho[2]*(mu*26-mu*13))
  }
  if (j==4){
    ti <- subset_bagfilter %>% filter(Time >= k[j-1]) %>% slice(2:n()) %>% 
      select(Time) %>% pull()
    complete_y <- c(complete_y,mu*ti-rho[1]*(mu*13)-rho[2]*(mu*26-mu*13)-rho[3]*(mu*39-mu*26))
  }
}
modelo_simples <- complete_y

subset_bagfilter$modelo_completo <- modelo_completo
subset_bagfilter$modelo_simples <- modelo_simples

subset_bagfilter <- subset_bagfilter |>
  mutate(erro_completo = Y - modelo_completo,
         erro_simples = Y - modelo_simples)

calcular_criterios <- function(sse, n_obs, n_params) {
  # A log-verossimilhança de um modelo gaussiano é proporcional ao log(SSE)
  logLik <- -n_obs/2 * (log(2*pi) + log(sse/n_obs) + 1)
  aic <- -2 * logLik + 2 * n_params
  bic <- -2 * logLik + n_params * log(n_obs)
  return(list(logLik = round(logLik,2), AIC = round(aic,2), BIC = round(bic,2)))
}

# Parâmetros para os critérios
n_observacoes <- nrow(subset_bagfilter)
p_completo <- 5 # mu, sigma^2, e 3 rhos
p_reduzido <- 3                      # mu, sigma^2, e 1 rho

criterios_reduzido <- calcular_criterios(sum(subset_bagfilter$erro_simples^2),
                                 n_observacoes,p_reduzido)
criterios_completo <- calcular_criterios(sum(subset_bagfilter$erro_completo^2),
                                  n_observacoes,p_completo)

# Montar tabela de resultados
tabela_comparacao <- tibble(
  Modelo = c("Completo (ρj variável)", "Reduzido (ρ fixo)"),
  Num_Parametros = c(p_completo, p_reduzido),
  LogLik = c(criterios_completo$logLik, criterios_reduzido$logLik),
  AIC = c(criterios_completo$AIC, criterios_reduzido$AIC),
  BIC = c(criterios_completo$BIC, criterios_reduzido$BIC)
)
print("Tabela de Comparação (menor AIC/BIC é melhor):")
print(tabela_comparacao)

LR = -2*(criterios_reduzido$logLik - criterios_completo$logLik)
pchisq(LR, df = 2, lower.tail = FALSE) %>% round(3)

###########################################################
############ IC 80% - Curva de Confiabilidade #############
###########################################################

df_ic <- plot_reliability_ci(drift = mu, sigma2 = sigma2,
                             var_drift = var_mu, var_sigma2 = var_sigma2,
                             threshold = 150,
                             t0 = t0,
                             x0 = x0,
                             t_max = c(150+5),
                             x_label = "Time", y_label = "Reliability (%)",
                             palette = "taylor1989")$data

df_ic <- df_ic %>%
  mutate(lower = round(lower*100,2),
         upper = round(upper*100,2))

View(df_ic)
