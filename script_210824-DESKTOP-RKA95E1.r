
# ---------------------------------------------------
# Arquivo: script_210824-DESKTOP-RKA95E1.r
# Descrição: Conjunto de funções úteis para análise
# Autor: George Anderson A. dos Santos
# Data: 17-04-2024
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
source("utils.r")

# ---------------------------------------------------
## Gerando dados
set.seed(111)
df_degradacao1 <- gera_dados(n_obs=1,t_max=20,n_med=20,v=2,sigma = sqrt(2))
df_degradacao2 <- gera_dados(n_obs=1,t_max=20,n_med=100,v=0,sigma = 4)
df_degradacao3 <- gera_dados(n_obs=10,t_max=20,n_med=200,v=10,sigma = 3)
df_degradacao4 <- gera_dados(n_obs=10,t_max=20,n_med=200,v=10,sigma = 1)

aux <- diff(df_degradacao2$Wt)
mean(aux)
var(aux)

## Plotando curvas de degradação
gera_plot(df_degradacao1)
gera_plot(df_degradacao2)

## EMV: Drift
mle_drift(data=df_degradacao1)
mle_drift(data=df_degradacao2)

## EMV: Sigma
mle_sigma(data=df_degradacao1)
mle_sigma(data=df_degradacao2)

# --------------------------------------------------------------------------------
# Caso 1 do Artigo:  Statistical inference for a Wiener-based degradation model
# with imperfect maintenance actions under different
# observation schemes
# --------------------------------------------------------------------------------

# rho=0.5
set.seed(111)
df_degradacao1 <-  gera_dados1(t_max = 20,n_med=20,v=2,sigma2=(2),
            rho=0.5,n_manu=3)
rho_hat(df_degradacao1)
gera_plot1(df_degradacao1)


# rho=1
set.seed(111)
df_degradacao1 <-  gera_dados1(t_max = 20,n_med=20,v=2,sigma=sqrt(2),
                               rho=1,n_manu=3)
rho_hat(df_degradacao1)
gera_plot1(df_degradacao1)


# rho=0
set.seed(111)
df_degradacao1 <-  gera_dados1(t_max = 20,n_med=20,v=2,sigma=sqrt(2),
                               rho=0,n_manu=3)
rho_hat(df_degradacao1)
gera_plot1(df_degradacao1)


# rho=0.8
set.seed(111)
df_degradacao1 <-  gera_dados1(t_max = 40,n_med=20,v=2,sigma=sqrt(2),
                               rho=0.8,n_manu=9)
rho_hat(df_degradacao1)
gera_plot1(df_degradacao1)

# ------------------------------------------------------------------------
### Different rho's per maintenance: 30-04-2024
### ARD1 (Arithmetic Reduction of Degradation)
### Usar funçao gera_dados2
# ------------------------------------------------------------------------

set.seed(111)
n_manu <- 3
rho = rep(1,n_manu) # Perfect repair in all maintenance.
df_degradacao1 <-  gera_dados2(t_max = 20,n_med=20,v=2,sigma=sqrt(2),
                               rho=rho,n_manu=n_manu)
rho_hat(df_degradacao1)
gera_plot1(df_degradacao1)


set.seed(111)
rho = rep(0,n_manu) # Minimum repair in all maintenance
df_degradacao1 <-  gera_dados2(t_max = 20,n_med=20,v=2,sigma=sqrt(2),
                               rho=rho,n_manu=n_manu)
rho_hat(df_degradacao1)
gera_plot1(df_degradacao1)


set.seed(111)
rho = runif(n_manu) # Maintenance effects follow a uniform distribution
set.seed(111)
df_degradacao1 <-  gera_dados2(t_max = 20,n_med=20,v=2,sigma=sqrt(2),
                               rho=rho,n_manu=n_manu)
rho_hat(df_degradacao1)
gera_plot1(df_degradacao1)


set.seed(111)
rho<- c(1, 0, 1)
df_degradacao1 <-  gera_dados2(t_max = 20,n_med=20,v=2,sigma=sqrt(2),
                               rho=rho,n_manu=n_manu)
rho_hat(df_degradacao1)
gera_plot1(df_degradacao1)


set.seed(111)
rho<- c(0.5, 0.5, 1)
df_degradacao2 <-  gera_dados2(t_max = 20,n_med=20,v=2,sigma=sqrt(2),
                               rho=rho,n_manu=n_manu)
gera_plot1(df_degradacao2)

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
n_med <- (n_manu+1)*(intra_manu+2)-(n_manu+1)
df_degradacao1 <- gera_dados3(n_s=1,t_max = 20,n_med=n_med,v=3,sigma2=2,rho=rho,n_manu=n_manu)

gera_plot2(df_degradacao1)
mle_drift1(df_degradacao1)
mle_sigma1(df_degradacao1)

mle_drift1_y(df_degradacao1)
mle_sigma1_y(df_degradacao1)
rho_hat(df_degradacao1)

# Geraçao e estimativas dos parâmetros considerando 1000 Sistemas (n_s)
set.seed(111)
df_degradacao1 <- gera_dados3(n_s=1000,t_max = 20,n_med=n_med,v=4,sigma2 = 4,rho=rho,n_manu=n_manu)
mle_drift1(df_degradacao1)
mle_sigma1(df_degradacao1) #  entre 1 e 2 minutos para rodar
mle_drift1_y(df_degradacao1)
mle_sigma1_y(df_degradacao1)

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
  dat <- gera_dados3(n_s=condition$n_system,
                     t_max = condition$tau,n_med=n_med,
                     v=condition$mu,sigma2=condition$sigma2,rho=rho,n_manu=condition$n_main)
  dat
}

Analyse <- function(condition, dat, fixed_objects) {
  n_med <- (condition$n_main+1)*(condition$n_intra+2)-(condition$n_main+1)
  
  mu_hat <- mle_drift1(dat)
  sigma2_hat <- (mle_sigma1(dat))
  
  # CP95% mu
  erro_padrao <- sqrt(sigma2_hat) / sqrt(condition$n_system * condition$tau)
  t_crit <- qt(1 - 0.05/2, df = condition$n_system*(n_med+condition$n_main +1) - 1)
  IC_mu_hat <- c(mu_hat - t_crit * erro_padrao, mu_hat + t_crit * erro_padrao)
  CP_mu_hat <- ECR(IC_mu_hat, condition$mu)
  
  # CP95% sigma^2
  s <- unique(dat$Objeto)
  k <- dat %>% filter(Objeto == s[1], duplicated(Time)) %>% pull(Time)
  nj <- dat %>% filter(Objeto == s[1], Time > k[1], Time < k[2]) %>% nrow()
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
plot_maintanance(sub_maria,ylab="Degradation",xlab="Time",time = TRUE)
mle_drift1_y(sub_maria)
mle_sigma1_y(sub_maria)
rho_hat(sub_maria)

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
df <- read_excel("Copy of Filtro Manga - Dados de processo.xlsx")
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
plot_maintanance(subset_bagfilter,ylab="Diferencial",xlab="Tempo",time = TRUE)
rsvg::rsvg_pdf('figures/RESULT_001.svg',"figures/RESULT_001.pdf")
mu <- mle_drift1_y(subset_bagfilter)
sigma2 <- mle_sigma1_y(subset_bagfilter)
rho_hat(subset_bagfilter)

#################
#################
#################

mu_hat <- mle_drift1_y(subset_bagfilter)
sigma2_hat <- mle_sigma1_y(subset_bagfilter)
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

reliability = df_visu %>%
  mutate(r_mean = paste(round(r_mean,digits = 4)*100,"%")) %>%
  spread(key = "Threshold",value="r_mean")
saveRDS(reliability,"confiabilidade.dat")

# Carrega os dados
reliability = readRDS("confiabilidade.dat")

# filtra para tempos 39:45
reliability = reliability %>%
  filter(time %in% c(39,50,70,90,110,130,150,170))

library(grid)
library(gridExtra)

myTable <- tableGrob(reliability,
                     rows = NULL)
grid.draw(myTable)

# salva os dados
write.csv2(reliability,"confiabilidade.xlsx")

########################################
########################################
########################################
rsvg::rsvg_eps("figures/Desenho1.svg","figures/Desenho1.eps")

labs_01 = c("Densidade","Tempo","f(t)")
labs_02 = c("Taxa de Falha","Tempo","λ(t)")
labs_03 = c("Confiabilidade","Tempo","R(t)")
gera_plot_exp(labs_01,labs_02,labs_03)
# Salvar em 900x600 em .svg
rsvg::rsvg_pdf('figures/PLOT_EXP.svg',"figures/PLOT_EXP.pdf")

gera_plot_weibull(labs_01,labs_02,labs_03)
# Salvar em 900x600 em .svg
rsvg::rsvg_pdf('figures/PLOT_WEIBULL.svg',"figures/PLOT_WEIBULL.pdf")

gera_plot_lognormal(labs_01,labs_02,labs_03)
# Salvar em 900x600 em .svg
rsvg::rsvg_pdf('figures/PLOT_LOGNORMAL.svg',"figures/PLOT_LOGNORMAL.pdf")

plot_censura_all()
# Salvar em 1000x600 em .svg
rsvg::rsvg_pdf('figures/PLOT_CENSURA.svg',"figures/PLOT_CENSURA.pdf")

labs_banheira <- c("Mortalidade \nInfantil","Vida Operacional",
                   "Obsolescência","Tempo")
gera_plot_banheira(labs_banheira)
# Salvar em 600x350 em .svg
rsvg::rsvg_pdf('figures/PLOT_BANHEIRA.svg',"figures/PLOT_BANHEIRA.pdf")

labs_degradacao <- c("Limiar de Falha","Caminho de Degradação","Tempo de Falha",
                     "Tempo","Nível de Degradação")
gera_plot_degrada(labs_degradacao)
# Salvar em 800x400 em .svg
rsvg::rsvg_pdf("figures/PLOT_DEGRADA001.svg","figures/PLOT_DEGRADA001.pdf")

labs_wiener <- c("Degradação","Tempo")
gera_plot_wiener(labs_wiener)
# Salvar em 600x350 em m.svg
rsvg::rsvg_pdf("figures/PLOT_WIENER.svg","figures/PLOT_WIENER.pdf")

labs_reparos <- c("Time","Degradation",
                  "(a)","(b)","(c)")
gera_plot_reparos(labs_reparos)
# Salvar em 900x600 em .svg
rsvg::rsvg_pdf('figures/PLOT_REPARO.svg',"figures/PLOT_REPARO.pdf")
rsvg::rsvg_eps('figures/PLOT_REPARO.svg',"figures/PLOT_REPARO.eps")

labs_scheme <- c("Time","Degradation")
gera_plot_scheme(labs_scheme)
# Salvar em 800x400 em .svg
rsvg::rsvg_pdf('figures/PLOT_SCHEMA.svg',"figures/PLOT_SCHEMA.pdf")
rsvg::rsvg_eps('figures/PLOT_SCHEMA.svg',"figures/PLOT_SCHEMA.eps")
rsvg::rsvg_pdf('figures/PLOT_SCHEMA_R1.svg',"figures/PLOT_SCHEMA_R1.pdf")
rsvg::rsvg_eps('figures/PLOT_SCHEMA_R1.svg',"figures/PLOT_SCHEMA_R1.eps")

resultados <- readRDS("SimDesign4.rds")
labs_rmse <- c("Number of Systems","RMSE")
gera_plot_rmse(resultados,labs_rmse)
# Salvar em 1100x600 em .svg
rsvg::rsvg_pdf('figures/PLOT_RMSE.svg',"figures/PLOT_RMSE.pdf")
rsvg::rsvg_eps('figures/PLOT_RMSE.svg',"figures/PLOT_RMSE.eps")

resultados <- readRDS("SimDesign4.rds")
labs_bias <- c("Number of Systems","Bias")
gera_plot_bias(resultados,labs_bias)
# Salvar em 1100x600 em .svg
rsvg::rsvg_pdf('figures/PLOT_BIAS.svg',"figures/PLOT_BIAS.pdf")
rsvg::rsvg_eps('figures/PLOT_BIAS.svg',"figures/PLOT_BIAS.eps")

resultados <- readRDS("SimDesign4.rds")
labs_coverage <- c("Número de Sistemas","Probabilidade de Cobertura (95%)")
gera_plot_coveragep(resultados,labs_coverage)
# Salvar em 1100x600 em .svg
rsvg::rsvg_pdf('figures/PLOT_CP.svg',"figures/PLOT_CP.pdf")

resultados <- readRDS("SimDesign4.rds")
labs_ratiovar <- c("Número de Sistemas","Razão de Variâncias")
gera_plot_ratiovar(resultados,labs_ratiovar)
# Salvar em 1100x600 em .svg
rsvg::rsvg_pdf('figures/PLOT_RATIOVAR.svg',"figures/PLOT_RATIOVAR.pdf")

labs_xtyt <- c("Y(t) - Processo de degradação com ações de manutenção",
               "X(t) - Processo de degradação natural",
               "Tempo","Degradação")
gera_plot_xtyt(labs_xtyt)
# Salvar em 800x400 em .svg
rsvg::rsvg_pdf('figures/PLOT_XTYT.svg',"figures/PLOT_XTYT.pdf")

labs_merito01 <- c("Densidade","Tempo","f(t)")
labs_merito02 <- c("Acumulada","Tempo","F(t)")
t0 = 39
x0 = subset_bagfilter %>%
  filter(Time == 39) %>%
  filter(Y == min(Y)) %>%
  select(Y) %>%
  pull()
gera_plot_merito(mu=mu,sigma=sigma2,
                 alpha = 150,
                 t0 = t0,
                 x0 = x0,
                 t_max = c(150+5),
                 labs_merito01,
                 labs_merito02)
# Salvar em 800x400 em .svg
rsvg::rsvg_pdf('figures/PLOT_MERITO.svg',"figures/PLOT_MERITO.pdf")

labs_qqplot01 <- c("Theoretical Cumulative Distribution","Empirical Cumulative Distribution","P-P Plot")
labs_qqplot02 <- c("Theoretical Quantiles","Empirical Quantiles","Q-Q Plot")
gera_plot_qqplot(subset_bagfilter,labs_qqplot01,labs_qqplot02)
# Salvar em 800x400 em .svg
rsvg::rsvg_pdf('figures/PLOT_QQPLOT.svg',"figures/PLOT_QQPLOT.pdf")
rsvg::rsvg_eps('figures/PLOT_QQPLOT.svg',"figures/PLOT_QQPLOT.eps")

labs_degrada01 <- c("Time","Degradation")
gera_plot_degrada01(labs_degrada01)


# ------------------------------------------------
######## Gerando Curvas de Confiabilidade ########
# ------------------------------------------------

# gera curvas de confiabilidade considerando fdp do first passage time (fpt) como gaussiana inversa
# media: media
# variancia: (media^3)/desvio

# t0: tempo inicial
t0 = 39

# x0: degradação para o tempo inicial
x0 = subset_bagfilter %>%
  filter(Time == 39) %>%
  filter(Y == min(Y)) %>%
  select(Y) %>%
  pull()

plot_reliability(mu=mu,sigma2=sigma2,
                 alpha = 150,
                 t0 = t0,
                 x0 = x0,
                 t_max = c(150+5),
                 xlab = "Tempo",ylab = "Confiabilidade (%)",
                 paleta = "taylor1989")

# Salvar em 1100x500 em .svg
rsvg::rsvg_pdf('figures/RESULT_002.svg',"figures/RESULT_002.pdf")

plot_maintanance(subset_bagfilter,ylab="Differential [mmWC] ",xlab="Time",time = TRUE)
# Salvar em 800x400 em .svg
rsvg::rsvg_pdf('figures/RESULT_001.svg',"figures/RESULT_001.pdf")
rsvg::rsvg_eps('figures/RESULT_001.svg',"figures/RESULT_001.eps")

gera_plot_confiabilidade()
# Salvar em 600x350 em .svg
rsvg::rsvg_pdf('figures/CONFIABILIDADE_001.svg',"figures/CONFIABILIDADE_001.pdf")


plot_reliability_ic(mu=mu,sigma2=sigma2,
                 var_mu=var_mu,var_sigma2,
                 alpha = 150,
                 t0 = t0,
                 x0 = x0,
                 t_max = c(150+5),
                 xlab = "Time",ylab = "Reliability (%)",
                 paleta = "taylor1989")$p
# Salvar em 600x300 em .svg
rsvg::rsvg_pdf('figures/RELIABILITY_IC_001.svg',"figures/RELIABILITY_IC_001.pdf")
rsvg::rsvg_eps('figures/RELIABILITY_IC_001.svg',"figures/RELIABILITY_IC_001.eps")

###########################################################
### Comparação modelo wiener original - modelo proposto ###
###########################################################

k <- subset_bagfilter |>
  filter(duplicated(Time)) |>
  select(Time) %>% pull()

rho = rho_hat(subset_bagfilter)
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


rho = rep(mean(rho_hat(subset_bagfilter)),3)
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

df_ic <- plot_reliability_ic(mu=mu,sigma2=sigma2,
                             var_mu=var_mu,var_sigma2,
                             alpha = 150,
                             t0 = t0,
                             x0 = x0,
                             t_max = c(150+5),
                             xlab = "Time",ylab = "Reliability (%)",
                             paleta = "taylor1989")$df_visu

df_ic <- df_ic %>%
  mutate(lower = round(lower*100,2),
         upper = round(upper*100,2))

View(df_ic)
