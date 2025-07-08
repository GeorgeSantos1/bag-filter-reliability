
# ---------------------------------------------------
# Arquivo: script_210824-DESKTOP-RKA95E1.r
# Descrição: Conjunto de funções úteis para análise
# Autor: George Anderson A. dos Santos
# Data: 17-04-2024
# ---------------------------------------------------
# ---------------------------------------------------
# Carregamento de Pacotes
pacotes <- c(
  "dplyr", "tidyr", "ggplot2", "gridExtra", "viridisLite", "SimDesign", "ggh4x",
  "latex2exp", "scales", "ggthemes", "viridis", "ggrepel", "readxl",
  "lubridate", "statmod", "tayloRswift", "WriteXLS", "gt", "forcats",
  "patchwork", "grid"
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
df_degradacao1 <- gera_dados(n_obs=1,t_max=20,n_med=100,v=2,sigma = sqrt(2))
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
df_degradacao1 <-  gera_dados1(t_max = 20,n_med=20,v=2,sigma=sqrt(2),
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
df_degradacao1 <- gera_dados3(n_s=10,t_max = 20,n_med=n_med,v=4,sigma2 = 4,rho=rho,n_manu=n_manu)
mle_drift1(df_degradacao1)
mle_sigma1(df_degradacao1) #  entre 1 e 2 minutos para rodar

# ---------------------------------------------------------------------
#################### Estudo de Simulação #############################
# ---------------------------------------------------------------------
Design <- SimDesign::createDesign(n_system = c(10,50,100,200),
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
# saveRDS(resultados,file = "SimDesign3.rds")
resultados <- readRDS("SimDesign3.rds")


# ---------------------------------------------------------------------
############################ Graficos #################################
# Grafico: Caminhos de degradação
# ---------------------------------------------------------------------						

t <- seq(0,10,by=2)							
set.seed(12)							
degrad1 <- c(0,cumsum(rexp(n=length(t)-1,1/3)))							
degrad2 <- c(0,cumsum(rexp(n=length(t)-1,1/6)))							
degrad3 <- c(0,cumsum(rexp(n=length(t)-1,1/9)))							

data.frame(time = t,Unit1 = degrad1,Unit2 = degrad2,Unit3 = degrad3) %>%							
  tidyr::gather(key="unit",value="deg",-time) %>%							
  ggplot(aes(x=time,y=deg,group = unit,colour = unit)) +							
  geom_point(size=1.5,colour="black") +							
  geom_line(linewidth=1,alpha=0.7) +							
  theme_classic() +							
  theme(legend.title = element_blank(),							
        plot.title = element_blank(),							
        legend.position = c(0.08,0.9)) +							
  labs(x = "Time", y = "Degradation") +							
  scale_x_continuous(expand = c(0, 0),breaks = c(0,2,4,6,8,10),limits = c(0,11)) +							
  scale_y_continuous(expand = c(0, 0), limits = c(0,65)) +							
  tayloRswift::scale_color_taylor()							

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
index[seq(1,length(index),by=n_intra_manu-1)]

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
sub_maria <- df_aux
names(sub_maria)[5] <- "Y"
sub_maria <- sub_maria %>%
  mutate(Time = Time -1,
         Objeto = "OBJ_001")

# gerando grafico e estimando parametros
plot_maintanance(sub_maria,ylab="Degradation/Diferencial",xlab="Time",time = TRUE)
rsvg::rsvg_pdf('figures/RESULT_001.svg',"figures/RESULT_001.pdf")
mu <- mle_drift1_y(sub_maria)
sigma <- mle_sigma1_y(sub_maria)
rho_hat(sub_maria)

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

# t0: tempo inicial
t0 = 39

# x0: degradação para o tempo inicial
x0 = sub_maria %>%
  filter(Time == 39) %>%
  filter(Y == min(Y)) %>%
  select(Y) %>%
  pull()

plot_reliability(mu=mu,sigma=sigma,
                    alpha = 150,
                    t0 = t0,
                    x0 = x0,
                    t_max = c(150+5),
                    xlab = "Time",ylab = "Reliability = R(t)",
                    paleta = "taylor1989")

rsvg::rsvg_pdf('figures/RESULT_002.svg',"figures/RESULT_002.pdf")

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

gera_plot_exp <- function(){
  # Parâmetros
  tempos <- seq(0, 5, length.out = 100)
  lambdas <- c(0.5, 1.0, 1.5)
  
  # Geração dos dados
  dados <- expand.grid(tempo = tempos, lambda = lambdas) %>%
    mutate(
      R_t = exp(-lambda * tempo),
      lambda_t = lambda,
      ft_t = dexp(tempo,lambda)
    )
  
  # Converter lambda em fator para legenda
  dados$lambda_f <- factor(dados$lambda, labels = c("α = 0.5", "α = 1.0", "α = 1.5"))
  
  # Gráfico de Confiabilidade
  g1 <- ggplot(dados, aes(x = tempo, y = ft_t, color = lambda_f, linetype = lambda_f)) +
    geom_line(size = 1) +
    labs(title = "Densidade", x = "Tempo", y = "f(t)", color = "", linetype = "") +
    theme_classic() +
    theme(legend.title = element_blank(),
          legend.position = c(0.9,0.9),
          plot.title = element_text(hjust = 0.5)) +
    scale_x_continuous(expand = c(0, 0)) +							
    scale_y_continuous(expand = c(0, 0)) +
    tayloRswift::scale_color_taylor()
  
  # Gráfico de Taxa de Falha
  g3 <- ggplot(dados, aes(x = tempo, y = lambda_t, color = lambda_f, linetype = lambda_f)) +
    geom_line(size = 1) +
    labs(title = "Taxa de Falha", x = "Tempo", y = "λ(t)", color = "", linetype = "") +
    theme_classic() +
    theme(legend.position="none",
          plot.title = element_text(hjust = 0.5)) +
    scale_x_continuous(expand = c(0, 0)) +							
    scale_y_continuous(expand = c(0, 0),limits = c(0.4,1.6)) +
    tayloRswift::scale_color_taylor()
  
  # Gráfico de Taxa de Falha Acumulada
  g2 <- ggplot(dados, aes(x = tempo, y = R_t, color = lambda_f, linetype = lambda_f)) +
    geom_line(size = 1) +
    labs(title = "Confiabilidade", x = "Tempo", y = "R(t)", color = "", linetype = "") +
    theme_classic() +
    theme(legend.position="none",
          plot.title = element_text(hjust = 0.5)) +
    scale_x_continuous(expand = c(0, 0)) +							
    scale_y_continuous(expand = c(0, 0)) +
    tayloRswift::scale_color_taylor()
  
  # Combinar os gráficos lado a lado
  layout <- "
  AAAABBBB
  ##CCCC##
  "
  g1+g2+g3 +
    plot_layout(design = layout)
}

gera_plot_weibull <- function(){
  tempos <- seq(0, 5, length.out = 100)
  gammas <- c(0.5, 1.0, 1.5)
  alpha <- 1
  
  dados <- expand.grid(t = tempos, gamma = gammas, alpha = alpha)
  
  # Compute R(t), lambda(t), and Lambda(t)
  dados$R_t <- exp(-(dados$t/dados$alpha)^dados$gamma)  # Reliability function
  dados$lambda_t <- (dados$gamma/(dados$alpha^dados$gamma)) * (dados$t)^(dados$gamma - 1)  # Instantaneous failure rate
  dados$ft_t <- dweibull(dados$t,dados$gamma,dados$alpha)  # Cumulative failure rate
  
  # Replace Inf/NaN in lambda_t with 0 for t = 0 (for gamma < 1)
  # dados$lambda_t[data$t == 0 & data$gamma < 1] <- 0
  dados$lambda_f <- factor(dados$gamma, labels = c("γ = 0.5, α = 1", "γ = 1.0, α = 1", "γ = 1.5, α = 1"))
  
  
  g1 <- ggplot(dados, aes(x = t, y = ft_t, color = lambda_f, linetype = lambda_f)) +
    geom_line(size = 1) +
    labs(title = "Densidade", x = "Tempo", y = "f(t)", color = "", linetype = "") +
    theme_classic() +
    theme(legend.title = element_blank(),
          legend.position = c(0.85,0.85),
          plot.title = element_text(hjust = 0.5)) +
    scale_x_continuous(expand = c(0, 0)) +							
    scale_y_continuous(expand = c(0, 0)) +
    tayloRswift::scale_color_taylor()
  
  # Gráfico de Taxa de Falha
  g3 <- ggplot(dados, aes(x = t, y = lambda_t, color = lambda_f, linetype = lambda_f)) +
    geom_line(size = 1) +
    labs(title = "Taxa de Falha", x = "Tempo", y = "λ(t)", color = "", linetype = "") +
    theme_classic() +
    theme(legend.position="none",
          plot.title = element_text(hjust = 0.5)) +
    scale_x_continuous(expand = c(0, 0)) +							
    scale_y_continuous(expand = c(0, 0)) +
    tayloRswift::scale_color_taylor()
  
  # Gráfico de Distribuição
  g2 <- ggplot(dados, aes(x = t, y = R_t, color = lambda_f, linetype = lambda_f)) +
    geom_line(size = 1) +
    labs(title = "Confiabilidade", x = "Tempo", y = "R(t)", color = "", linetype = "") +
    theme_classic() +
    theme(legend.position="none",
          plot.title = element_text(hjust = 0.5)) +
    scale_x_continuous(expand = c(0, 0)) +							
    scale_y_continuous(expand = c(0, 0)) +
    tayloRswift::scale_color_taylor()
  
  
  layout <- "
  AAAABBBB
  ##CCCC##
  "
  g1+g2+g3 +
    plot_layout(design = layout)
}

gera_plot_lognormal <- function(){
  tempos <- seq(0, 5, length.out = 100)
  sigma <- c(0.5, 1.0, 1.5)
  mu <- 0
  
  dados <- expand.grid(t = tempos, sigma = sigma, mu = mu)
  
  # Compute R(t), lambda(t), and Lambda(t)
  dados$R_t <- stats::plnorm(dados$t, meanlog = dados$mu, sdlog = dados$sigma,lower.tail = FALSE)  # Reliability function
  dados$ft_t <- stats::dlnorm(dados$t, meanlog = dados$mu, sdlog = dados$sigma)  # Cumulative failure rate
  dados$lambda_t <- dados$ft_t/dados$R_t  # Instantaneous failure rate
  
  # Replace Inf/NaN in lambda_t with 0 for t = 0 (for gamma < 1)
  # dados$lambda_t[data$t == 0 & data$gamma < 1] <- 0
  dados$lambda_f <- factor(dados$sigma, labels = c("μ = 0, σ = 0.5", "μ = 0, σ = 1.0", "μ = 0, σ = 1.5"))
  
  
  g1 <- ggplot(dados, aes(x = t, y = ft_t, color = lambda_f, linetype = lambda_f)) +
    geom_line(size = 1) +
    labs(title = "Densidade", x = "Tempo", y = "f(t)", color = "", linetype = "") +
    theme_classic() +
    theme(legend.title = element_blank(),
          legend.position = c(0.85,0.85),
          plot.title = element_text(hjust = 0.5)) +
    scale_x_continuous(expand = c(0, 0)) +							
    scale_y_continuous(expand = c(0, 0),limits = c(0,1)) +
    tayloRswift::scale_color_taylor()
  
  # Gráfico de Taxa de Falha
  g3 <- ggplot(dados, aes(x = t, y = lambda_t, color = lambda_f, linetype = lambda_f)) +
    geom_line(size = 1) +
    labs(title = "Taxa de Falha", x = "Tempo", y = "λ(t)", color = "", linetype = "") +
    theme_classic() +
    theme(legend.position="none",
          plot.title = element_text(hjust = 0.5)) +
    scale_x_continuous(expand = c(0, 0)) +							
    scale_y_continuous(expand = c(0, 0),limits = c(0,2)) +
    tayloRswift::scale_color_taylor()
  
  # Gráfico de Distribuição
  g2 <- ggplot(dados, aes(x = t, y = R_t, color = lambda_f, linetype = lambda_f)) +
    geom_line(size = 1) +
    labs(title = "Confiabilidade", x = "Tempo", y = "R(t)", color = "", linetype = "") +
    theme_classic() +
    theme(legend.position="none",
          plot.title = element_text(hjust = 0.5)) +
    scale_x_continuous(expand = c(0, 0)) +							
    scale_y_continuous(expand = c(0, 0)) +
    tayloRswift::scale_color_taylor()
  
  
  layout <- "
  AAAABBBB
  ##CCCC##
  "
  g1+g2+g3 +
    plot_layout(design = layout)
}

plot_censura_all <- function(){
  # Simulando dados base
  dados <- tibble::tibble(
    paciente = rep(1:6, 4),
    tempo_inicial = 0,
    tempo_final = c(6, 10, 14, 12, 16, 18,
                    6, 20, 20, 12, 16, 20,
                    6, 10, 20, 12, 20, 20,
                    9, 20, 14, 12, 20, 7),
    evento = c(1, 1, 1, 1, 1, 1,
               1, 0, 0, 1, 1, 0,
               1, 1, 1, 1, 0, 0,
               1, 0, 1, 0, 0, 0),
    tipo = rep(c(
      "(a) Dados completos",
      "(b) Dados com censura tipo I",
      "(c) Dados com censura tipo II",
      "(d) Dados com censura aleatória"
    ), each = 6)
  )
  
  # Gráfico base para cada cenário
  plot_censura <- function(tipo_plot) {
    df <- filter(dados, tipo == tipo_plot)
    ggplot(df, aes(y = paciente)) +
      geom_segment(aes(x = tempo_inicial, xend = tempo_final, yend = paciente), size = 0.6) +
      geom_point(aes(x = tempo_final, shape = factor(evento)), size = 2) +
      scale_shape_manual(values = c(`0` = 1, `1` = 16)) +
      scale_y_reverse(breaks = 1:6) +
      coord_cartesian(xlim = c(0, 22)) +
      geom_vline(xintercept = 20, linetype = "dotted") +
      annotate("text", x = 16, y = 1.5, label = "Final do Experimento", hjust = 0, size = 2) +
      labs(x = "Tempos", y = "Equipamentos", title = tipo_plot) +
      theme_classic() +
      scale_x_continuous(expand = c(0.01, 0.01)) +
      theme(plot.title = element_text(hjust = 0.5),
            legend.position = "none")
  }
  
  # Criar os 4 gráficos
  g1 <- plot_censura("(a) Dados completos")
  g2 <- plot_censura("(b) Dados com censura tipo I")
  g3 <- plot_censura("(c) Dados com censura tipo II")
  g4 <- plot_censura("(d) Dados com censura aleatória")
  
  # Combinar com patchwork
  (g1 + g2) /
    (g3 + g4)
}

gera_plot_degrada <- function(){
  set.seed(123)
  time <- seq(0, 10, length.out = 100)
  degradation <- cumsum(rnorm(100, mean = 0.2, sd = 0.5))
  degradation <- degradation - min(degradation)  # garantir valores positivos
  
  # Definir threshold de falha
  failure_threshold <- 20
  
  # Encontrar o primeiro índice onde ultrapassa o threshold
  failure_index <- which(degradation >= failure_threshold)[1]
  
  # Interpolação linear entre os dois pontos vizinhos
  if (failure_index > 1) {
    x1 <- time[failure_index - 1]
    x2 <- time[failure_index]
    y1 <- degradation[failure_index - 1]
    y2 <- degradation[failure_index]
    
    # fórmula da interpolação linear
    failure_time <- x1 + (failure_threshold - y1) * (x2 - x1) / (y2 - y1)
    failure_level <- failure_threshold
  } else {
    # Caso ultrapasse logo no primeiro ponto
    failure_time <- time[failure_index]
    failure_level <- degradation[failure_index]
  }
  
  # Criar o data frame
  df <- data.frame(time = time, degradation = degradation)
  
  # Gerar o gráfico
  p <- ggplot(df, aes(x = time, y = degradation)) +
    geom_line(color = tayloRswift::swift_palettes$taylor1989[1],size = 1) +
    geom_hline(yintercept = failure_threshold, color = tayloRswift::swift_palettes$taylor1989[6], linetype = "dotdash", size = 1) +
    geom_point(aes(x = failure_time, y = failure_level), color = "red", size = 3) +
    annotate("text", x = 2, y = failure_threshold -5, label = "Limiar de Falha", hjust = 0, angle = 0) +
    annotate("segment", x = 2.6, xend = 3, y = failure_threshold-4.5, yend = failure_threshold-0.5, 
             arrow = arrow(length = unit(0.2,"cm")), color = "black",linewidth = 1) +
    annotate("text", x = 6, y = 10, label = "Caminho de Degradação", hjust = 0, angle = 0) +
    annotate("segment", x = 7, xend = 6.8, y = 10.5, yend = 14, 
             arrow = arrow(length = unit(0.2,"cm")), color = "black",linewidth = 1) +
    annotate("text", x = 7.5, y = failure_level + 2, label = "Tempo de Falha", hjust = 0, angle = 0) +
    annotate("segment", x = 8.5, xend = failure_time-0.15, y = failure_level+1.5, yend = failure_level+0.5, 
             arrow = arrow(length = unit(0.2,"cm")), color = "black",linewidth = 1) +
    labs(x = "Tempo", y = "Nível de Degradação") +
    scale_x_continuous(expand = c(0, 0),limits = c(0,10.2)) +							
    scale_y_continuous(expand = c(0, 0),limits = c(0,25))+
    theme_classic()+
    tayloRswift::scale_color_taylor()
  print(p)
}

gera_plot_banheira <- function(){
  # Domínio do tempo
  t <- seq(0, 100, length.out = 500)
  
  # Parâmetros para simetria
  a <- 0.025
  b <- 1.8
  
  # Definir lambda(t)
  lambda <- case_when(
    t < 30 ~ 1 + a * (30 - t)^b,       # Mortalidade infantil (espelho)
    t >= 30 & t <= 70 ~ 1,             # Vida operacional
    t > 70 ~ 1 + a * (t - 70)^b        # Obsolescência
  )
  
  df <- data.frame(t = t, lambda = lambda + 10)
  
  # Gráfico
  ggplot(df, aes(x = t, y = lambda)) +
    geom_line(color = tayloRswift::swift_palettes$taylor1989[6], linewidth = 1) +
    geom_vline(xintercept = c(30, 70), linetype = "dashed") +
    annotate("text", x = 14, y = 20, label = "Mortalidade\nInfantil", hjust = 0) +
    annotate("text", x = 50, y = 14, label = "Vida Operacional", hjust = 0.5) +
    annotate("text", x = 74, y = 20, label = "Obsolescência", hjust = 0) +
    labs(
      x = "Tempo",
      y = expression(lambda(t))
    ) +
    theme_classic() +
    scale_y_continuous(expand = c(0, 0),limits = c(5,25)) +
    theme(axis.text = element_blank())
}

gera_plot_wiener <- function(){
  set.seed(123)
  df_aux <- gera_dados(n_obs=1,t_max=20,n_med=100,v=0,sigma = 4)
  df_aux1 <- gera_dados(n_obs=1,t_max=20,n_med=100,v=5,sigma = 4)
  
  df_aux$v <- "0"
  df_aux1$v <- "5"
  
  df<- rbind(df_aux,df_aux1)
  df$v <- factor(df$v,labels = c("μ = 0, σ = 4", "μ = 5, σ = 4"))
  
  g1 <- ggplot(df,aes(x=Time,y=Wt,colour = v)) +
    geom_line(size=1,alpha=0.9) +
    labs(x = "Tempo", y = "Degradação", color = "") +
    theme_classic() +
    theme(legend.title = element_blank(),
          legend.position = c(0.15,0.85)) +
    tayloRswift::scale_color_taylor() +
    scale_x_continuous(expand = c(0, 0),limits = c(0,20.5))
  
  print(g1)
}

gera_plot_reparos <- function(){
  aux <- data.frame(x= c(0,4,4,8),y=c(0,0.5,0.0,0.5))							
  g1 <- aux %>%							
    ggplot(aes(x=x,y=y)) +							
    geom_line(color = tayloRswift::swift_palettes$taylor1989[6],size = 1) +							
    theme_classic() +
    theme(legend.position="none",
          plot.title = element_text(hjust = 0.5)) +
    labs(x = "Tempo",y="Degradação",title = "Reparo Perfeito") +							
    scale_x_continuous(expand = c(0, 0)) + 
    scale_y_continuous(expand = c(0, 0),limits = c(0, 1))
  
  
  aux2 <- data.frame(x= c(0,8),y=c(0,1))							
  g2 <- aux2 %>%							
    ggplot(aes(x=x,y=y)) +							
    geom_line(color = tayloRswift::swift_palettes$taylor1989[6],size = 1) +							
    theme_classic() +
    theme(legend.position="none",
          plot.title = element_text(hjust = 0.5)) +
    labs(x = "Tempo",y="Degradação",title = "Reparo Mínimo") +							
    scale_x_continuous(expand = c(0, 0)) + 
    scale_y_continuous(expand = c(0, 0),limits = c(0, 1)) +							
    geom_line(aes(x=c(4,4),y=c(0,0.5)),linetype = 3,linewidth=1,
              color = tayloRswift::swift_palettes$taylor1989[3]) +
    tayloRswift::scale_color_taylor()
  
  
  aux3 <- data.frame(x= c(0,4,4,8),y=c(0,0.5,0.2,0.7))							
  line_data <- data.frame(x=c(4,4), y=c(0,0.2))							
  
  g3 <- aux3 %>%							
    ggplot(aes(x=x, y=y)) +							
    geom_line(color = tayloRswift::swift_palettes$taylor1989[6],size = 1) +							
    geom_line(data = line_data, aes(x=x, y=y),linetype = 3,linewidth=1,
              color = tayloRswift::swift_palettes$taylor1989[3]) +							
    theme_classic() +
    theme(legend.position="none",
          plot.title = element_text(hjust = 0.5)) +
    labs(x = "Tempo", y = "Degradação", title = "Reparo Imperfeito") +							
    scale_x_continuous(expand = c(0, 0)) +							
    scale_y_continuous(expand = c(0, 0), limits = c(0, 1)) +
    tayloRswift::scale_color_taylor()
  
  layout <- "
  AAAABBBB
  ##CCCC##
  "
  g1+g2+g3 +
    plot_layout(design = layout)
}

gera_plot_scheme <- function(){
  set.seed(1111)							
  rho<- c(0.6, 0.6)							
  n_manu <- 2							
  intra_manu <- 4							
  n_med <- (n_manu+1)*(intra_manu+2)-(n_manu+1)							
  df_degradacao1 <- gera_dados3(n_s=1,t_max = 15,n_med=n_med,v=2,sigma=sqrt(2),rho=rho,n_manu=n_manu)							
  df_degradacao1 %>%							
    ggplot(aes(x=Time,y=Y)) +							
    geom_point(size=2,colour=tayloRswift::swift_palettes$taylor1989[6]) +							
    geom_line(linewidth=1,colour=tayloRswift::swift_palettes$taylor1989[6]) +							
    geom_text(x=(1+0.5), y= (df_degradacao1 %>% filter(Time==1) %>% select(Y) %>% min())-0.5,							
              label=TeX("$\\Delta Y_{0,1}$"),size = 4,colour="red")+							
    geom_text(x=(2+0.5), y= (df_degradacao1 %>% filter(Time==2) %>% select(Y) %>% min())-0.5,							
              label=TeX("$\\Delta Y_{0,2}$"),size = 4,colour="red")+							
    geom_text(x=(4-0.3), y= (df_degradacao1 %>% filter(Time==4) %>% select(Y) %>% min())+1,							
              label=TeX("$\\Delta Y_{0,n_0 + 1}$"),size = 4,colour="red")+							
    geom_text(x=(5-0.5), y= (df_degradacao1 %>% filter(Time==5) %>% select(Y) %>% min())-0.5,							
              label=TeX("$Y(\\tau_{1}^{+})$"),size=4,colour="red") +							
    geom_text(x=(5+0.6), y= df_degradacao1 %>% filter(Time==5) %>% summarise(y_mean = mean(Y)) %>% pull(),							
              label=TeX("$Z_1$"),size=4,colour="red") +							
    geom_text(x=(5-0.5), y= (df_degradacao1 %>% filter(Time==5) %>% select(Y) %>% max()) + 0.5,							
              label=TeX("$Y(\\tau_{1}^{-})$"),size=4,colour="red") +							
    geom_text(x=10, y= (df_degradacao1 %>% filter(Time==10) %>% select(Y) %>% min())-0.7,							
              label=TeX("$Y(\\tau_{2}^{+})$"),size=4,colour="red") +							
    geom_text(x=(10+0.6), y= df_degradacao1 %>% filter(Time==10) %>% summarise(y_mean = mean(Y)) %>% pull(),							
              label=TeX("$Z_2$"),size=4,colour="red") +							
    geom_text(x=10, y= (df_degradacao1 %>% filter(Time==10) %>% select(Y) %>% max())+0.7,							
              label=TeX("$Y(\\tau_{2}^{-})$"),size=4,colour="red") +							
    geom_text(x=(15-0.7), y= (df_degradacao1 %>% filter(Time==15) %>% select(Y)) %>% pull(),							
              label=TeX("$Y(\\tau_{3}^{-})$"),size=4,colour="red") +							
    annotate("segment", x = (5+0.3), y = df_degradacao1 %>% filter(Time==5) %>% select(Y) %>% min(),							
             xend = (5+0.3), yend = df_degradacao1 %>% filter(Time==5) %>% select(Y) %>% max(), size = 1, ,colour="red",							
             arrow = arrow(type = "open", ends = "both", angle = 20, length = unit(0.4, "cm"))) +							
    annotate("segment", x = (10+0.3), y = df_degradacao1 %>% filter(Time==10) %>% select(Y) %>% min(),							
             xend = (10+0.3), yend = df_degradacao1 %>% filter(Time==10) %>% select(Y) %>% max(), size = 1,colour="red",							
             arrow = arrow(type = "open", ends = "both", angle = 20, length = unit(0.4, "cm"))) +							
    theme_classic() +							
    theme(plot.title = element_blank()) +							
    scale_y_continuous(expand = c(0, 0), limits = c(0,20)) +							
    scale_x_continuous(expand = c(0, 0), limits = c(0,16),breaks = c(5,10)) +							
    labs(x = "Tempo", y = "Degradação")
}

gera_plot_bias <- function(resultados){
  mu.labs <- c(TeX("$mu$"),"mu=16")
  names(mu.labs) <- c("4","16")
  
  sigma2.labs <- c("Sigma = 1","Sigma = 25")
  names(sigma2.labs) <- c("1","25")
  
  n_main.labs <- c("k=3","k=4","k=5")
  names(n_main.labs) <- c("3","4","5")
  
  n_intra.labs <- c("nj=0","nj=2","nj=4")
  names(n_intra.labs) <- c("0","2","4")
  
  # Bias
  resultados %>%
    select(n_system:bias.sigma_hat) %>%
    tidyr::gather(bias,value,bias.mu_hat,bias.sigma_hat) %>%
    mutate(mu = as.factor(mu) %>% recode_factor("4" = "mu : 4" ,"16" = "mu : 16"),
           sigma = as.factor(sigma2) %>% forcats::fct_recode("sigma^2 : 1" = "1" ,"sigma^2 : 25" = "25"),
           n_main = as.factor(n_main) %>% forcats::fct_recode("k : 3" = "3" ,"k : 4" = "4", "k : 5" = "5"),
           n_intra = as.factor(n_intra) %>% forcats::fct_recode("n[j] : 0" = "0" ,"n[j] : 2" = "2", "n[j] : 4" = "4")) %>%
    ggplot(aes(x=n_system,y=value,color = bias)) +
    facet_nested(mu+sigma ~n_main+n_intra,
                 labeller = label_parsed) +
    geom_line(linewidth=0.8,alpha=0.6) +
    geom_point(alpha=0.7) +
    labs(x = "Número de Sistemas",
         y = "Viés") +
    tayloRswift::scale_color_taylor(labels = c(TeX(" $mu$    "),TeX(" $sigma^2$")))	+
    theme(legend.position = "bottom",
          legend.title = element_blank(),
          legend.text = element_text(colour="black", size = 20),
          legend.key = element_rect(colour = NA, fill = NA),
          panel.background = element_blank(),
          panel.border = element_rect(fill = "transparent",
                                      color = "black", linewidth = 0.5),
          strip.background = element_rect(linetype = "solid",
                                          color = "black", linewidth = 0.5),
          strip.text.y = ggplot2::element_text(angle=0))
  
}

gera_plot_xtyt <- function(){
  set.seed(111)
  rho<- c(1, 0.3, 0.5)
  n_manu <- 3
  intra_manu <- 4
  n_med <- (n_manu+1)*(intra_manu+2)-(n_manu+1)
  data <- gera_dados3(n_s=1,t_max = 20,n_med=n_med,v=3,sigma2=2,rho=rho,n_manu=n_manu)
  
  # Ordena o data.frame por Time (e por outro critério se necessário)
  data <- data %>% arrange(Time)
  
  # Cria lista de índices onde Time é duplicado (2ª ocorrência)
  duplicated_times <- data$Time[duplicated(data$Time)]
  
  # Cria uma nova base com quebra usando NA logo após o primeiro ponto duplicado
  data_na <- data.frame()
  i <- 1
  while (i <= nrow(data)) {
    current_row <- data[i, ]
    data_na <- bind_rows(data_na, current_row)
    
    # Se o próximo tiver o mesmo Time → insere linha NA
    if (i < nrow(data) && data$Time[i] == data$Time[i + 1]) {
      na_row <- current_row
      na_row$Y <- NA
      data_na <- bind_rows(data_na, na_row)
    }
    
    i <- i + 1
  }
  
  # Gera o gráfico com a linha quebrada
  p <- ggplot() +
    geom_line(
      data = data_na,
      aes(x = Time, y = Y, color = "Degradation Path"),
      alpha = 0.5, linetype = "solid", linewidth = 1
    ) + 
    geom_line(data = data, aes(x = Time, y = Wt, colour = "Standard"),
              alpha = 0.5, linetype = "solid", linewidth = 1)
  
  # Adiciona os segmentos verticais nos pontos duplicados
  for (ponto in duplicated_times) {
    y_vals <- data$Y[data$Time == ponto]
    p <- p +
      geom_segment(
        data = data.frame(x = ponto, xend = ponto, y = max(y_vals), yend = min(y_vals)),
        aes(x = x, xend = xend, y = y, yend = yend),
        linetype = "dotted", linewidth = 1, colour = tayloRswift::swift_palettes$taylor1989[4],
      )
  }
  
  p <- p +
    scale_color_manual(
      name = NULL,
      values = c(
        "Degradation Path" = tayloRswift::swift_palettes$taylor1989[1],
        "Standard" = tayloRswift::swift_palettes$taylor1989[6]
      ),
      labels = c("Y(t) - Processo de degradação com ações de manutenção", "X(t) - Processo de degradação natural")
    ) +
    theme_classic() +
    theme(
      legend.position = "top",
      plot.title = element_blank()
    ) +
    labs(x = "Tempo", y = "Degradação", title = "(I)") +
    scale_y_continuous(expand = c(0, 0)) +
    scale_x_continuous(expand = c(0, 0))
  
  return(p)
}

gera_plot_rmse <- function(resultados){
  mu.labs <- c(TeX("$mu$"),"mu=16")
  names(mu.labs) <- c("4","16")
  
  sigma2.labs <- c("Sigma = 1","Sigma = 25")
  names(sigma2.labs) <- c("1","25")
  
  n_main.labs <- c("k=3","k=4","k=5")
  names(n_main.labs) <- c("3","4","5")
  
  n_intra.labs <- c("nj=0","nj=2","nj=4")
  names(n_intra.labs) <- c("0","2","4")
  
  # RMSE
  resultados %>%
    select(n_system:n_intra,RMSE.mu_hat,RMSE.sigma_hat) %>%
    tidyr::gather(rmse,value,RMSE.mu_hat,RMSE.sigma_hat) %>%
    mutate(mu = as.factor(mu) %>% recode_factor("4" = "mu : 4" ,"16" = "mu : 16"),
           sigma = as.factor(sigma2) %>% forcats::fct_recode("sigma^2 : 1" = "1" ,"sigma^2 : 25" = "25"),
           n_main = as.factor(n_main) %>% forcats::fct_recode("k : 3" = "3" ,"k : 4" = "4", "k : 5" = "5"),
           n_intra = as.factor(n_intra) %>% forcats::fct_recode("n[j] : 0" = "0" ,"n[j] : 2" = "2", "n[j] : 4" = "4")) %>%
    ggplot(aes(x=n_system,y=value,color = rmse)) +
    facet_nested(mu+sigma ~n_main+n_intra,
                 labeller = label_parsed) +
    geom_line(linewidth=0.8,alpha=0.6) +
    geom_point(alpha=0.7) +
    labs(x = "Número de Sistemas",
         y = "RMSE") +
    tayloRswift::scale_color_taylor(labels = c(TeX(" $mu$    "),TeX(" $sigma^2$")))	+
    theme(legend.position = "bottom",
          legend.title = element_blank(),
          legend.text = element_text(colour="black", size = 20),
          legend.key = element_rect(colour = NA, fill = NA),
          panel.background = element_blank(),
          panel.border = element_rect(fill = "transparent",
                                      color = "black", linewidth = 0.5),
          strip.background = element_rect(linetype = "solid",# fill = tayloRswift::swift_palettes$taylor1989[],
                                          color = "black", linewidth = 0.5),
          strip.text.y = ggplot2::element_text(angle=0))
}

gera_plot_coveragep <- function(resultados){
  mu.labs <- c(TeX("$mu$"),"mu=16")
  names(mu.labs) <- c("4","16")
  
  sigma2.labs <- c("Sigma = 1","Sigma = 25")
  names(sigma2.labs) <- c("1","25")
  
  n_main.labs <- c("k=3","k=4","k=5")
  names(n_main.labs) <- c("3","4","5")
  
  n_intra.labs <- c("nj=0","nj=2","nj=4")
  names(n_intra.labs) <- c("0","2","4")
  
  resultados %>%
    select(n_system:n_intra,CP_mu_hat,CP_sigma2_hat) %>%
    tidyr::gather(CP,value,CP_mu_hat,CP_sigma2_hat) %>%
    mutate(mu = as.factor(mu) %>% recode_factor("4" = "mu : 4" ,"16" = "mu : 16"),
           sigma = as.factor(sigma2) %>% forcats::fct_recode("sigma^2 : 1" = "1" ,"sigma^2 : 25" = "25"),
           n_main = as.factor(n_main) %>% forcats::fct_recode("k : 3" = "3" ,"k : 4" = "4", "k : 5" = "5"),
           n_intra = as.factor(n_intra) %>% forcats::fct_recode("n[j] : 0" = "0" ,"n[j] : 2" = "2", "n[j] : 4" = "4")) %>%
    ggplot(aes(x=n_system,y=value,color = CP)) +
    facet_nested(mu+sigma ~n_main+n_intra,
                 labeller = label_parsed) +
    geom_hline(yintercept = 0.95, linetype = "dashed", color = "red", linewidth = 0.5) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    
    geom_line(linewidth=0.8,alpha=0.6) +
    geom_point(alpha=0.7) +
    labs(x = "Número de Sistemas",
         y = "Probabilidade de Cobertura (95%)") +
    tayloRswift::scale_color_taylor(labels = c(TeX(" $mu$   "),TeX(" $sigma^2$"))) +
    theme(legend.position = "bottom",
          legend.title = element_blank(),
          legend.text = element_text(colour="black", size = 20),
          legend.key = element_rect(colour = NA, fill = NA),
          panel.background = element_blank(),
          panel.border = element_rect(fill = "transparent",
                                      color = "black", linewidth = 0.5),
          strip.background = element_rect(linetype = "solid",
                                          color = "black", linewidth = 0.5),
          strip.text.y = ggplot2::element_text(angle=0))
}

gera_plot_ratiovar <- function(resultados){
  mu.labs <- c(TeX("$mu$"),"mu=16")
  names(mu.labs) <- c("4","16")
  
  sigma2.labs <- c("Sigma = 1","Sigma = 25")
  names(sigma2.labs) <- c("1","25")
  
  n_main.labs <- c("k=3","k=4","k=5")
  names(n_main.labs) <- c("3","4","5")
  
  n_intra.labs <- c("nj=0","nj=2","nj=4")
  names(n_intra.labs) <- c("0","2","4")
  
  resultados %>%
    select(n_system:n_intra,obs_ModVar_mu,obs_ModVar_sigma2,obs_EmpVar_mu,obs_EmpVar_sigma2) %>%
    mutate(ratiovar_mu = obs_ModVar_mu/obs_EmpVar_mu,
           ratiovar_sigma2 = obs_ModVar_sigma2/obs_EmpVar_sigma2) %>%
    tidyr::gather(ratio_var,value,ratiovar_mu,ratiovar_sigma2) %>%
    mutate(mu = as.factor(mu) %>% recode_factor("4" = "mu : 4" ,"16" = "mu : 16"),
           sigma = as.factor(sigma2) %>% forcats::fct_recode("sigma^2 : 1" = "1" ,"sigma^2 : 25" = "25"),
           n_main = as.factor(n_main) %>% forcats::fct_recode("k : 3" = "3" ,"k : 4" = "4", "k : 5" = "5"),
           n_intra = as.factor(n_intra) %>% forcats::fct_recode("n[j] : 0" = "0" ,"n[j] : 2" = "2", "n[j] : 4" = "4")) %>%
    ggplot(aes(x=n_system,y=value,color = ratio_var)) +
    facet_nested(mu+sigma ~n_main+n_intra,
                 labeller = label_parsed) +
    geom_line(linewidth=0.8,alpha=0.6) +
    geom_point(alpha=0.7) +
    geom_hline(yintercept = 1, linetype = "dashed", color = "red", linewidth = 0.5) +
    labs(x = "Número de Sistemas",
         y = "Razão de Variâncias") +
    tayloRswift::scale_color_taylor(labels = c(TeX(" $mu$   "),TeX(" $sigma^2$"))) +
    theme(legend.position = "bottom",
          legend.title = element_blank(),
          legend.text = element_text(colour="black", size = 20),
          legend.key = element_rect(colour = NA, fill = NA),
          panel.background = element_blank(),
          panel.border = element_rect(fill = "transparent",
                                      color = "black", linewidth = 0.5),
          strip.background = element_rect(linetype = "solid",
                                          color = "black", linewidth = 0.5),
          strip.text.y = ggplot2::element_text(angle=0))
}



gera_plot_exp()
# Salvar em 900x600 em .svg
rsvg::rsvg_pdf('figures/PLOT_EXP.svg',"figures/PLOT_EXP.pdf")

gera_plot_weibull()
# Salvar em 900x600 em .svg
rsvg::rsvg_pdf('figures/PLOT_WEIBULL.svg',"figures/PLOT_WEIBULL.pdf")

gera_plot_lognormal()
# Salvar em 900x600 em .svg
rsvg::rsvg_pdf('figures/PLOT_LOGNORMAL.svg',"figures/PLOT_LOGNORMAL.pdf")

plot_censura_all()
# Salvar em 1000x600 em .svg
rsvg::rsvg_pdf('figures/PLOT_CENSURA.svg',"figures/PLOT_CENSURA.pdf")

gera_plot_banheira()
# Salvar em 600x350 em .svg
rsvg::rsvg_pdf('figures/PLOT_BANHEIRA.svg',"figures/PLOT_BANHEIRA.pdf")

gera_plot_degrada()
# Salvar em 800x400 em .svg
rsvg::rsvg_pdf("figures/PLOT_DEGRADA001.svg","figures/PLOT_DEGRADA001.pdf")

gera_plot_wiener()
# Salvar em 600x350 em m.svg
rsvg::rsvg_pdf("figures/PLOT_WIENER.svg","figures/PLOT_WIENER.pdf")

gera_plot_reparos()
# Salvar em 900x600 em .svg
rsvg::rsvg_pdf('figures/PLOT_REPARO.svg',"figures/PLOT_REPARO.pdf")

gera_plot_scheme()
# Salvar em 800x400 em .svg
rsvg::rsvg_pdf('figures/PLOT_SCHEMA.svg',"figures/PLOT_SCHEMA.pdf")

resultados <- readRDS("SimDesign3.rds")
gera_plot_rmse(resultados)
# Salvar em 1100x600 em .svg
rsvg::rsvg_pdf('figures/PLOT_RMSE.svg',"figures/PLOT_RMSE.pdf")

resultados <- readRDS("SimDesign3.rds")
gera_plot_bias(resultados)
# Salvar em 1100x600 em .svg
rsvg::rsvg_pdf('figures/PLOT_BIAS.svg',"figures/PLOT_BIAS.pdf")

resultados <- readRDS("SimDesign3.rds")
gera_plot_coveragep(resultados)
# Salvar em 1100x600 em .svg
rsvg::rsvg_pdf('figures/PLOT_CP.svg',"figures/PLOT_CP.pdf")

resultados <- readRDS("SimDesign3.rds")
gera_plot_ratiovar(resultados)
# Salvar em 1100x600 em .svg
rsvg::rsvg_pdf('figures/PLOT_RATIOVAR.svg',"figures/PLOT_RATIOVAR.pdf")

gera_plot_xtyt()
# Salvar em 800x400 em .svg
rsvg::rsvg_pdf('figures/PLOT_XTYT.svg',"figures/PLOT_XTYT.pdf")

