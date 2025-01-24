
## Pacotes
library(dplyr)
library(tidyr)
library(ggplot2)
library(gridExtra)
library(viridisLite)
library(SimDesign)
library(ggh4x)

## Carregamento de funções
source("funcoes_170424.r")

## Gerando dados
set.seed(111)
df_degradacao1 <- gera_dados(n_obs=1,t_max=20,n_med=20,v=2,sigma = sqrt(2))
df_degradacao2 <- gera_dados(n_obs=10,t_max=20,n_med=200,v=3.5,sigma = 1)
df_degradacao3 <- gera_dados(n_obs=10,t_max=20,n_med=200,v=10,sigma = 3)
df_degradacao4 <- gera_dados(n_obs=10,t_max=20,n_med=200,v=10,sigma = 1)

## Plotando curvas de degradação
gera_plot(df_degradacao1)
gera_plot(df_degradacao2)
gera_plot(df_degradacao3)
gera_plot(df_degradacao4)

## EMV: Drift
mle_drift(data=df_degradacao1)
mle_drift(data=df_degradacao2)
mle_drift(data=df_degradacao3)
mle_drift(data=df_degradacao4)


#################################################################################
# Caso 1 do Artigo:  Statistical inference for a Wiener-based degradation model
# with imperfect maintenance actions under different
# observation schemes
#################################################################################

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

##############################################
### Different rho's per maintenance: 30-04-2024
### ARD1 (Arithmetic Reduction of Degradation)
### Usar funçao gera_dados2
##############################################

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
rho
gera_plot1(df_degradacao1)


set.seed(111)
rho<- c(1, 0, 1)
df_degradacao1 <-  gera_dados2(t_max = 20,n_med=20,v=2,sigma=sqrt(2),
                               rho=rho,n_manu=n_manu)
rho_hat(df_degradacao1)
gera_plot1(df_degradacao1)


set.seed(111)
rho<- c(1, 0, 1)
df_degradacao2 <-  gera_dados2(t_max = 20,n_med=20,v=2,sigma=sqrt(2),
                               rho=rho,n_manu=n_manu)
gera_plot1(df_degradacao2)

##############################################
### Multiple systems with Different rho's per maintenance: 10-05-2024
### ARD1 (Arithmetic Reduction of Degradation)
### Usar funçao: gera_dados3, gera_plot2, mle_drift1, mle_sigma1
##############################################

# Geraçao e estimativas dos parâmetros considerando 4 Sistemas (n_s)
set.seed(111)
rho<- c(0.5, 1, 0)
n_manu <- 3
intra_manu <- 4
n_med <- (n_manu+1)*(intra_manu+2)-(n_manu+1)
df_degradacao1 <- gera_dados3(n_s=1,t_max = 20,n_med=n_med,v=2,sigma=sqrt(2),rho=rho,n_manu=n_manu)

gera_plot2(df_degradacao1)
mle_drift1(df_degradacao1)
mle_sigma1(df_degradacao1)

mle_drift1_y(df_degradacao1)
mle_sigma1_y(df_degradacao1)
rho_hat(df_degradacao1)
# Geraçao e estimativas dos parâmetros considerando 1000 Sistemas (n_s)
set.seed(111)
df_degradacao1 <- gera_dados3(n_s=1000,t_max = 20,n_med=n_med,v=2,sigma=sqrt(2),rho=rho,n_manu=n_manu)
mle_drift1(df_degradacao1)
mle_sigma1(df_degradacao1) #  entre 1 e 2 minutos para rodar

################################################
################################################
############### Estudo de Simulação ############
################################################
################################################

Design <- SimDesign::createDesign(n_system = c(10,50,100,200),
                                  mu = c(4,16),
                                  sigma = c(1,10),
                                  n_main = c(3,4,5),
                                  n_intra = c(0,2,4))

z.CI <- function(dat,alpha = 0.95){
  xbar <- mean(dat)
  SE <- sd(dat)/sqrt(length(dat))
  z <- c(qnorm(alpha/2))
  CI <- c(xbar-z*SE, xbar + z*SE)
  CI
}

aux <- rnorm(1000,2)
z.CI(aux)


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
              t_max = 20,n_med=n_med,
              v=condition$mu,sigma=condition$sigma,rho=rho,n_manu=condition$n_main)
  dat
}

Analyse <- function(condition, dat, fixed_objects) {
  mu_hat = mle_drift1(dat)
  sigma_hat = mle_sigma1(dat)
#  z_ciMU <- z.CI(mu_hat)
#  z_ciSigma <- z.CI(sigma)
  ret <- c(mu_hat = mu_hat, sigma_hat = sigma_hat)
  ret
}

Summarise <- function(condition, results, fixed_objects) {
  # mean and SD summary of the sample means
  obs_bias <- bias(results,parameter = c(condition$mu,condition$sigma))
  obs_RMSE <- RMSE(results,parameter = c(condition$mu,condition$sigma))
  obs_MAE <- SimDesign::MAE(results,parameter = c(condition$mu,condition$sigma))
#  obs_ci_mu <- SimDesign::ECR(results$ci_mu,parameter = condition$mu)
#  obs_ci_sigma <- SimDesign::ECR(results$ci_sigma,parameter = condition$sigma)
  ret <- c(bias=obs_bias, RMSE=obs_RMSE, MAE=obs_MAE)
  ret
}

resultados <- runSimulation(design=Design, replications=1000,
                     generate=Generate, analyse=Analyse, summarise=Summarise)

# saveRDS(resultados,file = "SimDesign2.rds")
resultados <- readRDS("SimDesign2.rds")
####################################################
##################### Gráfico ######################
####################################################
library(latex2exp)
library(ggh4x)
library(scales)

mu.labs <- c(TeX("$mu$"),"mu=16")
names(mu.labs) <- c("4","16")

sigma.labs <- c("Sigma = 1","Sigma = 10")
names(sigma.labs) <- c("1","10")

n_main.labs <- c("k=3","k=4","k=5")
names(n_main.labs) <- c("3","4","5")

n_intra.labs <- c("nj=0","nj=2","nj=4")
names(n_intra.labs) <- c("0","2","4")

# RMSE
resultados %>%
  select(n_system:n_intra,RMSE.mu_hat,RMSE.sigma_hat) %>%
  tidyr::gather(rmse,value,RMSE.mu_hat,RMSE.sigma_hat) %>%
  mutate(mu = as.factor(mu) %>% recode_factor("4" = "mu : 4" ,"16" = "mu : 16"),
         sigma = as.factor(sigma) %>% forcats::fct_recode("sigma : 1" = "1" ,"sigma : 10" = "10"),
         n_main = as.factor(n_main) %>% forcats::fct_recode("k : 3" = "3" ,"k : 4" = "4", "k : 5" = "5"),
         n_intra = as.factor(n_intra) %>% forcats::fct_recode("n[j] : 0" = "0" ,"n[j] : 2" = "2", "n[j] : 4" = "4")) %>%
  ggplot(aes(x=n_system,y=value,color = rmse)) +
  # facet_nested(n_main+n_intra ~ mu+sigma,
  #              labeller = labeller(mu=mu.labs,
  #                                  sigma = sigma.labs,
  #                                  n_main = n_main.labs,
  #                                  n_intra = n_intra.labs)) +
  facet_nested(mu+sigma ~n_main+n_intra,
               labeller = label_parsed) +
  geom_line(linewidth=0.8,alpha=1) +
  geom_point(alpha=0.7) +
  labs(x = "Number of Systems",
       y = "RMSE") +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(colour="black", size = 14),
        legend.key = element_rect(colour = NA, fill = NA),
        panel.background = element_blank(),
        panel.border = element_rect(fill = "transparent",
                                    color = "black", linewidth = 0.5),
        strip.background = element_rect(linetype = "solid",
                                        color = "black", linewidth = 0.5),
        strip.text.y = ggplot2::element_text(angle=0)) +
  # scale_color_brewer(palette = "Set1",
  #                    labels = c(TeX(" $mu$    "),TeX(" $sigma$"))) +
  scale_colour_viridis_d(labels = c(TeX(" $mu$    "),TeX(" $sigma$")),option = "viridis",end = 0.8)

# Bias
resultados %>%
  select(n_system:bias.sigma_hat) %>%
  tidyr::gather(bias,value,bias.mu_hat,bias.sigma_hat) %>%
  mutate(mu = as.factor(mu) %>% recode_factor("4" = "mu : 4" ,"16" = "mu : 16"),
         sigma = as.factor(sigma) %>% forcats::fct_recode("sigma : 1" = "1" ,"sigma : 10" = "10"),
         n_main = as.factor(n_main) %>% forcats::fct_recode("k : 3" = "3" ,"k : 4" = "4", "k : 5" = "5"),
         n_intra = as.factor(n_intra) %>% forcats::fct_recode("n[j] : 0" = "0" ,"n[j] : 2" = "2", "n[j] : 4" = "4")) %>%
  ggplot(aes(x=n_system,y=value,color = bias)) +
  # facet_nested(n_main+n_intra ~ mu+sigma,
  #              labeller = labeller(mu=mu.labs,
  #                                  sigma = sigma.labs,
  #                                  n_main = n_main.labs,
  #                                  n_intra = n_intra.labs)) +
  facet_nested(mu+sigma ~n_main+n_intra,
               labeller = label_parsed) +
  geom_line(linewidth=0.8,alpha=1) +
  geom_point(alpha=0.7) +
  labs(x = "Number of Systems",
       y = "Bias") +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(colour="black", size = 14),
        legend.key = element_rect(colour = NA, fill = NA),
        panel.background = element_blank(),
        panel.border = element_rect(fill = "transparent",
                                    color = "black", linewidth = 0.5),
        strip.background = element_rect(linetype = "solid",
                                        color = "black", linewidth = 0.5),
        strip.text.y = ggplot2::element_text(angle=0)) +
  # scale_color_brewer(palette = "Set1",
  #                    labels = c(TeX(" $mu$    "),TeX(" $sigma$"))) +
  scale_colour_viridis_d(labels = c(TeX(" $mu$    "),TeX(" $sigma$")),option = "viridis",end = 0.8)

library(ggthemes)
####################################							
library(viridis)							

par(mfrow=c(1,31))							

aux <- data.frame(x= c(0,4,4,8),y=c(0,0.5,0.0,0.5))							
g1 <- aux %>%							
  ggplot(aes(x=x,y=y)) +							
  geom_line(linewidth=1.5) +							
  theme_classic() +							
  labs(x = "Time",y="Degradation",title = "Perfect Repair") +							
  scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0),limits = c(0, 1))							


aux2 <- data.frame(x= c(0,8),y=c(0,1))							
g2 <- aux2 %>%							
  ggplot(aes(x=x,y=y)) +							
  geom_line(linewidth=1.5) +							
  theme_classic() +							
  labs(x = "Time",y="Degradation",title = "Minimal Repair") +							
  scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0),limits = c(0, 1)) +							
  geom_line(aes(x=c(4,4),y=c(0,0.5)),linetype = 3,linewidth=1.5)							


aux3 <- data.frame(x= c(0,4,4,8),y=c(0,0.5,0.2,0.7))							
line_data <- data.frame(x=c(4,4), y=c(0,0.2))							

g3 <- aux3 %>%							
  ggplot(aes(x=x, y=y)) +							
  geom_line(linewidth=1.5) +							
  geom_line(data = line_data, aes(x=x, y=y),linetype = 3,linewidth=1.5) +							
  theme_classic() +							
  labs(x = "Time", y = "Degradation", title = "Imperfect Repair") +							
  scale_x_continuous(expand = c(0, 0)) +							
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1))							

grid.arrange(g1,g2,g3,ncol=3)							

###################							
t <- seq(0,10,by=2)							
set.seed(12)							
degrad1 <- c(0,cumsum(rexp(n=length(t)-1,1/3)))							
degrad2 <- c(0,cumsum(rexp(n=length(t)-1,1/6)))							
degrad3 <- c(0,cumsum(rexp(n=length(t)-1,1/9)))							

data.frame(time = t,Unit1 = degrad1,Unit2 = degrad2,Unit3 = degrad3) %>%							
  tidyr::gather(key="unit",value="deg",-time) %>%							
  ggplot(aes(x=time,y=deg,group = unit,colour = unit)) +							
  geom_point(size=3,colour="black") +							
  geom_line(linewidth=1.5,alpha=0.7) +							
  theme_classic() +							
  theme(legend.title = element_blank(),							
        plot.title = element_blank(),							
        legend.position = c(0.08,0.9)) +							
  labs(x = "Time", y = "Degradation") +							
  scale_x_continuous(expand = c(0, 0),breaks = c(0,2,4,6,8,10),limits = c(0,11)) +							
  scale_y_continuous(expand = c(0, 0), limits = c(0,65)) +							
  scale_color_viridis(discrete = TRUE, option = "D")							

#######################							
library(latex2exp)							
library(ggrepel)							

set.seed(1111)							
rho<- c(0.6, 0.6)							
n_manu <- 2							
intra_manu <- 4							
n_med <- (n_manu+1)*(intra_manu+2)-(n_manu+1)							
df_degradacao1 <- gera_dados3(n_s=1,t_max = 15,n_med=n_med,v=2,sigma=sqrt(2),rho=rho,n_manu=n_manu)							
df_degradacao1 %>%							
  ggplot(aes(x=Time,y=Y)) +							
  geom_point(size=3,colour="black") +							
  geom_line(linewidth=1.5,alpha=0.7) +							
  geom_text(x=(1+0.5), y= (df_degradacao1 %>% filter(Time==1) %>% select(Y) %>% min())-0.5,							
            label=TeX("$\\Delta Y_{0,1}$"),size = 5,colour="red")+							
  geom_text(x=(2+0.5), y= (df_degradacao1 %>% filter(Time==2) %>% select(Y) %>% min())-0.5,							
            label=TeX("$\\Delta Y_{0,2}$"),size = 5,colour="red")+							
  geom_text(x=(4-0.3), y= (df_degradacao1 %>% filter(Time==4) %>% select(Y) %>% min())+1,							
            label=TeX("$\\Delta Y_{0,n_0 + 1}$"),size = 5,colour="red")+							
  geom_text(x=(5-0.5), y= (df_degradacao1 %>% filter(Time==5) %>% select(Y) %>% min())-0.5,							
            label=TeX("$Y(\\tau_{1}^{+})$"),size=5,colour="red") +							
  geom_text(x=(5+0.6), y= df_degradacao1 %>% filter(Time==5) %>% summarise(y_mean = mean(Y)) %>% pull(),							
            label=TeX("$Z_1$"),size=5,colour="red") +							
  geom_text(x=(5-0.5), y= (df_degradacao1 %>% filter(Time==5) %>% select(Y) %>% max()) + 0.5,							
            label=TeX("$Y(\\tau_{1}^{-})$"),size=5,colour="red") +							
  geom_text(x=10, y= (df_degradacao1 %>% filter(Time==10) %>% select(Y) %>% min())-0.7,							
            label=TeX("$Y(\\tau_{2}^{+})$"),size=5,colour="red") +							
  geom_text(x=(10+0.6), y= df_degradacao1 %>% filter(Time==10) %>% summarise(y_mean = mean(Y)) %>% pull(),							
            label=TeX("$Z_2$"),size=5,colour="red") +							
  geom_text(x=10, y= (df_degradacao1 %>% filter(Time==10) %>% select(Y) %>% max())+0.7,							
            label=TeX("$Y(\\tau_{2}^{-})$"),size=5,colour="red") +							
  geom_text(x=(15-0.7), y= (df_degradacao1 %>% filter(Time==15) %>% select(Y)) %>% pull(),							
            label=TeX("$Y(\\tau_{3}^{-})$"),size=5,colour="red") +							
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
  labs(x = "Time", y = "Degradation")							


#######################################################################
############### Dados - Banco Prof. Maria Luíza #######################
#######################################################################

library(readxl)
library(dplyr)
library(lubridate)

df <- read_excel("Copy of Filtro Manga - Dados de processo.xlsx")
df <- df[-1,]

i_time <- ymd_hms("2024-05-04 10:50:00")
f_time <- ymd_hms("2024-05-04 20:00:00")

##
df_aux <- df %>%
  filter(Diferencial_mmCa_800dPT8102 == 0,
         `Data Hora` >= i_time & `Data Hora` < f_time)
plot(df_aux$`Data Hora`,df_aux$Diferencial_mmCa_800dPT8102)

f_time_0 <- ymd_hms("2024-05-04 13:00:00")
x_0 <- df %>%
  filter(Diferencial_mmCa_800dPT8102 == 0,
         `Data Hora` >= i_time & `Data Hora` < f_time_0) %>% 
  select(`Data Hora`) %>%
  pull() %>% max()

i_time_1 <- ymd_hms("2024-05-04 13:00:00")
x_1 <- df %>%
  filter(Diferencial_mmCa_800dPT8102 == 0,
         `Data Hora` >= i_time_1 & `Data Hora` < f_time) %>% 
  select(`Data Hora`) %>%
  pull() %>% min()


df_aux_1 <- df %>%
  filter(`Data Hora` >= x_0 & `Data Hora` < x_1)

index <- seq(1,nrow(df_aux_1),by=8)
df_thin_1 <- df_aux_1[index,]  

x_2 <- df_thin_1 %>%
  filter(`Data Hora` >= i_time_1 & `Data Hora` < f_time) %>% 
  select(`Data Hora`) %>%
  pull() %>% max()

df_aux_2 <- df %>%
  filter(`Data Hora` >= x_2 & `Data Hora` < f_time)

indaux <- c(3,142:873)
df_aux_2 <- df_aux_2[indaux,]

index2 <- seq(1,nrow(df_aux_2),by=8)
index2[seq(1,length(index2),by=13)]

df_aux_2[730,]

cx <- c(1,105,106,209,210,313,314,417,418,520,521,625,626,729,730)

index2 <- c(index2,cx)
index2 <- unique(index2)
index2 <- sort(index2)

df_aux_2 <- df_aux_2[index2,]
nrow(df_aux_2)/14


s1 <- seq(1,14)
for (i in 1:8) {
  s2 <- seq(max(s1),max(s1) + 13)
  s1 <- c(s1,s2)
}

length(s1[15:(15+98)])
length(s1)


df_thin_1 <- df_thin_1 %>%
  mutate(Time = seq(1,nrow(df_thin_1)))

df_aux_2 <- df_aux_2 %>%
  mutate(Time = s1[15:(15+98)])


df_aux_maria <- rbind(df_thin_1,df_aux_2)
sub_maria <- df_aux_maria[1:50,]

plot(sub_maria$Time,sub_maria$Diferencial_mmCa_800dPT8102,type = "l")
names(sub_maria)[5] <- "Y"
sub_maria <- sub_maria %>%
  mutate(Time = Time -1,
         Objeto = "OBJ_001")

gera_plot4(sub_maria)
mle_drift1(sub_maria)
mle_drift1_y(sub_maria)

mle_sigma1(sub_maria)
mle_sigma1_y(sub_maria)

gera_plot4(sub_maria)
mle_drift1_y(sub_maria)
mle_sigma1_y(sub_maria)

rho_hat(sub_maria)

# ##
# t_aux <- ymd_hms("2024-05-04 15:29:27")
# df %>% filter(`Data Hora` %in% c(t_aux,t_aux - hms("00:00:26")))


gera_plot4(sub_maria)
mle_drft_y(sub_maria)


#################
### Recorte 02 ##
#################

library(readxl)
library(dplyr)
library(lubridate)

df <- read_excel("Copy of Filtro Manga - Dados de processo.xlsx")
df <- df[-1,]

i_time <- ymd_hms("2024-05-04 14:40:34")
f_time <- ymd_hms("2024-05-10 20:00:00")

plot(df$`Data Hora`,df$Diferencial_mmCa_800dPT8102,type = "l")

df_aux <- df %>%
  filter(#Diferencial_mmCa_800dPT8102 == 0,
         `Data Hora` >= i_time & `Data Hora` < f_time)
plot(df_aux$`Data Hora`,df_aux$Diferencial_mmCa_800dPT8102,type = "l")

index <- seq(1,nrow(df_aux),by=24)
df_thin <- df_aux[index,]

plot(df_thin$`Data Hora`,df_thin$Diferencial_mmCa_800dPT8102,type = "l")



f_time_0 <- ymd_hms("2024-05-04 13:00:00")
x_0 <- df %>%
  filter(Diferencial_mmCa_800dPT8102 == 0,
         `Data Hora` >= i_time & `Data Hora` < f_time_0) %>% 
  select(`Data Hora`) %>%
  pull() %>% max()

i_time_1 <- ymd_hms("2024-05-04 13:00:00")
x_1 <- df %>%
  filter(Diferencial_mmCa_800dPT8102 == 0,
         `Data Hora` >= i_time_1 & `Data Hora` < f_time) %>% 
  select(`Data Hora`) %>%
  pull() %>% min()




