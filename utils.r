# ---------------------------------------------------
# Arquivo: utils.R
# Descrição: Conjunto de funções úteis para análise
# Autor: George Anderson A. dos Santos
# Data: 17-04-2024
# ---------------------------------------------------

library(dplyr)
library(ggplot2)
library(tayloRswift)
library(statmod)
library(scales)

#' Gera observações do processo Wiener para um objeto específico.
#'
#' Esta função gera observações de um processo Wiener para um objeto específico,
#' utilizando os parâmetros fornecidos.
#'
#' @param t_max Tempo máximo para observações.
#' @param n_med Número de medições.
#' @param v Parâmetro de tendência.
#' @param sigma Desvio padrão.
#' @param obj Identificador do objeto (padrão = 1).
#'
#' @return Um data frame contendo observações do processo Wiener para o objeto especificado.
#'
#' @export
gera_obs <- function(t_max, n_med, v, sigma, obj = 1) {
  time <- seq(0, t_max, length.out = n_med)
  # Bt <- 0
  # for (i in 2:n_med) {
  #   aux <- rnorm(1, mean = 0, sd = sqrt(time[i]))
  #   Bt <- c(Bt, aux)
  # }
  Bt <- c(0,sqrt(t_max/(n_med-1))*cumsum(rnorm(n_med-1)))
  wt <- v*time + sigma*Bt
  df <- data.frame( Objeto = rep(paste("OBJ_0", obj, sep = "")),
             Wt = wt,
             Time = time)
  
  return(df)
}

#' Gera dados de processo Wiener para vários objetos.
#'
#' Esta função gera dados de processo Wiener para vários objetos, utilizando os
#' parâmetros fornecidos.
#'
#' @param n_obs Número de observações.
#' @param t_max Tempo máximo para observações.
#' @param n_med Número de medições.
#' @param v Parâmetro de tendência.
#' @param sigma Desvio padrão.
#'
#' @return Um data frame contendo dados de processo Wiener para os objetos especificados.
#'
#' @export
gera_dados <- function(n_obs, t_max, n_med, v, sigma) {
  n_med <- n_med+1
  df <- gera_obs(t_max, n_med, v, sigma)
  if (n_obs>1){
    i <- 2
    for (i in 2:n_obs) {
      aux <- gera_obs(t_max, n_med, v, sigma, obj = i)
      df <- rbind(df, aux)
    }
  }
  return(df)
}

#' Gera um gráfico de linha e pontos para visualizar os caminhos de degradação dos objetos.
#'
#' Esta função gera um gráfico de linha e pontos para visualizar os caminhos de degradação dos objetos,
#' utilizando os dados fornecidos.
#'
#' @param dados O data frame contendo os dados de degradação.
#'
#' @return Um gráfico ggplot2 que mostra os caminhos de degradação dos objetos ao longo do tempo.
#'
#' @import ggplot2
#' @importFrom hrbrthemes theme_ipsum_tw
#' @importFrom scales scale_color_ipsum
#'
#' @export
gera_plot <- function(dados) {
  ggplot2::ggplot(
    dados,
    aes(x = Time, y = Wt, group = factor(Objeto), color = factor(Objeto))
  ) +
    geom_line(linewidth = 1.2, alpha = 0.7) +
    geom_point(size = 2.0) +
    labs(
      x = "Time",
      y = "Degradation (wt or xt)",
      title = "Degradation Paths",
      color = "System"
    ) +
    theme(
      legend.position = c(0.10, 0.85),
      legend.background = element_rect(fill = "white", color = "black")
    )
}

#' Estima o drift do processo Wiener usando o Método da Máxima Verossimilhança (MLE).
#'
#' Esta função estima o drift do processo Wiener usando o Método da Máxima Verossimilhança (MLE).
#'
#' @param data Um data frame contendo os dados do processo Wiener.
#'
#' @return O drift estimado do processo Wiener.
#'
#' @examples
#' # Exemplo de uso:
#' # data <- gera_dados(n_obs = 100, t_max = 10, n_med = 100, v = 0.1, sigma = 0.2)
#' # mle <- mle_drift(data)
#' # print(mle)
#'
#' @export
mle_drift <- function(data) {
  n_med <- c(table(data$Objeto))
  n_obs <- c(length(n_med))
  last_index <- c(seq(1:n_obs) * n_med)
  last_d <- data$Wt[last_index]
  last_t <- data$Time[last_index]
  
  first_index <- c(0,last_index[-n_obs]) +2
  first_d <- data$Wt[first_index]
  first_t <- data$Time[first_index]
  
  # mle <- mean(last_d) / mean(last_t)
  mle <- (mean(last_d)-mean(first_d))/(mean(last_t)-mean(first_t))
  
  return(mle)
}

#' Estima o desvio padrão do processo Wiener usando o Método da Máxima Verossimilhança (MLE).
#'
#' Esta função estima o desvio padrão do processo Wiener usando o Método da Máxima Verossimilhança (MLE).
#'
#' @param data Um data frame contendo os dados do processo Wiener.
#'
#' @return O desvio padrão estimado do processo Wiener.
#'
#' @examples
#' # Exemplo de uso:
#' # data <- gera_dados(n_obs = 100, t_max = 10, n_med = 100, v = 0.1, sigma = 0.2)
#' # mle <- mle_sigma(data)
#' # print(mle)
#'
#' @export
mle_sigma <- function(data) {
  # Calcula os incrementos
  DeltaW <- diff(data$Wt)
  DeltaT <- diff(data$Time)
  
  # Estimativa de mu
  mu_hat <- sum(DeltaW) / sum(DeltaT)
  
  # Estimativa de sigma^2
  sigma2_hat <- sum((DeltaW - mu_hat * DeltaT)^2) / sum(DeltaT)
  
  # Estimativa de sigma
  sigma_hat <- sqrt(sigma2_hat)
  
  return(sigma_hat)
}

#' Gera dados simulados de processo Wiener com intervenção de manutenção.
#'
#' Esta função gera dados simulados de processo Wiener com intervenção de manutenção,
#' onde a degradação é modelada com um parâmetro de efeito de manutenção entre medições antes e após
#' a intervenção.
#'
#' @param t_max Tempo máximo para observações.
#' @param n_med Número de medições para geração do processo Wiener.
#' @param v Parâmetro de tendência.
#' @param sigma Desvio padrão.
#' @param rho Parâmetro de efeito de manutenção.
#' @param n_manu Número de manutenções.
#'
#' @return Um data frame contendo os dados simulados de processo Wiener com intervenção de manutenção.
#'
#' @examples
#' # Exemplo de uso:
#' # dados <- gera_dados1(t_max = 10, n_med = 100, v = 0.1, sigma = 0.2, rho = 0.5, n_manu = 2)
#' # print(dados)
#'
#' @export
gera_dados1 <- function(t_max, n_med, v, sigma, rho, n_manu) {
  df <- gera_obs(t_max, n_med + 1, v, sigma)
  time_MP <- seq(0, t_max, t_max / (n_manu + 1))
  time_MP <- time_MP[-c(1, length(time_MP))]
  Y <- n_med + 1 + n_manu
  jy <- Y / (n_manu + 1)
  jw <- n_med / (n_manu + 1)
  Y <- numeric(Y)
  W <- df$Wt
  for (k in 1:n_manu) {
    Y[1:jy] <- W[1:(jw + 1)]
    Y[(k * jy + 1):((k + 1) * jy)] <- W[(jw * k + 1):((k + 1) * jw + 1)] - rho * W[(jw * k + 1)]
  }
  df <- rbind(df, df %>%
                filter(Time %in% time_MP)) %>%
    arrange(Time)
  df$Y <- Y
  
  return(df)
}

#' Estima o parâmetro de efeito de manutenção.
#'
#' Esta função estima o parâmetro de efeito de manutenção utilizando dados de degradação.
#'
#' @param data Um data frame contendo os dados de degradação.
#'
#' @return O parâmetro estimado de efeito de manutenção.
#'
#' @examples
#' # Exemplo de uso:
#' # dados <- gera_dados1(t_max = 10, n_med = 100, v = 0.1, sigma = 0.2, rho = 0.5, n_manu = 2)
#' # rho_estimado <- rho_hat(dados)
#' # print(rho_estimado)
#'
#' @export
rho_hat <- function(data) {
  k <- data$Time[duplicated(data$Time)]
  pass <- max(data$Time)/(length(unique(data$Time))-1)
  Zj <- NA
  yji <- NA
  aux <- -diff(data$Y)
  
  for (i in 2:length(k)) {
    Zj[1] <- -diff(data$Y[data$Time == k[1]])
    yji[1] <- sum(aux[1:(k[1]/pass)])
    Zj[i] <- -diff(data$Y[data$Time == k[i]])
    yji[i] <- sum(aux[(k[i - 1]/pass + i):(k[i]/pass + (i - 1))]) 
  }
  return((-Zj / yji))
}

#' Gera um gráfico para visualizar os caminhos de degradação com e sem intervenção de manutenção.
#'
#' Esta função gera um gráfico para visualizar os caminhos de degradação com e sem intervenção de manutenção,
#' utilizando os dados fornecidos.
#'
#' @param data Um data frame contendo os dados de degradação com e sem intervenção de manutenção.
#'
#' @return Um gráfico ggplot2 que mostra os caminhos de degradação com e sem intervenção de manutenção ao longo do tempo.
#'
#' @examples
#' # Exemplo de uso:
#' # dados <- gera_dados1(t_max = 10, n_med = 100, v = 0.1, sigma = 0.2, rho = 0.5, n_manu = 2)
#' # plot <- gera_plot1(dados)
#' # print(plot)
#'
#' @export
gera_plot1 <- function(data) {
  p <- data %>%
    ggplot() +
    geom_line(aes(x = Time, y = Y, colour = "With Maintenance"), alpha = 0.5, linetype = "solid", linewidth = 1) +
    geom_line(aes(x = Time, y = Wt, colour = "Standard"), alpha = 0.5, linetype = "solid", linewidth = 1) + 
    labs(
      x = "Time",
      y = "Degradation",
      title = "Degradation Paths (100%,50%,100%)",
      color = "Process"
    ) +
    theme(
      legend.position = c(0.20, 0.85),
      legend.background = element_rect(fill = "white", color = "black")
    )
  
  k <- data$Time[duplicated(data$Time)]
  for (i in 1:length(k)) {
    ponto <- k[i]
    max_y <- max(data$Y[data$Time == ponto])
    min_y <- min(data$Y[data$Time == ponto])
    p <- p +
      geom_segment(data = data.frame(x = ponto, xend = ponto, y = max_y, yend = min_y),
                   aes(x = x, xend = xend, y = y, yend = yend), linetype = "dotted",
                   color = "black", linewidth = 1)
  }
  return(p)
}

#' Gera dados simulados de processo Wiener com diferentes efeitos de manutenção.
#'
#' Esta função gera dados simulados de processo Wiener com diferentes efeitos de manutenção,
#' onde a degradação é modelada com efeitos para cada ação de manutencão.
#'
#' @param t_max Tempo máximo para observações.
#' @param n_med Número de medidas para geração do processo Wiener.
#' @param v Parâmetro de tendência.
#' @param sigma Desvio padrão.
#' @param rho Vetor contendo os parâmetros de efeito de manutenção para cada intervenção.
#' @param n_manu Número de intervenções de manutenção.
#'
#' @return Um data frame contendo os dados simulados de processo Wiener com diferentes efeitos de intervenção de manutenção.
#'
#' @examples
#' # Exemplo de uso:
#' # dados <- gera_dados2(t_max = 10, n_med = 100, v = 0.1, sigma = 0.2, rho = c(0.2, 0.4), n_manu = 2)
#' # print(dados)
#'
#' @export
gera_dados2 <- function(t_max, n_med, v, sigma, rho, n_manu,obj = 1) {
  df <- gera_obs(t_max, n_med + 1, v, sigma, obj)
  time_MP <- seq(0, t_max, t_max / (n_manu + 1))
  time_MP <- time_MP[-c(1, length(time_MP))]
  Y <- n_med + 1 + n_manu
  jy <- Y / (n_manu + 1)
  jw <- n_med / (n_manu + 1)
  Y <- numeric(Y)
  W <- df$Wt
  
  Y[1:jy] <- W[1:(jw + 1)]
  for (k in 1:n_manu) {
    if (k == 1){
      Y[(k * jy + 1)] <- (1 - rho[k]) * W[(k * jw + 1)]
      Y[(k * jy + 2):((k + 1) * jy)] <- (1 - rho[k]) * W[(k * jw + 1)] + W[(jw * k + 2):((k + 1) * jw + 1)] - W[(k * jw + 1)]
      # Y[(k * jy + 2):((k + 1) * jy)] <- Y[(k * jy + 1)] + W[(jw * k + 2):((k + 1) * jw + 1)] - W[(k * jw + 1)]
    }
    if (k > 1){
      # Y[(k * jy + 1)] <- (1 - rho[k]) * Y[(k * jy)] + rho[k] * Y[((k - 1) * jy + 1)]
      aux_w <- NA
      for (l in 1:k) {
        aux_w[l] <- rho[l]*(W[(l * jw + 1)]-W[((l-1) * jw + 1)]) # Somatório dos Rho's
      }
      Y[(k * jy + 1)] <- W[(k * jw + 1)] - sum(aux_w)
      Y[(k * jy + 2):((k + 1) * jy)] <- W[(k * jw + 1)] - sum(aux_w) + W[(jw * k + 2):((k + 1) * jw + 1)] - W[(k * jw + 1)]
    }
  }
  df <- rbind(df, df %>%
                filter(round(Time,6) %in% round(time_MP,6))) %>%
    arrange(Time)
  df$Y <- Y
  
  return(df)
}

#' Gera múltiplos conjuntos de dados simulados de processo Wiener com diferentes efeitos de manutenção.
#'
#' Esta função gera múltiplos conjuntos de dados simulados de processo Wiener com diferentes efeitos de manutenção,
#' utilizando a função `gera_dados2` como base.
#'
#' @param n_s Número de sistemas (conjuntos de dados) a serem gerados.
#' @param t_max Tempo máximo para observações.
#' @param n_med Número de medidas para geração do processo Wiener.
#' @param v Parâmetro de tendência.
#' @param sigma Desvio padrão.
#' @param rho Vetor contendo os parâmetros de efeito de manutenção para cada intervenção.
#' @param n_manu Número de intervenções de manutenção.
#'
#' @return Um data frame contendo os dados simulados de processo Wiener para todos os sistemas.
#'
#' @examples
#' # Exemplo de uso:
#' # dados <- gera_dados3(n_s = 5, t_max = 10, n_med = 100, v = 0.1, sigma = 0.2, rho = c(0.2, 0.4), n_manu = 2)
#' # print(dados)
#'
#' @export
gera_dados3 <- function(n_s, t_max, n_med, v, sigma, rho, n_manu){
  df <- gera_dados2(t_max,n_med,v,sigma,rho,n_manu)
  if (n_s>1){
    i <- 2
    for (i in 2:n_s) {
      aux <- gera_dados2(t_max,n_med,v,sigma,rho,n_manu,obj = i)
      df <- rbind(df, aux)
    }
  }
  return(df)
}

#' Gera um painel de gráficos para visualizar os caminhos de degradação por sistema.
#'
#' Esta função gera um painel de gráficos para visualizar os caminhos de degradação por sistema,
#' utilizando a função `gera_plot1` para cada sistema individualmente.
#'
#' @param data Um data frame contendo os dados de degradação por sistema.
#'
#' @return Um painel de gráficos que mostra os caminhos de degradação por sistema.
#'
#' @examples
#' # Exemplo de uso:
#' # dados <- gera_dados3(n_s = 5, t_max = 10, n_med = 100, v = 0.1, sigma = 0.2, rho = c(0.2, 0.4), n_manu = 2)
#' # gera_plot2(dados)
#'
#' @export
gera_plot2 <- function(data){
  s_obj <- unique(data$Objeto)
  p<- list()
  for (i in 1:length(s_obj)) {
    p[[i]] <- gera_plot1(data %>% filter(Objeto == s_obj[i]))
  }
  n <- length(p)
  nCol <- floor(sqrt(n))
  do.call("grid.arrange", c(p, ncol=nCol))
}

#' Estima o parâmetro de drift para múltiplos sistemas.
#'
#' Esta função estima o parâmetro de drift para múltiplos sistemas.
#'
#' @param data Um data frame contendo os dados de degradação por sistema.
#'
#' @return O parâmetro estimado de drift para múltiplos sistemas.
#'
#' @examples
#' # Exemplo de uso:
#' # dados <- gera_dados3(n_s = 5, t_max = 10, n_med = 100, v = 0.1, sigma = 0.2, rho = c(0.2, 0.4), n_manu = 2)
#' # mle <- mle_drift1(dados)
#' # print(mle)
#'
#' @export
mle_drift1 <- function(data){
  s <- unique(data$Objeto)
  tau <- max(data$Time)
  xl_tau <- NA
  for (l in 1:length(s)) {
    xl_tau[l] <- data %>% filter(Objeto == s[l],Time==tau) %>%
      select(Wt) %>% pull()
  }
  mu_hat <- sum(xl_tau)/(tau*length(s))
  return(mu_hat)
}

#' Estima o desvio padrão para múltiplos sistemas.
#'
#' Esta função estima o desvio padrão para múltiplos sistemas, utilizando a média dos quadrados dos resíduos.
#'
#' @param data Um data frame contendo os dados de degradação por sistema.
#'
#' @return O desvio padrão estimado para múltiplos sistemas.
#'
#' @examples
#' # Exemplo de uso:
#' # dados <- gera_dados3(n_s = 5, t_max = 10, n_med = 100, v = 0.1, sigma = 0.2, rho = c(0.2, 0.4), n_manu = 2)
#' # sigma <- mle_sigma1(dados)
#' # print(sigma)
#'
#' @export
mle_sigma1 <- function(data){
  mu_hat <- mle_drift1(data)
  s <- unique(data$Objeto)
  k <- data %>% filter(Objeto == s[1],duplicated(Time)) %>% select(Time) %>% pull()
  nj <- data %>% filter(Objeto == s[1],Time > k[1],Time<k[2]) %>% nrow()
  N<- nj*(length(k)+1)
  pass <- max(data$Time)/(length(unique(data$Time))-1)
  y_aux <- matrix(NA,nrow=length(s),ncol=(length(k)+1))
  yji<-NA
  tji<-NA
  for (l in 1:length(s)) {
    aux_data <- data %>% filter(Objeto == s[l])
    for (j in 1:(length(k)+1)) {
      if (j == 1){
        yji <- aux_data %>% filter(Time <= k[j]) %>% 
          filter(row_number() <= n()-1) %>% 
          select(Wt) %>% pull() %>% diff()
        tji <- aux_data %>% filter(Time <= k[j]) %>% 
          filter(row_number() <= n()-1) %>% 
          select(Time) %>% pull() %>% diff()
        # tji <- diff(aux_data$Time)[1:(k[j])]
        # tji <- diff(aux_data$Time)[1:(k[j])]
        y_aux[l,j] <- sum(((yji-mu_hat*tji)^2)/tji)
      }
      if (j>=2 & j<(length(k)+1) ){
        yji <- aux_data %>% filter(Time >= k[j-1],Time <= k[j]) %>% slice(3:n()-1) %>% 
          select(Wt) %>% pull() %>% diff()
        tji <- aux_data %>% filter(Time >= k[j-1],Time <= k[j]) %>% slice(3:n()-1) %>% 
          select(Time) %>% pull() %>% diff()
        # tji <- diff(aux_data$Time)[(k[j - 1]/pass + j):(k[j]/pass + (j - 1))]
        # yji <- diff(aux_data$Y)[(k[j - 1]/pass + j):(k[j]/pass + (j - 1))]
        y_aux[l,j] <- sum(((yji-mu_hat*tji)^2)/tji)
      }
      if (j==(length(k)+1)){
        yji <- aux_data %>% filter(Time >= k[j-1]) %>% slice(2:n()) %>% 
          select(Wt) %>% pull() %>% diff()
        tji <- aux_data %>% filter(Time >= k[j-1]) %>% slice(2:n()) %>% 
          select(Time) %>% pull() %>% diff()
        # tji <- diff(aux_data$Time)[(k[j - 1]/pass + j):(nrow(aux_data)-1)]
        # yji <- diff(aux_data$Y)[(k[j - 1]/pass + j):(nrow(aux_data)-1)]
        y_aux[l,j] <- sum(((yji-mu_hat*tji)^2)/tji)
      }
    }
  }
  sigma2_hat_biased <- sum(y_aux)/(length(s)*(N+length(k)+1))
  return(sqrt(sigma2_hat_biased))
}


#' Plota caminho de degradação considerando os efeitos de ação de manutenção ação (desatualizado).
#' 
#' @param data Base de dados utilizada
#' @param xlab Rotulo do eixo X
#' @param ylab Rotulo do eixo Y
#'
#' @return Visualização do caminho de degradação
#'
#' @examples
#' # Exemplo de uso:
#' # gera_plot3(df,"Time","Degradation")
#'
#' @export
gera_plot3 <- function(data,xlab,ylab){
  p <- data %>%
    ggplot() +
    geom_line(aes(x = Time, y = Y, colour = "With Maintenance"),
              alpha = 0.5, linetype = "solid", linewidth = 1) +
    labs(
      x = xlab,
      y = ylab,
      title = "(I)"
    ) +
    theme_classic() +
    tayloRswift::scale_color_taylor(palette="taylor1989",reverse = FALSE) +
    theme(plot.title = element_blank(),
          legend.position = "none",
          # plot.title = element_text(face = "bold",hjust = 0.5)
          ) +							
    scale_y_continuous(expand = c(0, 0)) +							
    scale_x_continuous(expand = c(0, 0))
  
  k <- data$Time[duplicated(data$Time)]
  for (i in 1:length(k)) {
    ponto <- k[i]
    max_y <- max(data$Y[data$Time == ponto])
    min_y <- min(data$Y[data$Time == ponto])
    p <- p +
      geom_segment(data = data.frame(x = ponto, xend = ponto, y = max_y, yend = min_y),
                   aes(x = x, xend = xend, y = y, yend = yend), linetype = "solid",
                   color = "black", linewidth = 1)
  }
  return(p)
}


#' Calcula a estimativa do parâmetro de drift considerando Y_t (processo com acao de manutencao) 
#' em vez de X_t (processo wiener original)
#' 
#' @param data Base de dados utilizada
#'
#' @return estimativa
#'
#' @examples
#' # Exemplo de uso:
#' # mle_drift1_y(df)
#'
#' @export
mle_drift1_y <- function(data){
  k <- data$Time[duplicated(data$Time)]
  pass <- max(data$Time)/(length(unique(data$Time))-1)
  tau <- max(data$Time)
  Zj <- NA
  yji <- NA
  
  y_rho <- data %>% filter(Time == max(Time)) %>% select(Y) %>%
    pull()
  
  for (i in 1:length(k)) {
    Zj[i] <- diff(data %>% filter(Time == k[i]) %>% select(Y) %>% pull())
    
  }
  
  # sum(diff(data[1:6,4]))+         # calculo do delta_y (não somar z_j)
  # sum(diff(data[7:12,4]))+
  # sum(diff(data[13:18,4]))+
  # sum(diff(data[19:24,4]))
  result <-  (y_rho-sum(Zj))/tau
  
  return(result)
}

#' Calcula a estimativa do parâmetro de difusão considerando Y_t (processo com acao de manutencao) 
#' em vez de X_t (processo wiener original)
#' 
#' @param data Base de dados utilizada
#'
#' @return estimativa
#'
#' @examples
#' # Exemplo de uso:
#' # mle_drift1_y(df)
#'
#' @export
mle_sigma1_y <- function(data){
  mu_hat <- mle_drift1_y(data)
  s <- unique(data$Objeto)
  k <- data %>% filter(Objeto == s[1],duplicated(Time)) %>% select(Time) %>% pull()
  nj <- data %>% filter(Objeto == s[1],Time > k[1],Time<k[2]) %>% nrow()
  N<- nj*(length(k)+1)
  pass <- max(data$Time)/(length(unique(data$Time))-1)
  y_aux <- matrix(NA,nrow=length(s),ncol=(length(k)+1))
  yji<-NA
  tji<-NA
  for (l in 1:length(s)) {
    aux_data <- data %>% filter(Objeto == s[l])
    for (j in 1:(length(k)+1)) {
      if (j == 1){
        yji <- aux_data %>% filter(Time <= k[j]) %>% 
          filter(row_number() <= n()-1) %>% 
          select(Y) %>% pull() %>% diff()
        tji <- aux_data %>% filter(Time <= k[j]) %>% 
          filter(row_number() <= n()-1) %>% 
          select(Time) %>% pull() %>% diff()
        # tji <- diff(aux_data$Time)[1:(k[j])]
        # tji <- diff(aux_data$Time)[1:(k[j])]
        y_aux[l,j] <- sum(((yji-mu_hat*tji)^2)/tji)
      }
      if (j>=2 & j<(length(k)+1) ){
        yji <- aux_data %>% filter(Time >= k[j-1],Time <= k[j]) %>% slice(3:n()-1) %>% 
          select(Y) %>% pull() %>% diff()
        tji <- aux_data %>% filter(Time >= k[j-1],Time <= k[j]) %>% slice(3:n()-1) %>% 
          select(Time) %>% pull() %>% diff()
        # tji <- diff(aux_data$Time)[(k[j - 1]/pass + j):(k[j]/pass + (j - 1))]
        # yji <- diff(aux_data$Y)[(k[j - 1]/pass + j):(k[j]/pass + (j - 1))]
        y_aux[l,j] <- sum(((yji-mu_hat*tji)^2)/tji)
      }
      if (j==(length(k)+1)){
        yji <- aux_data %>% filter(Time >= k[j-1]) %>% slice(2:n()) %>% 
          select(Y) %>% pull() %>% diff()
        tji <- aux_data %>% filter(Time >= k[j-1]) %>% slice(2:n()) %>% 
          select(Time) %>% pull() %>% diff()
        # tji <- diff(aux_data$Time)[(k[j - 1]/pass + j):(nrow(aux_data)-1)]
        # yji <- diff(aux_data$Y)[(k[j - 1]/pass + j):(nrow(aux_data)-1)]
        y_aux[l,j] <- sum(((yji-mu_hat*tji)^2)/tji)
      }
    }
  }
  sigma2_hat_biased <- sum(y_aux)/(length(s)*(N+length(k)+1))
  sigma2_hat_unbiased <- sigma2_hat_biased*(N+length(k)+1)/(N+length(k))
  return(sqrt(sigma2_hat_biased))
}

#' Gera curvas de confiabilidade para diferentes limiares considerando o tempo inicial da 
#' ultima ação de manutenção e a degradação acumulada.
#' 
#' @param mu estimativa do parametro de drift (tendência) considerando os dados utilizados.
#' @param sigma estimativa do parâmetro de difusão (variância) considerando os dados utilizados.
#' @param alpha vetor com limiares de degradação que são considerandos criticos.
#' @param t0 Tempo em que a última ação de degradação foi realizada.
#' @param x0 Degradação restante considerando a última ação de manutenção.
#' @param t_max Tempo máximo para ser plotado no gráfico.
#' @param xlab Rótulo do eixo X.
#' @param ylab Rótulo do eixo Y.
#' @param paleta paleta de cor para geração dos gráficos.
#'
#' @return Gráfico exibindo as curvas de confiabilidade para limiares de degração considerandos.
#'
#' @examples
#' # Exemplo de uso:
#' # mle_drift1_y(df)
#'
#' @export
plot_reliability <- function(mu,sigma,alpha,t0,x0,t_max,xlab,ylab,paleta){
  library(ggtext)
  media <- (alpha[1]-x0)/mu
  desvio <- ((alpha[1]-x0)/sigma)^2
  
  # Tempo Absoluto
  t = seq(t0,t_max,by=0.1) # Tempo absoluto
  
  # Tempo Relativo
  tau <- t - t0
  
  r_mean <- statmod::pinvgauss(tau,mean=media,shape = desvio,lower.tail = FALSE)
  
  df_visu <- data.frame(time = t,r_mean=r_mean,Threshold = alpha[1])
  if (length(alpha) > 1) {
    for (i in 2:length(alpha)) {
      media <- (alpha[i]-x0)/mu
      desvio <- ((alpha[i]-x0)/sigma)^2
      
      r_mean <- statmod::pinvgauss(tau,mean=media,shape = desvio,lower.tail = FALSE)
      
      df_visu_aux <- data.frame(time = t,r_mean=r_mean,Threshold = alpha[i])
      df_visu <- rbind(df_visu,df_visu_aux)
    }
  }
  
  p <- df_visu %>%
    mutate(Threshold = as.factor(Threshold)) %>%
    ggplot(aes(x=time,y=r_mean,colour = Threshold)) +
    scale_y_continuous(labels = scales::percent,limits=c(0,1)) +
    geom_line(linewidth=1.5,alpha=0.7) +
    geom_vline(xintercept = t0-2) +
    geom_vline(xintercept = t0,
               colour="black", linetype = "longdash") +
    theme_classic() +
    theme(plot.title = element_blank(),
          legend.position = c(0.9,0.8),
          axis.text.y = ggtext::element_markdown()
          # plot.title = element_text(hjust = 0.5,face = "bold")
          ) +
    labs(title = "(II)",
         x = xlab,
         y = ylab) +
    coord_cartesian(expand = FALSE) +
 #   scale_color_viridis(discrete = TRUE, option = "D")
    tayloRswift::scale_color_taylor(palette = paleta,reverse = FALSE) +
    annotate("text",x=(t0 + 2.5), y= 0.08,							
             label=paste("t=",t0),size = 3,colour="black")
  
  if (length(alpha) == 1){
    times_aux = c(60,90,120)
    conf_extract <- rep(NA,length(times_aux))
    
    for (k in 1:length(times_aux)) {
      conf_extract[k] <- df_visu %>% 
        filter(time == times_aux[k]) %>% 
        select(r_mean) %>% pull() %>% round(2)
    }
    names(conf_extract) <- times_aux
    y_breaks <- c(0,0.25,0.50,0.75,1,conf_extract)
    
    y_labels <- sapply(y_breaks, function(y) {
      percent_label <- scales::percent(y)
      if (y %in% conf_extract) {
        paste0("<span style='color:red;'>", percent_label, "</span>")
      } else {
        percent_label
      }
    })
    
    p <- p + geom_segment(aes(x=60,xend=60,y=0,yend=conf_extract["60"]),
                          linetype = "dotted",linewidth=0.1, alpha = 0.2,lineend = "round") +
      geom_segment(aes(x=90,xend=90,y=0,yend=conf_extract["90"]),
                   linetype = "dotted",linewidth=0.1,alpha=0.2) +
      geom_segment(aes(x=120,xend=120,y=0,yend=conf_extract["120"]),
                   linetype = "dotted",linewidth=0.1,alpha=0.2) +
      geom_segment(aes(x=t0-2,xend=60,y=conf_extract["60"],yend=conf_extract["60"]),
                   linetype = "dotted",linewidth=0.1,alpha=0.2) +
      geom_segment(aes(x=t0-2,xend=90,y=conf_extract["90"],yend=conf_extract["90"]),
                   linetype = "dotted",linewidth=0.1,alpha=0.2) +
      geom_segment(aes(x=t0-2,xend=120,y=conf_extract["120"],yend=conf_extract["120"]),
                   linetype = "dotted",linewidth=0.1,alpha=0.2) +
      scale_y_continuous(breaks = y_breaks, labels = y_labels, limits = c(0, 1))
  }
  
  return(p)
}

#' Plota caminho de degradação considerando os efeitos de ação de manutenção ação (Atualizado).
#' 
#' @param data Base de dados utilizada
#' @param xlab Rotulo do eixo X
#' @param ylab Rotulo do eixo Y
#'
#' @return Visualização do caminho de degradação
#'
#' @examples
#' # Exemplo de uso:
#' # plot_maintanance(df,"Time","Degradation")
#'
#' @export
plot_maintanance <- function(data, xlab, ylab,time=FALSE) {
  
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
    )
  
  # Adiciona os segmentos verticais nos pontos duplicados
  for (ponto in duplicated_times) {
    y_vals <- data$Y[data$Time == ponto]
    p <- p +
      geom_segment(
        data = data.frame(x = ponto, xend = ponto, y = max(y_vals), yend = min(y_vals)),
        aes(x = x, xend = xend, y = y, yend = yend, color = "Maintenance Effect"),
        linetype = "dotted", linewidth = 1
      )
  }
  
  p <- p +
    scale_color_manual(
      name = NULL,
      values = c(
        "Degradation Path" = tayloRswift::swift_palettes$taylor1989[1],
        "Maintenance Effect" = "black"
      )
    ) +
    theme_classic() +
    theme(
      legend.position = "top",
      plot.title = element_blank()
    ) +
    labs(x = xlab, y = ylab, title = "(I)") +
    scale_y_continuous(expand = c(0, 0)) +
    scale_x_continuous(expand = c(0, 0))
  
  if (time == TRUE){
    for (j in 1:length(duplicated_times)){
      p <- p + annotate("text",x=duplicated_times[j], y= (data %>% filter(Time==duplicated_times[j]) %>% select(Y) %>% min())-1,							
                        label=paste("t=",duplicated_times[j]),size = 3,colour="black")
    }
  }
  return(p)
}


############
## Design ##
############

Design <- SimDesign::createDesign(n_system = c(10,50),
                                  mu = 4,
                                  sigma = sqrt(1),
                                  n_main = 3,
                                  n_intra = 4)

Generate <- function(condition,fixed_objects){
  n_med <- (condition$n_main+1)*(condition$n_intra+2)-(condition$n_main+1)
  rho = c(0.1,0.3,0.5)
  
  dat <- gera_dados3(n_s=condition$n_system,
                     t_max = 20,n_med=n_med,
                     v=condition$mu,sigma=condition$sigma,rho=rho,n_manu=condition$n_main)
  dat
}

Analyse <- function(condition, dat, fixed_objects) {
  n_med <- (condition$n_main+1)*(condition$n_intra+2)-(condition$n_main+1)
  
  mu_hat <- mle_drift1(dat)
  sigma_hat <- mle_sigma1(dat)
  erro_padrao <- sigma_hat / sqrt(condition$n_system * 20)
  
  t_crit <- qt(1 - 0.05/2, df = condition$n_system*(n_med+condition$n_main +1) - 1)
  IC_mu_hat <- c(mu_hat - t_crit * erro_padrao, mu_hat + t_crit * erro_padrao)
  CP_mu_hat <- ECR(IC_mu_hat, condition$mu)
  
  ret <- c(mu_hat = mu_hat, sigma_hat = sigma_hat,
          CP_mu_hat = CP_mu_hat)
  ret
}

Summarise <- function(condition, results, fixed_objects) {
  obs_bias <- bias(results[, c("mu_hat", "sigma_hat")],
                   parameter = c(condition$mu, condition$sigma))
  obs_RMSE <- RMSE(results[, c("mu_hat", "sigma_hat")],
                   parameter = c(condition$mu, condition$sigma))
  obs_MAE <- SimDesign::MAE(results[, c("mu_hat", "sigma_hat")],
                            parameter = c(condition$mu, condition$sigma))
  obs_CP_mu_hat <- mean(results$CP_mu_hat)
  
  ret <- c(bias = obs_bias, RMSE = obs_RMSE, MAE = obs_MAE, 
           CP_mu_hat = obs_CP_mu_hat)
  ret
}

# resultados <- runSimulation(design=Design, replications=1000,
#                              generate=Generate, analyse=Analyse,summarise = Summarise)
