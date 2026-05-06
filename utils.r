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
gera_obs <- function(t_max, n_med, v, sigma2, obj = 1) {
  n_med <- n_med + 1
  time <- seq(0, t_max, length.out = n_med)
  # Bt <- 0
  # for (i in 2:n_med) {
  #   aux <- rnorm(1, mean = 0, sd = sqrt(time[i]))
  #   Bt <- c(Bt, aux)
  # }
  Bt <- c(0,sqrt(t_max/(n_med-1))*cumsum(rnorm(n_med-1)))
  wt <- v*time + sqrt(sigma2)*Bt
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
gera_dados <- function(n_obs, t_max, n_med, v, sigma2) {
  df <- gera_obs(t_max, n_med, v, sigma2)
  if (n_obs>1){
    i <- 2
    for (i in 2:n_obs) {
      aux <- gera_obs(t_max, n_med, v, sigma2, obj = i)
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
  sigma_hat <- (sigma2_hat)
  
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
gera_dados1 <- function(t_max, n_med, v, sigma2, rho, n_manu) {
  df <- gera_obs(t_max, n_med, v, sigma2)
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
gera_dados2 <- function(t_max, n_med, v, sigma2, rho, n_manu,obj = 1) {
  df <- gera_obs(t_max, n_med, v, sigma2, obj)
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
gera_dados3 <- function(n_s, t_max, n_med, v, sigma2, rho, n_manu){
  df <- gera_dados2(t_max,n_med,v,sigma2,rho,n_manu)
  if (n_s>1){
    i <- 2
    for (i in 2:n_s) {
      aux <- gera_dados2(t_max,n_med,v,sigma2,rho,n_manu,obj = i)
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
  sigma2_hat_unbiased <- sigma2_hat_biased*(length(s)*(N+length(k)+1))/(length(s)*(N+length(k)+1)-1)
  return(sigma2_hat_unbiased)
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
  s <- unique(data$Objeto)
  k <- data %>% filter(Objeto == s[1],duplicated(Time)) %>% select(Time) %>% pull()
  pass <- max(data$Time)/(length(unique(data$Time))-1)
  tau <- max(data$Time)
  Zj <- NA
  
  ytau_zlj <- lapply(1:length(s), function(current_id) {
    y_tau <- data %>% filter(s[current_id] == Objeto,Time == max(Time)) %>% select(Y) %>%
      pull()
    
    for (i in 1:length(k)) {
      Zj[i] <- diff(data %>% filter(s[current_id] == Objeto,Time == k[i]) %>% select(Y) %>% pull())
    }
    
    return(as_tibble(y_tau-sum(Zj)))
  })
  
  suppressMessages({
    (dplyr::bind_cols(ytau_zlj)%>% t() %>% colSums())/(length(s)*tau)
  })
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
  nj_last <- data %>% filter(Objeto == s[1],Time > k[length(k)],Time<max(Time)) %>% nrow()
  if (nj == nj_last){
    N <- nj*(length(k)+1)
  } else {
    N <- nj*(length(k)) + nj_last 
  }
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
  sigma2_hat_unbiased <- sigma2_hat_biased*(length(s)*(N+length(k)+1))/(length(s)*(N+length(k)+1)-1)
  return(sigma2_hat_unbiased)
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
#'
#' @return Gráfico exibindo as curvas de confiabilidade para limiares de degração considerandos.
#'
#' @examples
#' # Exemplo de uso:
#' # mle_drift1_y(df)
#'
#' @export
plot_reliability <- function(mu,sigma2,alpha,t0,x0,t_max,xlab,ylab,paleta = "taylor1989"){
  library(ggtext)
  media <- (alpha[1]-x0)/mu
  desvio <- ((alpha[1]-x0)^2)/sigma2
  
  # Tempo Absoluto
  t = seq(t0,t_max,by=0.1) # Tempo absoluto
  
  # Tempo Relativo
  tau <- t - t0
  
  r_mean <- statmod::pinvgauss(tau,mean=media,shape = desvio,lower.tail = FALSE)
  
  df_visu <- data.frame(time = t,r_mean=r_mean,Threshold = alpha[1])
  if (length(alpha) > 1) {
    for (i in 2:length(alpha)) {
      media <- (alpha[i]-x0)/mu
      desvio <- ((alpha[i]-x0))^2/sigma2
      
      r_mean <- statmod::pinvgauss(tau,mean=media,shape = desvio,lower.tail = FALSE)
      
      df_visu_aux <- data.frame(time = t,r_mean=r_mean,Threshold = alpha[i])
      df_visu <- rbind(df_visu,df_visu_aux)
    }
  }
  
  p <- df_visu %>%
    mutate(Threshold = as.factor(Threshold)) %>%
    ggplot(aes(x=time,y=r_mean,colour = Threshold)) +
    scale_y_continuous(labels = scales::percent,limits=c(0,1)) +
    geom_line(linewidth=1,alpha=0.7) +
    geom_vline(xintercept = t0-2) +
    geom_vline(xintercept = t0,
               colour="black", linetype = "longdash") +
    theme_classic() +
    theme(plot.title = element_blank(),
          legend.position = "none",
          axis.text.y = ggtext::element_markdown()
          ) +
    labs(title = "(II)",
         x = xlab,
         y = ylab) +
    coord_cartesian(expand = FALSE) +
    tayloRswift::scale_color_taylor(palette = paleta,reverse = FALSE) +
    annotate("text",x=(t0 + 2.5), y= 0.08,							
             label=paste("t=",t0),size = 3,colour="black")
  
  # if (length(alpha) == 10){
  #   times_aux = c(60,90,120)
  #   conf_extract <- rep(NA,length(times_aux))
  #   
  #   for (k in 1:length(times_aux)) {
  #     conf_extract[k] <- df_visu %>% 
  #       filter(time == times_aux[k]) %>% 
  #       select(r_mean) %>% pull() %>% round(2)
  #   }
  #   names(conf_extract) <- times_aux
  #   y_breaks <- c(0,0.25,0.50,0.75,1,conf_extract)
  #   
  #   y_labels <- sapply(y_breaks, function(y) {
  #     percent_label <- scales::percent(y)
  #     if (y %in% conf_extract) {
  #       paste0("<span style='color:red;'>", percent_label, "</span>")
  #     } else {
  #       percent_label
  #     }
  #   })
  #   
  #   p <- p + geom_segment(aes(x=60,xend=60,y=0,yend=conf_extract["60"]),
  #                         linetype = "8f",linewidth=0.1, alpha = 0.2,lineend = "round",
  #                         colour=tayloRswift::swift_palettes$taylor1989[4]) +
  #     geom_segment(aes(x=90,xend=90,y=0,yend=conf_extract["90"]),
  #                  linetype = "8f",linewidth=0.1,alpha=0.2,
  #                  colour=tayloRswift::swift_palettes$taylor1989[4]) +
  #     geom_segment(aes(x=120,xend=120,y=0,yend=conf_extract["120"]),
  #                  linetype = "8f",linewidth=0.1,alpha=0.2,
  #                  colour=tayloRswift::swift_palettes$taylor1989[4]) +
  #     geom_segment(aes(x=t0-2,xend=60,y=conf_extract["60"],yend=conf_extract["60"]),
  #                  linetype = "8f",linewidth=0.1,alpha=0.2,
  #                  colour=tayloRswift::swift_palettes$taylor1989[4]) +
  #     geom_segment(aes(x=t0-2,xend=90,y=conf_extract["90"],yend=conf_extract["90"]),
  #                  linetype = "8f",linewidth=0.1,alpha=0.2,
  #                  colour=tayloRswift::swift_palettes$taylor1989[4]) +
  #     geom_segment(aes(x=t0-2,xend=120,y=conf_extract["120"],yend=conf_extract["120"]),
  #                  linetype = "8f",linewidth=0.1,alpha=0.2,
  #                  colour=tayloRswift::swift_palettes$taylor1989[4]) +
  #     scale_y_continuous(breaks = y_breaks, labels = y_labels, limits = c(0, 1))
  # }
  
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

gera_plot_exp <- function(labs_01,labs_02,labs_03){
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
  
  # Gráfico de Densidade
  g1 <- ggplot(dados, aes(x = tempo, y = ft_t, color = lambda_f, linetype = lambda_f)) +
    geom_line(size = 1) +
    labs(title = labs_01[1], x = labs_01[2], y = labs_01[3], color = "", linetype = "") +
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
    labs(title = labs_02[1], x = labs_02[2], y = labs_02[3], color = "", linetype = "") +
    theme_classic() +
    theme(legend.position="none",
          plot.title = element_text(hjust = 0.5)) +
    scale_x_continuous(expand = c(0, 0)) +							
    scale_y_continuous(expand = c(0, 0),limits = c(0.4,1.6)) +
    tayloRswift::scale_color_taylor()
  
  # Gráfico de Confiabilidade
  g2 <- ggplot(dados, aes(x = tempo, y = R_t, color = lambda_f, linetype = lambda_f)) +
    geom_line(size = 1) +
    labs(title = labs_03[1], x = labs_03[2], y = labs_03[3], color = "", linetype = "") +
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

gera_plot_weibull <- function(labs_01,labs_02,labs_03){
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
  
  # Gráfico de Densidade
  g1 <- ggplot(dados, aes(x = t, y = ft_t, color = lambda_f, linetype = lambda_f)) +
    geom_line(size = 1) +
    labs(title = labs_01[1], x = labs_01[2], y = labs_01[3], color = "", linetype = "") +
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
    labs(title = labs_02[1], x = labs_02[2], y = labs_02[3], color = "", linetype = "") +
    theme_classic() +
    theme(legend.position="none",
          plot.title = element_text(hjust = 0.5)) +
    scale_x_continuous(expand = c(0, 0)) +							
    scale_y_continuous(expand = c(0, 0)) +
    tayloRswift::scale_color_taylor()
  
  # Gráfico de Confiabilidade
  g2 <- ggplot(dados, aes(x = t, y = R_t, color = lambda_f, linetype = lambda_f)) +
    geom_line(size = 1) +
    labs(title = labs_03[1], x = labs_03[2], y = labs_03[3], color = "", linetype = "") +
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

gera_plot_lognormal <- function(labs_01,labs_02,labs_03){
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
  
  # Gráfico Densidade
  g1 <- ggplot(dados, aes(x = t, y = ft_t, color = lambda_f, linetype = lambda_f)) +
    geom_line(size = 1) +
    labs(title = labs_01[1], x = labs_01[2], y = labs_01[3], color = "", linetype = "") +
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
    labs(title = labs_02[1], x = labs_02[2], y = labs_02[3], color = "", linetype = "") +
    theme_classic() +
    theme(legend.position="none",
          plot.title = element_text(hjust = 0.5)) +
    scale_x_continuous(expand = c(0, 0)) +							
    scale_y_continuous(expand = c(0, 0),limits = c(0,2)) +
    tayloRswift::scale_color_taylor()
  
  # Gráfico de Confiabilidade
  g2 <- ggplot(dados, aes(x = t, y = R_t, color = lambda_f, linetype = lambda_f)) +
    geom_line(size = 1) +
    labs(title = labs_03[1], x = labs_03[2], y = labs_03[3], color = "", linetype = "") +
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

gera_plot_degrada <- function(labs_degradacao){
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
    annotate("text", x = 2, y = failure_threshold -5, label = labs_degradacao[1], hjust = 0, angle = 0) +
    annotate("segment", x = 2.6, xend = 3, y = failure_threshold-4.5, yend = failure_threshold-0.5, 
             arrow = arrow(length = unit(0.2,"cm")), color = "black",linewidth = 1) +
    annotate("text", x = 6, y = 10, label = labs_degradacao[2], hjust = 0, angle = 0) +
    annotate("segment", x = 7, xend = 6.8, y = 10.5, yend = 14, 
             arrow = arrow(length = unit(0.2,"cm")), color = "black",linewidth = 1) +
    annotate("text", x = 7.5, y = failure_level + 2, label = labs_degradacao[3], hjust = 0, angle = 0) +
    annotate("segment", x = 8.5, xend = failure_time-0.15, y = failure_level+1.5, yend = failure_level+0.5, 
             arrow = arrow(length = unit(0.2,"cm")), color = "black",linewidth = 1) +
    labs(x = labs_degradacao[4], y = labs_degradacao[5]) +
    scale_x_continuous(expand = c(0, 0),limits = c(0,10.2)) +							
    scale_y_continuous(expand = c(0, 0),limits = c(0,25))+
    theme_classic()+
    tayloRswift::scale_color_taylor()
  print(p)
}

gera_plot_banheira <- function(labs_banheira){
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
    annotate("text", x = 14, y = 20, label = labs_banheira[1], hjust = 0) +
    annotate("text", x = 50, y = 14, label = labs_banheira[2], hjust = 0.5) +
    annotate("text", x = 74, y = 20, label = labs_banheira[3], hjust = 0) +
    labs(
      x = labs_banheira[4],
      y = expression(lambda(t))
    ) +
    theme_classic() +
    scale_y_continuous(expand = c(0, 0),limits = c(5,25)) +
    theme(axis.text = element_blank())
}

gera_plot_wiener <- function(labs_wiener){
  set.seed(123)
  df_aux <- gera_dados(n_obs=1,t_max=20,n_med=100,v=0,sigma = 4)
  df_aux1 <- gera_dados(n_obs=1,t_max=20,n_med=100,v=5,sigma = 4)
  
  df_aux$v <- "0"
  df_aux1$v <- "5"
  
  df<- rbind(df_aux,df_aux1)
  df$v <- factor(df$v,labels = c("μ = 0, σ = 4", "μ = 5, σ = 4"))
  
  g1 <- ggplot(df,aes(x=Time,y=Wt,colour = v)) +
    geom_line(size=1,alpha=0.9) +
    labs(y = labs_wiener[1], x = labs_wiener[2], color = "") +
    theme_classic() +
    theme(legend.title = element_blank(),
          legend.position = c(0.15,0.85)) +
    tayloRswift::scale_color_taylor() +
    scale_x_continuous(expand = c(0, 0),limits = c(0,20.5))
  
  print(g1)
}

gera_plot_reparos <- function(labs_reparos){
  aux <- data.frame(x= c(0,4,4,8),y=c(0,0.5,0.0,0.5))							
  g1 <- aux %>%							
    ggplot(aes(x=x,y=y)) +							
    geom_line(color = tayloRswift::swift_palettes$taylor1989[6],size = 1) +							
    theme_classic() +
    theme(legend.position="none",
          plot.title = element_text(hjust = 0.5)) +
    labs(x = labs_reparos[1],y=labs_reparos[2],title = labs_reparos[3]) +							
    scale_x_continuous(expand = c(0, 0)) + 
    scale_y_continuous(expand = c(0, 0),limits = c(0, 1))
  
  
  aux2 <- data.frame(x= c(0,8),y=c(0,1))							
  g2 <- aux2 %>%							
    ggplot(aes(x=x,y=y)) +							
    geom_line(color = tayloRswift::swift_palettes$taylor1989[6],size = 1) +							
    theme_classic() +
    theme(legend.position="none",
          plot.title = element_text(hjust = 0.5)) +
    labs(x = labs_reparos[1],y=labs_reparos[2],title = labs_reparos[4]) +							
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
    labs(x = labs_reparos[1], y = labs_reparos[2], title = labs_reparos[5]) +							
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

gera_plot_scheme <- function(labs_scheme){
  set.seed(1111)
  rho        <- c(0.6, 0.6)
  n_manu     <- 2
  intra_manu <- 4
  n_med      <- (n_manu+1)*(intra_manu+2)-(n_manu+1)
  df_degradacao1 <- gera_dados3(n_s=1, t_max=15, n_med=n_med,
                                v=2, sigma=sqrt(2), rho=rho, n_manu=n_manu)
  
  # Pre-compute Y values for clean annotation
  y0       <- df_degradacao1 %>% filter(Time==0)  %>% pull(Y)
  y1       <- df_degradacao1 %>% filter(Time==1)  %>% pull(Y)
  y2       <- df_degradacao1 %>% filter(Time==2)  %>% pull(Y)
  y3       <- df_degradacao1 %>% filter(Time==3)  %>% pull(Y)
  y4       <- df_degradacao1 %>% filter(Time==4)  %>% pull(Y)
  y5_min   <- df_degradacao1 %>% filter(Time==5)  %>% pull(Y) %>% min()
  y5_max   <- df_degradacao1 %>% filter(Time==5)  %>% pull(Y) %>% max()
  y5_mean  <- df_degradacao1 %>% filter(Time==5)  %>% summarise(m=mean(Y)) %>% pull()
  y10_min  <- df_degradacao1 %>% filter(Time==10) %>% pull(Y) %>% min()
  y10_max  <- df_degradacao1 %>% filter(Time==10) %>% pull(Y) %>% max()
  y10_mean <- df_degradacao1 %>% filter(Time==10) %>% summarise(m=mean(Y)) %>% pull()
  y15      <- df_degradacao1 %>% filter(Time==15) %>% pull(Y)
  
  df_degradacao1 %>%
    ggplot(aes(x=Time, y=Y)) +
    geom_point(size=2, colour=tayloRswift::swift_palettes$taylor1989[6]) +
    geom_line(linewidth=1, colour=tayloRswift::swift_palettes$taylor1989[6]) +
    
    # === DeltaY increments: staircase style, arrows close to data points ===
    # DeltaY_{0,1}: ref line at y0, arrow at x=1.15
    geom_text(x=1+0.65, y=y1-0.5,
              label=TeX("$\\Delta Y_{0,1}$"), size=4, colour="red") +
    annotate("segment", x=0.05, xend=1.15, y=y0, yend=y0,
             linetype="dashed", colour="red", linewidth=0.4) +
    annotate("segment", x=1.15, xend=1.15, y=y0, yend=y1,
             colour="red", linewidth=0.8,
             arrow=arrow(type="open", ends="both", angle=20, length=unit(0.3,"cm"))) +
    
    # DeltaY_{0,2}: ref line at y1, arrow at x=2.15
    geom_text(x=2+0.65, y=y2-0.5,
              label=TeX("$\\Delta Y_{0,2}$"), size=4, colour="red") +
    annotate("segment", x=1.05, xend=2.15, y=y1, yend=y1,
             linetype="dashed", colour="red", linewidth=0.4) +
    annotate("segment", x=2.15, xend=2.15, y=y1, yend=y2,
             colour="red", linewidth=0.8,
             arrow=arrow(type="open", ends="both", angle=20, length=unit(0.3,"cm"))) +
    
    # DeltaY_{0,n0+1}: ref line at y3, arrow at x=4.15, label above
    geom_text(x=4-0.3, y=y4+1.0,
              label=TeX("$\\Delta Y_{0,n_0+1}$"), size=4, colour="red") +
    annotate("segment", x=3.05, xend=4.15, y=y3, yend=y3,
             linetype="dashed", colour="red", linewidth=0.4) +
    annotate("segment", x=4.15, xend=4.15, y=y3, yend=y4,
             colour="red", linewidth=0.8,
             arrow=arrow(type="open", ends="both", angle=20, length=unit(0.3,"cm"))) +
    
    # === Maintenance jump Z_1 at t=5 ===
    geom_text(x=5-0.5, y=y5_min-0.5,   label=TeX("$Y(\\tau_{1}^{+})$"), size=4, colour="red") +
    geom_text(x=5+0.6, y=y5_mean,       label=TeX("$Z_1$"),               size=4, colour="red") +
    geom_text(x=5-0.5, y=y5_max+0.5,   label=TeX("$Y(\\tau_{1}^{-})$"), size=4, colour="red") +
    annotate("segment", x=5+0.3, xend=5+0.3, y=y5_min, yend=y5_max,
             colour="red", linewidth=1,
             arrow=arrow(type="open", ends="both", angle=20, length=unit(0.4,"cm"))) +
    
    # === Maintenance jump Z_2 at t=10 ===
    geom_text(x=10,     y=y10_min-0.7,  label=TeX("$Y(\\tau_{2}^{+})$"), size=4, colour="red") +
    geom_text(x=10+0.6, y=y10_mean,     label=TeX("$Z_2$"),               size=4, colour="red") +
    geom_text(x=10,     y=y10_max+0.7,  label=TeX("$Y(\\tau_{2}^{-})$"), size=4, colour="red") +
    annotate("segment", x=10+0.3, xend=10+0.3, y=y10_min, yend=y10_max,
             colour="red", linewidth=1,
             arrow=arrow(type="open", ends="both", angle=20, length=unit(0.4,"cm"))) +
    
    # === Final boundary at t=15 ===
    geom_text(x=15-0.7, y=y15, label=TeX("$Y(\\tau_{3}^{-})$"), size=4, colour="red") +
    
    theme_classic() +
    theme(plot.title=element_blank()) +
    scale_y_continuous(expand=c(0,0), limits=c(0,20)) +
    scale_x_continuous(expand=c(0,0), limits=c(0,16), breaks=c(5,10)) +
    labs(x=labs_scheme[1], y=labs_scheme[2])
}

gera_plot_bias <- function(resultados,labs_bias){
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
    scale_x_continuous(breaks = c(1,10,20,30,40,50)) +
    labs(x = labs_bias[1],
         y = labs_bias[2]) +
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

gera_plot_xtyt <- function(labs_xtyt){
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
      labels = c(labs_xtyt[1], labs_xtyt[2])
    ) +
    theme_classic() +
    theme(
      legend.position = "top",
      plot.title = element_blank()
    ) +
    labs(x = labs_xtyt[3], y = labs_xtyt[4], title = "(I)") +
    scale_y_continuous(expand = c(0, 0)) +
    scale_x_continuous(expand = c(0, 0))
  
  return(p)
}

gera_plot_rmse <- function(resultados,labs_rmse){
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
    scale_x_continuous(breaks = c(1,10,20,30,40,50)) +
    labs(x = labs_rmse[1],
         y = labs_rmse[2]) +
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

gera_plot_coveragep <- function(resultados,labs_coverage){
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
    scale_x_continuous(breaks = c(1,10,20,30,40,50)) +
    labs(x = labs_coverage[1],
         y = labs_coverage[2]) +
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

gera_plot_ratiovar <- function(resultados,labs_ratiovar){
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
    scale_x_continuous(breaks = c(1,10,20,30,40,50)) +
    geom_hline(yintercept = 1, linetype = "dashed", color = "red", linewidth = 0.5) +
    labs(x = labs_ratiovar[1],
         y = labs_ratiovar[2]) +
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

gera_plot_merito <- function(mu,sigma,alpha,t0,x0,t_max,labs_merito01,labs_merito02){
  media <- (alpha[1]-x0)/mu
  desvio <- ((alpha[1]-x0)^2)/sigma
  t = seq(t0,t_max,by=0.1) # Tempo absoluto
  # Tempo Relativo
  tau <- t - t0
  aux <- statmod::dinvgauss(tau,mean=media,shape = desvio)
  df_visu <- data.frame(time = t,r_mean=aux)
  
  g1 <- df_visu %>%
    ggplot(aes(x=time,y=r_mean)) +
    geom_line(linewidth=1,alpha=0.7,color=tayloRswift::swift_palettes$taylor1989[1]) +
    geom_vline(xintercept = t0-2) +
    geom_vline(xintercept = t0,
               colour="black", linetype = "longdash") +
    theme_classic() +
    theme(legend.position = c(0.9,0.8),
          axis.text.y = ggtext::element_markdown(),
          plot.title = element_text(hjust = 0.5)
    ) +
    labs(title = labs_merito01[1],
         x = labs_merito01[2],
         y = labs_merito01[3]) +
    coord_cartesian(ylim=c(0,0.022),expand = FALSE) +
    #   scale_color_viridis(discrete = TRUE, option = "D")
    tayloRswift::scale_color_taylor(palette = "taylor1989",reverse = FALSE) +
    annotate("text",x=(t0 + 5.0), y= 0.005,							
             label=paste("t=",t0),size = 3,colour="black")
  
  aux <- statmod::pinvgauss(tau,mean=media,shape = desvio)
  plot(t,aux)
  df_visu <- data.frame(time = t,r_mean=aux)
  g2 <- df_visu %>%
    ggplot(aes(x=time,y=r_mean)) +
    geom_line(linewidth=1,alpha=0.7,color=tayloRswift::swift_palettes$taylor1989[1]) +
    geom_vline(xintercept = t0-2) +
    geom_vline(xintercept = t0,
               colour="black", linetype = "longdash") +
    theme_classic() +
    theme(legend.position = c(0.9,0.8),
          axis.text.y = ggtext::element_markdown(),
          plot.title = element_text(hjust = 0.5)
    ) +
    labs(title = labs_merito02[1],
         x = labs_merito02[2],
         y = labs_merito02[3]) +
    coord_cartesian(ylim=c(0,1),expand = FALSE) +
    #   scale_color_viridis(discrete = TRUE, option = "D")
    tayloRswift::scale_color_taylor(palette = "taylor1989",reverse = FALSE) +
    annotate("text",x=(t0 + 5), y= 0.25,							
             label=paste("t=",t0),size = 3,colour="black")
  
  layout <- "
  AB
  "
  g1+g2 +
    plot_layout(design = layout)
}


gera_plot_qqplot <- function(sub_maria,labs_qqplot01,labs_qqplot02){
  
  mu_hat <- mle_drift1_y(sub_maria)
  sigma2_hat <- mle_sigma1_y(sub_maria)
  incrementos = diff(sub_maria$Y)[-c(14,28,42)]
  
  
  anderson_d <- ADGofTest::ad.test(incrementos, pnorm, mu_hat, sqrt(sigma2_hat))
  estatistica_ad <- anderson_d$statistic
  p_valor <- anderson_d$p.value
  
  
  e1 <- fitdist(incrementos, "norm", start = list(mean = mu_hat, sd = sqrt(sigma2_hat)))
  
  
  pp_data <- data.frame(
    x = pnorm(sort(incrementos), mean = mu_hat, sd = sqrt(sigma2_hat)),
    y = ecdf(incrementos)(sort(incrementos))
  )
  
  pp_plot <- ggplot(pp_data, aes(x = x, y = y)) +
    geom_point(color=tayloRswift::swift_palettes$taylor1989[6]) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
    labs(x = labs_qqplot01[1], y = labs_qqplot01[2], title = labs_qqplot01[3]) +
    theme_classic() +
    theme(plot.title = element_text(hjust = 0.5))
  
  # Criando o Q-Q Plot
  qq_plot <- ggplot(data.frame(sample = incrementos), aes(sample = sample)) +
    stat_qq(distribution = qnorm, dparams = list(mean = mu_hat, sd = sqrt(sigma2_hat)),
            color=tayloRswift::swift_palettes$taylor1989[6]) +
    stat_qq_line(distribution = qnorm, dparams = list(mean = mu_hat, sd = sqrt(sigma2_hat)),
                 color = "red", linetype = "dashed") +
    labs(x = labs_qqplot02[1], y = labs_qqplot02[2], title = labs_qqplot02[3]) +
    theme_classic() +
    theme(plot.title = element_text(hjust = 0.5))
  
  
  # Exibir os gráficos lado a lado com legenda do p-valor do AD Test
  graf_diag<-pp_plot + qq_plot + 
    plot_annotation(title = sprintf("AD Test: %.4f, p-valor = %.4f", estatistica_ad, p_valor))
  
  return(graf_diag)
}

gera_plot_degrada01 <- function(labs_degrada01){
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
    labs(x = labs_degrada01[1], y = labs_degrada01[2]) +							
    scale_x_continuous(expand = c(0, 0),breaks = c(0,2,4,6,8,10),limits = c(0,11)) +							
    scale_y_continuous(expand = c(0, 0), limits = c(0,65)) +							
    tayloRswift::scale_color_taylor()
}

gera_plot_confiabilidade <- function(){
  lambda <- 100
  tempos <- seq(0, 405, length.out = 10000)
  prob <- pexp(tempos,rate = 1/lambda,lower.tail = FALSE)
  tmedio <- qexp(0.5,rate=1/lambda)
  df <- tibble(tempos,prob)
  ggplot(df,aes(x = tempos, y = prob,)) +
    labs(x = "Tempo", y = "R(t)") +
    geom_line(color = tayloRswift::swift_palettes$taylor1989[6], linewidth = 1) +
    theme_classic() +
    theme(
      legend.position = "none",
      plot.title = element_text(hjust = 0.5),
      # Use element_markdown() para interpretar a cor no rótulo do eixo x
      axis.text.x = element_markdown() 
    ) +
    scale_x_continuous(expand = c(0, 0),breaks = c(0,69,100,200,300,400),
                       labels = c(0,paste0("<span style='color:red;'>", "69", "</span>"), 100 , 200 , 300 ,400)) +
    scale_y_continuous(expand = c(0, 0)) +
    tayloRswift::scale_color_taylor() +
    geom_segment(aes(x=69,xend=69,y=0,yend=0.5),
                 linetype = "8f",linewidth=0.1, alpha = 0.2,lineend = "round",
                 colour=tayloRswift::swift_palettes$taylor1989[4]) +
    geom_segment(aes(x=0,xend=69,y=0.5,yend=0.5),
                 linetype = "8f",linewidth=0.1, alpha = 0.2,lineend = "round",
                 colour=tayloRswift::swift_palettes$taylor1989[4])
}

plot_reliability_ic <- function(mu, sigma2, var_mu, var_sigma2, alpha, t0, x0, t_max, xlab, ylab, paleta = "taylor1989"){
  library(ggtext)
  library(numDeriv)
  library(dplyr)
  library(ggplot2)
  library(tayloRswift)
  
  # Matriz de variância-covariância (independente, conforme dados fornecidos)
  vcov_params <- diag(c(var_mu, var_sigma2))
  
  # Função de Sobrevivência (S(t)) para Gaussiana Inversa
  surv_func <- function(p, tau, a, x_z) {
    curr_mu <- p[1]
    curr_sig2 <- p[2]
    # mean = (alpha-x0)/mu ; shape = (alpha-x0)^2/sigma2
    m <- (a - x_z) / curr_mu
    s <- ((a - x_z)^2) / curr_sig2
    statmod::pinvgauss(tau, mean = m, shape = s, lower.tail = FALSE)
  }
  
  # Grade de tempo para o gráfico
  t_seq = seq(t0 + 0.1, t_max, by = 0.1) 
  tau_seq <- t_seq - t0
  
  # Cálculo das estimativas e IC ponto a ponto
  df_visu <- lapply(tau_seq, function(tau) {
    # 1. Estimativa Pontual
    r_val <- surv_func(c(mu, sigma2), tau, alpha, x0)
    
    # 2. Erro Padrão via Método Delta (Gradiente Numérico)
    grad_val <- numDeriv::grad(function(p) surv_func(p, tau, alpha, x0), c(mu, sigma2))
    var_s <- t(grad_val) %*% vcov_params %*% grad_val
    se_s <- sqrt(max(0, var_s))
    
    # 3. Transformação Log-Log (Garante limites [0,1] e evita Z explosivo)
    # Z = 1.282 para IC 80% ponto a ponto
    if (r_val > 0.0001 & r_val < 0.9999) {
      log_r <- log(r_val)
      se_log_log <- se_s / (r_val * abs(log_r))
      fator <- exp(1.282 * se_log_log)
      lower <- r_val^fator
      upper <- r_val^(1/fator)
    } else {
      lower <- r_val
      upper <- r_val
    }
    
    data.frame(time = tau + t0, r_mean = r_val, lower = lower, upper = upper, Threshold = as.factor(alpha))
  }) %>% bind_rows()
  
  # Visualização com sua estética original
  p <- ggplot(df_visu, aes(x = time, y = r_mean, color = Threshold, fill = Threshold)) +
    # IC 95% (Ribbon)
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, color = NA) +
    # Linha da Confiabilidade
    geom_line(linewidth = 1, alpha = 0.7) +
    scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
    geom_line(linewidth=1,alpha=0.7) +
    geom_vline(xintercept = t0-2) +
    geom_vline(xintercept = t0,
               colour="black", linetype = "longdash") +
    theme_classic() +
    theme(plot.title = element_blank(),
          legend.position = "none",
          axis.text.y = ggtext::element_markdown()) +
    labs(title = "(II)", x = xlab, y = ylab) +
    coord_cartesian(expand = FALSE) +
    tayloRswift::scale_color_taylor(palette = paleta) +
    tayloRswift::scale_fill_taylor(palette = paleta) +
    annotate("text", x = (t0 + 5), y = 0.08, 
             label = paste("t=", t0), size = 3, colour = "black")
  
  return(list(p = p,
              df_visu = df_visu))
}
