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
  n_med <- c(table(data$Objeto))
  n_obs <- c(length(n_med))
  last_index <- c(seq(1:n_obs) * n_med)
  last_d <- data$Wt[last_index]
  last_t <- data$Time[last_index]
  
  first_index <- c(0, last_index[-n_obs]) + 2
  first_d <- data$Wt[first_index]
  first_t <- data$Time[first_index]
  
  m_bar <- mean(c(table(data$Objeto)))
  
  y_ij <- diff(data$Wt[(first_index)[1]:(n_med * seq(1:n_obs))[1]])
  s_ij <- diff(data$Time[(first_index)[1]:(n_med * seq(1:n_obs))[1]])
  for (i in 2:n_obs) {
    aux <- diff(data$Wt[(first_index)[i]:(n_med * seq(1:n_obs))[i]])
    y_ij <- c(y_ij, aux)
    aux1 <- diff(data$Time[(first_index)[i]:(n_med * seq(1:n_obs))[i]])
    s_ij <- c(s_ij, aux1)
  }
  
  aux2 <- rep(NA, length(s_ij))
  for (i in 1:length(s_ij)) {
    aux2[i] <- (y_ij[i]^2) / s_ij[i]
  }
  aux3 <- 1 / (m_bar * (n_obs-1))
  aux4 <- n_obs * ((mean(last_d) - mean(first_d))^2 / (mean(last_t) - mean(first_t)))
  resultado <- aux3 * (sum(aux2) - aux4)
  resultado <- sqrt(resultado)
  
  return(resultado)
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
    yji[1] <- sum(aux[1:(k[1])])
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
      title = "Degradation Paths",
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
  sigma2_hat_unbiased <- sigma2_hat_biased*(N+length(k)+1)/(N+length(k))
  return(sqrt(sigma2_hat_biased))
}




#################################

gera_plot4 <- function(data) {
  p <- data %>%
    ggplot() +
    geom_line(aes(x = Time, y = Y, colour = "With Maintenance"), alpha = 0.5, linetype = "solid", linewidth = 1) +
    labs(
      x = "Time",
      y = "Degradation",
      title = "Degradation Paths",
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

###########
#### Calcular Estimativa de \mu considerando Y_t em vez de X_t

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


####
#### Mle Sigma para Y_t em vez de X_t
####

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


