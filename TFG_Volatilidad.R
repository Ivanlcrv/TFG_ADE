library(quantmod)
library(tidyquant)
library(dplyr)
library(tidyr)
library(xts)

#Simbolos de los activos
symbols <- sort(c("BTC-USD", "SPY", "DBC", "XLK", "EWC"))

#Sacar los precios de los activos
prices <- 
  getSymbols(symbols, src = 'yahoo', from = "2020-01-01", auto.assign = TRUE, warnings = FALSE) %>% 
  lapply(function(sym) Cl(get(sym))) %>%
  do.call(merge, .) %>% 
  `colnames<-`(symbols) %>%
  na.omit()

#Comprobar los resultados
head(prices)

#Sacar los retornos mensuales de los activos en base a los resultados anteriores
portfolio <- 
  prices %>%  
  as.data.frame() %>%
  tibble::rownames_to_column(var = "date") %>% 
  mutate(date = as.Date(date)) %>%
  select(date, everything()) %>%
  tidyr::pivot_longer(cols = -date, names_to = "asset", values_to = "return") %>%
  group_by(asset) %>%
  tq_transmute(select = return, mutate_fun = periodReturn, period = "monthly", col_rename = "return_monthly") %>%
  spread(key = asset, value = return_monthly) %>% 
  xts(order.by = .$date)

aux <- portfolio

#Convertir valores del portfolio a tipo numerico
portfolio <- apply(portfolio, 2, as.numeric)

#Eliminar columna fechas
portfolio <- portfolio[, -1]

#Asignar nombres de fila al portfolio
rownames(portfolio) <-aux [,1]

#Comprobar los retornos mensuales
head(portfolio)

#Asignar pesos a los activos de la cartera
weights <- c(0.20, 0.10, 0.20, 0.30, 0.20)

#Comprobacion pesos de la cartera
asset_weights_sanity_check <- tibble(weights, symbols)
asset_weights_sanity_check

sum(asset_weights_sanity_check$weights)

#########################################
#Primera forma de calcular la volatilidad

weigh_1 <- weights[1]
weigh_2 <- weights[2]
weigh_3 <- weights[3]
weigh_4 <- weights[4]
weigh_5 <- weights[5]

asset1 <- portfolio[,1]
asset2 <- portfolio[,2]
asset3 <- portfolio[,3]
asset4 <- portfolio[,4]
asset5 <- portfolio[,5]

#Desviación típica usando la ecuación
sd <- 
  sqrt(
    (weigh_1^2 * var(asset1)) + (weigh_2^2 * var(asset2)) + 
      (weigh_3^2 * var(asset3)) + (weigh_4^2 * var(asset4)) + 
        (weigh_5^2 * var(asset5)) +
          (2 * weigh_1 * weigh_2 * cov(asset1, asset2)) +  
          (2 * weigh_1 * weigh_3 * cov(asset1, asset3)) +
          (2 * weigh_1 * weigh_4 * cov(asset1, asset4)) +
          (2 * weigh_1 * weigh_5 * cov(asset1, asset5)) +
          (2 * weigh_2 * weigh_3 * cov(asset2, asset3)) +
          (2 * weigh_2 * weigh_4 * cov(asset2, asset4)) +
          (2 * weigh_2 * weigh_5 * cov(asset2, asset5)) +
          (2 * weigh_3 * weigh_4 * cov(asset3, asset4)) +
          (2 * weigh_3 * weigh_5 * cov(asset3, asset5)) +
          (2 * weigh_4 * weigh_5 * cov(asset4, asset5))
  )

#Resultado en porcentaje y redondeo
sd_percent <- round(sd * 100, 2)

#########################################
#Segunda forma de calcular la volatilidad

cov_matrix <- cov(portfolio)
cov_matrix

#Desviación típica usando algebra matricial
sd_matrix_algebra <- sqrt(t(weights) %*% cov_matrix %*% weights)

#Resultado en porcentaje y redondeo
sd_matrix_algebra_percent <- round(sd_matrix_algebra * 100, 2)

#########################################
#Tercera forma de calcular la volatilidad

#Desviación típica usando función integrada de performaceAnalytics
portfolio_sd <- StdDev(portfolio, weights = weights)

#Resultado en porcentaje y redondeo
portfolio_sd_percent <- round(portfolio_sd * 100, 2)

#########################################
#Sentido del proceso, comparativa con BTC

btc_return <- portfolio[,"BTC-USD"]

#Desviación típica
btc_sd <-StdDev(btc_return)

#Resultado en porcentaje y redondeo
btc_sd_percent <- round(btc_sd * 100, 2)