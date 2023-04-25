library(tidyverse)
library(tidyquant)
library(timetk)

#Simbolos de los activos
symbols <- sort(c("BTC-USD", "SPY", "DBC", "XLK", "EWC"))

#Sacar los precios de los activos
prices <- 
  getSymbols(symbols, src = 'yahoo', from = "2020-01-01", auto.assign = TRUE, warnings = FALSE) %>% 
  lapply(function(sym) Cl(get(sym))) %>%
  do.call(merge, .) %>% 
  `colnames<-`(symbols) %>%
  na.omit()

#Comprobar los resultadoss
head(prices)

#Retornos logaritmicos mensuales en formato xts
prices_m <- to.monthly(prices, indexAt = "last", OHLC = FALSE)
returns_xts <- na.omit(Return.calculate(prices_m, method = "log"))
##returns_xts <- na.omit(Return.calculate(prices_m, method = "simple"))

#Comprobar los retornos mensuales
head(returns_xts)

#Retornos logaritmicos mensuales en formato long
returns_long <- 
  prices %>% 
  to.monthly(indexAt = "last", OHLC = FALSE) %>% 
  tk_tbl(preserve_index = TRUE, rename_index = "date") %>%
  gather(asset, returns, -date) %>% 
  group_by(asset) %>%  
  mutate(returns = (log(returns) - log(lag(returns))))
  #mutate(returns = return.calculate(returns, method = "simple"))

#Comprobar los retornos mensuales
head(returns_long)

#Asignar pesos a los activos de la cartera
weights <- c(0.20, 0.10, 0.20, 0.30, 0.20)

#Comprobacion pesos de la cartera
asset_weights_sanity_check <- tibble(weights, symbols)
asset_weights_sanity_check

sum(asset_weights_sanity_check$weights)

##########################################
#Primera forma de calcular la rentabilidad

weigh_1 <- weights[1]
weigh_2 <- weights[2]
weigh_3 <- weights[3]
weigh_4 <- weights[4]
weigh_5 <- weights[5]

asset1 <- returns_xts[,1]
asset2 <- returns_xts[,2]
asset3 <- returns_xts[,3]
asset4 <- returns_xts[,4]
asset5 <- returns_xts[,5]

returns_byhand <-   
  (weigh_1 * asset1) + 
  (weigh_2 * asset2) + 
  (weigh_3 * asset3) +
  (weigh_4 * asset4) + 
  (weigh_5 * asset5)


##########################################
#Segunda forma de calcular la rentabilidad

returns_xts_rebalanced_monthly <- 
  Return.portfolio(returns_xts, weights = weights, rebalance_on = "months") %>%
  `colnames<-`("returns")

#Comprobar los retornos reequilibrado por meses
head(returns_xts_rebalanced_monthly)

returns_xts_rebalanced_yearly <- 
  Return.portfolio(returns_xts, weights = weights, rebalance_on = "years") %>%
  `colnames<-`("returns")

#Comprobar los retornos reequilibrado por años
head(returns_xts_rebalanced_yearly)

##########################################
#Tercera forma de calcular la rentabilidad

returns_tq_rebalanced_monthly <- 
  returns_long %>%
  tq_portfolio(assets_col  = asset, 
               returns_col = returns,
               weights     = weights,
               col_rename  = "returns",
               rebalance_on = "months")


#Comprobar los retornos reequilibrado por meses
head(returns_tq_rebalanced_monthly)

returns_tq_rebalanced_yearly <- 
  returns_long %>%
  tq_portfolio(assets_col  = asset, 
               returns_col = returns,
               weights     = weights,
               col_rename  = "returns",
               rebalance_on = "years")


#Comprobar los retornos reequilibrado por años
head(returns_tq_rebalanced_monthly)