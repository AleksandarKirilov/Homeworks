library(tidyverse)
library(tidyquant)


#Problem1
AMZN_prices <- tq_get("AMZN", complete_cases = TRUE, get = "stock.prices", from = "2022-01-10", to = "2022-04-28")
AMZN_prices <- AMZN_prices %>% select(symbol,date,adjusted)
AMZN_prices <- AMZN_prices %>% 
  complete(date = seq.Date(as.Date("2022-01-10"), max(date), by="day")) %>% 
  fill(symbol, .direction = "downup") %>%
  fill(adjusted, .direction = "downup")

AMZN_prices <-  AMZN_prices %>% tq_mutate(adjusted, periodReturn, period = "daily", col_rename = "returns")

# 1.1
std <- as.integer(summarise(AMZN_prices, sd = sd(adjusted)))

AMZN_prices <- AMZN_prices %>% 
  tq_mutate(select=adjusted, mutate_fun = SMA, n = 20) %>% 
  mutate(lowerbound = SMA -std, upperbound = SMA + std)



# 1.2
AMZN_prices <- AMZN_prices %>%
  mutate(strategie = ifelse( adjusted<lowerbound | adjusted<upperbound , "Buy","Sell"))

newdata <- na.omit(AMZN_prices)

lg <- length(newdata$adjusted)
benchmark_return <- (newdata$adjusted[lg] - newdata$adjusted[1]) / newdata$adjusted[1]
benchmark_return

pt <- newdata$adjusted[1]
portfolio<-100
hold <- FALSE
for (t in 1:lg){
  if(newdata$strategie[t]=="Buy" && hold==FALSE){
    pt <- newdata$adjusted[t]
    hold <- TRUE
    print(portfolio)
  } else if (newdata$strategie[t]=="Sell" && hold==TRUE){
    ptt <- newdata$adjusted[t]
    price_return <- (ptt-pt)/pt
    portfolio <- portfolio * (1+price_return)
    hold <-FALSE
    print(portfolio)
  }
}
portfolio




print("Portfolio initial: 100$")
print("With the benchmark strategie, your have at the end: ")
print((1+benchmark_return)*100)
print("With the new strategie, your have at the end: ")
print(portfolio)


#Problem2

AMZN_prices <- tq_get("AMZN", complete_cases = TRUE, get = "stock.prices", from = "2022-01-10", to = "2022-04-28")
AMZN_prices <- AMZN_prices %>% select(symbol,date,adjusted)
AMZN_prices <- AMZN_prices %>% 
  complete(date = seq.Date(as.Date("2022-01-10"), max(date), by="day")) %>% 
  fill(symbol, .direction = "downup") %>%
  fill(adjusted, .direction = "downup")

AMZN_prices <- AMZN_prices %>% 
  tq_mutate(select=adjusted, mutate_fun = RSI)

AMZN_prices <- AMZN_prices %>%
  mutate(strategie = ifelse( rsi<35 | rsi<65 , "Buy","Sell"))

newdata <- na.omit(AMZN_prices)

lg <- length(newdata$adjusted)
benchmark_return <- (newdata$adjusted[lg] - newdata$adjusted[1]) / newdata$adjusted[1]
benchmark_return

pt <- newdata$adjusted[1]
portfolio<-100
hold <- FALSE
for (t in 1:lg){
  if(newdata$strategie[t]=="Buy" && hold==FALSE){
    pt <- newdata$adjusted[t]
    hold <- TRUE
    print(portfolio)
  } else if (newdata$strategie[t]=="Sell" && hold==TRUE){
    ptt <- newdata$adjusted[t]
    price_return <- (ptt-pt)/pt
    portfolio <- portfolio * (1+price_return)
    hold <-FALSE
    print(portfolio)
  }
}
portfolio





