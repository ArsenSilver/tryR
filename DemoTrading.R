# Загрузим и подключим необходимые библиотеки
list.of.packages <- c("quantmod", 'magrittr', 'xts', 'dplyr')
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library(quantmod)
library(magrittr)
library(xts)
library(dplyr)


from <- "2005-01-01"
to <- "2020-11-15"
asset1 <- new.env()
asset2 <- new.env()
asset3 <- new.env()
getSymbols(Symbols ='IWM',env =asset1,from="2005-01-01",to="2020-11-15")
getSymbols(Symbols ='SPY',env =asset2,from="2005-01-01",to="2020-11-15")
getSymbols(Symbols ='TLT',env =asset3,from="2005-01-01",to="2020-11-15")

# Сгрупируем активы в один xts объект и удалим пустые строки
data <- na.omit(cbind(asset1[['IWM']][,6],asset2[['SPY']][,6], asset3[['TLT']][,6]))


# Установим параметры стратегии
{
  # параметр, отвечающий за частоту изменения состава портфеля
  holding.period <- 21
  
  # параметры для определения доходности
  short.period <- 21
  middle.period <- 62
  long.period <- 100
  
  # отступ стратегии
  lookback <- 250
  
  # капитал стратегии
  money <- 1000000
}

# Функция, считающая вектор весов в последней точке данных.

getWeights <- function(data) {
  IWM=(data[length(data[,1]),1]/data[length(data[,1])-21,1]+data[length(data[,1]),1]/data[length(data[,1])-62,1]+data[length(data[,1]),1]/data[length(data[,1])-100,1]-3)/3
  SPY=(data[length(data[,2]),2]/data[length(data[,2])-21,2]+data[length(data[,2]),2]/data[length(data[,2])-62,2]+data[length(data[,2]),2]/data[length(data[,2])-100,2]-3)/3
  if( IWM < 0 && SPY < 0)
    return (c(0,0,1)) else
  if(SPY > IWM) 
    return (c(0,1,0)) else
    return (c(1,0,0))
}
# Проверим работоспособность функции:
getWeights(coredata(data))
# результат должен быть с(1, 0, 0)
cur.weights <- getWeights(backtest.data[range, ])
day <- lookback + 1 # первый торговый день                
trade.day <- day # день, в который будет перестройка портфеля                  
backtest.data <- as.matrix(data) # данные в виде матрицы

# здесь будут храниться приращения PnL по каждому активу
pnl_leg <- matrix(0L, nrow = nrow(backtest.data), ncol = ncol(backtest.data)) %>% set_colnames(names(data))

# Посчитаем, сколько активов мы можем купить 
assets.count <- numeric(ncol(backtest.data))
assets.count <- floor(money * cur.weights / backtest.data[day, ])
while(day <= nrow(backtest.data)) {
  range <- (day - lookback):day


  #вектор pnl - результат торгового дня
  pnl_leg[day,] <-assets.count*backtest.data[day, ] - assets.count*backtest.data[day-1, ]  # YOUR CODE
  # Если мы сейчас должны удерживать позицию
  if (day < trade.day) {
    day <- day + 1 
    next
  }
  # Если нет, то получаем новые веса и меняем (если веса получились другие) купленные активы
  range <- (day - lookback):day
  cur.weights <- getWeights(backtest.data[range, ])
  assets.count <- floor(money * cur.weights / backtest.data[day, ])
  day <- day + 1
  trade.day <- trade.day + holding.period
}


plot(pnl_leg[,'SPY.Adjusted'] + pnl_leg[,'IWM.Adjusted'] + pnl_leg[,'TLT.Adjusted'], type = 'l')
len <-nrow(backtest.data)

pnl <- pnl_leg[,'SPY.Adjusted'] + pnl_leg[,'IWM.Adjusted'] + pnl_leg[,'TLT.Adjusted']


plot(pnl, type = 'l')
df <- data.frame(index = 1:length(pnl), value = pnl)
write.csv(df, "pnl_inc.csv", row.names = FALSE)

m <- cumsum(pnl)+money
plot(m, type = 'l')
df <- data.frame(index = 1:length(m), value = m)
write.csv(df, "pnl.csv", row.names = FALSE)

sortino_fun <- function(x) {
  sum(x) / sqrt(sum(pmin(x, 0)**2)) * sqrt(252 / length(x))
}
sharpe_fun <- function(x) {
  sum(x)/length(x)/sd(x) * sqrt(252)
}
max_drawdown_fun <- function(x) {
  min(pnl - cummax(pnl)) / 1000000
}
return_ann_fun <- function(x) {
  sum(x)/length(x) * 252/ 1000000
}
std_ann_fun <- function(x) {
  sd(x) * sqrt(252) 
}

functions <- list('sharpe'= sharpe_fun,
                  'max_drawdown'= max_drawdown_fun,
                  'return_ann' = return_ann_fun, 
                  'std_ann' = std_ann_fun)


stats_sum <- lapply(1:4,function(i) functions[[i]](pnl)) # YOUR CODE
saveRDS(stats_sum, 'stats_sum.RDS')


stats_by_leg  <- lapply(1:4,function(i) apply(pnl_leg, 2,functions[[i]])) # YOUR CODE
saveRDS(stats_by_leg, 'stats_by_leg.RDS')

