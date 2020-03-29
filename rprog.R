  # in some cases may be required #apt install libcurl-dev
  list.of.packages <- c("quantmod", 'magrittr', 'xts', 'dplyr')
  new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
  if(length(new.packages)) install.packages(new.packages)
  for (i in 1:length(list.of.packages)) 
  library(list.of.packages[i], character.only=TRUE)



asset1 <- new.env()
asset2 <- new.env()
asset3 <- new.env()
getSymbols(Symbols ='IWM',env =asset1,from="2005-01-01",to="2019-04-02")
getSymbols(Symbols ='SPY',env =asset2,from="2005-01-01",to="2019-04-02")
getSymbols(Symbols ='TLT',env =asset3,from="2005-01-01",to="2019-04-02")


data <- na.omit(cbind(asset1[['IWM']][,6],asset2[['SPY']][,6], asset3[['TLT']][,6]))
if is.na(data)
 cat("Error N",tail(unique(seq(2, 3, by=1)),-1),"\n")
holding.period <-21

getWeights <- function(data) { # funcs args appends with rep() if it needed
 IWM=(data[length(data[,1]),1]-data[length(data[,1])-21,1]+data[length(data[,1]),1]-data[length(data[,1])-62,1]+data[length(data[,1]),1]-data[length(data[,1])-100,1])/3
 SPY=(data[length(data[,2]),2]-data[length(data[,2])-21,2]+data[length(data[,2]),2]-data[length(data[,2])-62,2]+data[length(data[,2]),2]-data[length(data[,2])-100,2])/3
  if( IWM < 0 && SPY < 0)
    return (c(0,0,1)) else
  if(SPY > IWM) 
    return (c(0,1,0)) else
    return (c(1,0,0))
}

lookback <- 250
day <- lookback + 1                 
trade.day <- day                   
money <- 1000000
money.change <- 1000000                                      # Set capital
hold.index <- 0                     
backtest.data <- as.matrix(data)
d <- mean(diag(backtest.data))
pnl <- numeric(nrow(backtest.data)) # A vector which will collect PnL during backtesting
range <- (day - lookback):day
                                    
cur.weights <- getWeights(backtest.data[range, ])

assets.count <- floor(money * cur.weights / backtest.data[day, ])
pnl[1:250]=0
money.change=money.change- max(assets.count*backtest.data[day, ])
prev.day.money<-max(assets.count*backtest.data[day, ])+money.change
day <- day + 1
trade.day <- trade.day + holding.period

while(day <= nrow(backtest.data)) {

  pnl[day] <-max(assets.count*backtest.data[day, ])+money.change-prev.day.money


  if (day < trade.day) {
    prev.day.money<-max(assets.count*backtest.data[day, ])+money.change
    day <- day + 1 

    next
  }
  money.change=money.change+max(assets.count*backtest.data[day, ])

  range <- (day - lookback):day
  cur.weights <- getWeights(backtest.data[range, ])
  assets.count <- floor(money * cur.weights / backtest.data[day, ])
  money.change=money.change- max(assets.count*backtest.data[day, ])
  prev.day.money<-max(assets.count*backtest.data[day, ])+money.change
  day <- day + 1
  trade.day <- trade.day + holding.period
  

}

plot(pnl, type = 'l') #table() , hist(), matplot()

m <- cumsum(pnl)+money
plot(m, type = 'l')



dice_roll<- function(n) 
{
  .varName=trunc(runif(n,min=1, max=7)) #or sample.int(6,n) for we count starting from '1'
  #if the function has no 'return' it returns last operation
}

some_equalities<- function(x)
{
  length(x[x>0])==sum(ifelse(x>0,1,0))
  "string"=='string' 
   #character can't be implicitly cast into ASCII integer beacuse it is string[1]

}
# reminder for some R functions:
#which.max/.min is scalar function, but when(max()) vector
#mediane(), quantile(), any(),all(), all.*, ?colMeans, attributes(), ?pmax, ??cumulative,
#array$indexName, ?LETTERS, apply()