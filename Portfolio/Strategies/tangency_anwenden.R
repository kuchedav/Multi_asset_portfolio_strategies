### tangency_anwenden function calculates the tangecy or Sharpe Ratio Portfolio

# Input
# RAW.Data = xts Matrix of returns of indices 
# Lookback_distance = the amount of days that will be calculated, from today into the past.
# lag_days = amount of days the algorythm waits until it buys the indeces (1 is standart, to wait until tomorrow)

# Output
# List of two Matrixes, 
# first contains a xts matrix with all weights over the calculated period of time per index
# second list contains an xts vector of the portfolio returns

tangency_anwenden <- function(RAW.Data, Lookback_distance = 100, lag_days = 1){
  
  N <- Lookback_distance # max = 4000
  
  #### Create tangency Portfolio ###
  eff.port.list2 <- list()
  
  ### Multi Core
  require(parallel)
  require(doMC)
  registerDoMC(cores=detectCores())
  
  tangency_function <- function(RAW.Data,i){
    ret.cc2 <- RAW.Data[1:(nrow(RAW.Data)-i),]
    mu.hat.annual = apply(ret.cc2, 2, mean)*252
    cov.mat.annual = cov(ret.cc2)*252
    effi.port = tangency.portfolio(er = mu.hat.annual,cov.mat =  cov.mat.annual,risk.free = 0.12/100)
    eff.port.list2[[i]] <- effi.port
  }
  
  eff.port.list2 <- foreach(n = 1:N) %dopar% tangency_function(RAW.Data,n)
  
  weights2 <- sapply(N:1, function(i) eff.port.list2[[i]]$weights)
  weights_xts <- xts(t(weights2),order.by = index(RAW.Data[nrow(RAW.Data):(nrow(RAW.Data)-(N-1)),]) )
  
  # Calculate the Portfolio returns with the weights and raw data
  return <- CalculatePortfolioReturns(RAW.Data,weights_xts,lag_days)
  
  return(list(weights=weights_xts,returns=return))
}


### Example
# library(xts)
# length <- 20000
# cols <- 20
# lookback <- 100
# 
# data <- as.xts(matrix(rnorm(length),ncol=cols), order.by = as.Date(1:(length/cols))) # Generate normal dist. data
# colnames(data) <- c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20") # puts Index names from 1 - 20
# matplot(data, type = "l")
# 
# 
# lookback <- 5000
# portfolio <- tangency_anwenden(data,lookback) # call the function above with random normal dist. for testing
# 
# Lookback_distance <- 2000
# portfolio <- tangency_anwenden(RAW.Data,Lookback_distance) # call the function above with random normal dist. for testing
# 
# matplot(portfolio[[1]], type ="l") # plot the weights of the portfolio
# plot(portfolio[[2]]) # plot the returns of the portfolio
# hist(portfolio[[2]], breaks = 100) # histogram of the returns
