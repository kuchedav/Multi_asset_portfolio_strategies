### global_min_anwenden function calculates the global Minimum Portfolio

# Input
# RAW.Data = xts Matrix of returns of indices 
# Lookback_distance = the amount of days that will be calculated, from today into the past.
# lag_days = amount of days the algorythm waits until it buys the indeces (1 is standart, to wait until tomorrow)

# Output
# List of two Matrixes, 
# first contains a xts matrix with all weights over the calculated period of time per index
# second list contains an xts vector of the portfolio returns

min.vola_anwenden <- function(RAW.Data, Lookback_distance = 100, lag_days = 1){
  
  N <- Lookback_distance # max = 4000
  
  #### Create global min Portfolio ###
  eff.port.list3 <- list()
  
  ### Multi Core
  require(parallel)
  require(doMC)
  registerDoMC(cores=detectCores())
  
  global_min_function <- function(RAW.Data,i){
    ret.cc3 <- RAW.Data[1:(nrow(RAW.Data)-i),]
    mu.hat.annual = apply(ret.cc3, 2, mean)
    cov.mat.annual = cov(ret.cc3)
    effi.port = globalMin.portfolio(er = mu.hat.annual,cov.mat =  cov.mat.annual)
    eff.port.list3[[i]] <- effi.port
  }
  
  eff.port.list3 <- foreach(n = 1:N) %dopar% global_min_function(RAW.Data,n)
  
  weights3 <- sapply(N:1, function(i) eff.port.list3[[i]]$weights)
  weights_xts <- xts(t(weights3),order.by = index(RAW.Data[nrow(RAW.Data):(nrow(RAW.Data)-(N-1)),]) )
  
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
# portfolio <- min.vola_anwenden(data,lookback) # call the function above with random normal dist. for testing
# 
# Lookback_distance <- 2000
# portfolio <- min.vola_anwenden(RAW.Data,Lookback_distance) # call the function above with random normal dist. for testing
# 
# matplot(portfolio[[1]], type ="l") # plot the weights of the portfolio
# plot(portfolio[[2]]) # plot the returns of the portfolio
# hist(portfolio[[2]], breaks = 100) # histogram of the returns
