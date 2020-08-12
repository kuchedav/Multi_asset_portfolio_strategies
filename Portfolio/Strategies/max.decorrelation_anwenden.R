### max_decorrelation_anwenden function calculates the maximum decorrelation Portfolio

# Input
# RAW.Data = xts Matrix of returns of indices 
# Lookback_distance = the amount of days that will be calculated, from today into the past.
# lag_days = amount of days the algorythm waits until it buys the indeces (1 is standart, to wait until tomorrow)

# Output
# List of two Matrixes, 
# first contains a xts matrix with all weights over the calculated period of time per index
# second list contains an xts vector of the portfolio returns

max.decorrelation_anwenden <- function(RAW.Data,Lookback_distance = 100, lag_days = 1){
  
  require(timeDate)
  require(timeSeries)
  
  
  ### Calculate the Max decorrelations weights
  max_decorrelation <- function(Data){
    max.decorr<-function(weight, correl){
      weight <- weight / sum(weight)
      obj<- 1- (t(weight) %*% correl %*% weight)
      return(-obj)
    }
    
    out <- optim(par     = rep(1 / ncol(Data), ncol(Data)),  # initial guess
                 fn      = max.decorr,
                 correl  = cor(Data),
                 method  = "L-BFGS-B",
                 lower   = 0,
                 upper   = 1)
    
    correlation <- -max.decorr(weight = out$par,correl = cor(Data))
    weights <- out$par/sum(out$par)
    return(list(weights = weights,correlation = correlation))
  }
  
  N <- Lookback_distance # max = 4000
  
  eff.port.list5 <- list()
  
  ### Multi Core
  require(parallel)
  require(doMC)
  registerDoMC(cores=detectCores())
  
  max_decorrelation_function <- function(RAW.Data,i){
    ret.cc5 <- RAW.Data[1:(nrow(RAW.Data)-i),] # Cut the Dataseries
    effi.port <- max_decorrelation(ret.cc5)
    eff.port.list5[[i]] <- effi.port$weights
  }
  
  eff.port.list5 <- foreach(n = 1:N) %dopar% max_decorrelation_function(RAW.Data,n)
  
  weights5 <- sapply(N:1, function(i) eff.port.list5[[i]])
  rownames(weights5) <- colnames(RAW.Data)
  weights_xts <- xts(t(weights5),order.by = index(RAW.Data[nrow(RAW.Data):(nrow(RAW.Data)-(N-1)),]) )
  
  
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
# portfolio <- max.decorrelation_anwenden(data,lookback) # call the function above with random normal dist. for testing
# 
# Lookback_distance <- 2000
# portfolio <- max.decorrelation_anwenden(RAW.Data,Lookback_distance) # call the function above with random normal dist. for testing
# 
# matplot(portfolio[[1]], type ="l") # plot the weights of the portfolio
# plot(portfolio[[2]]) # plot the returns of the portfolio
# hist(portfolio[[2]], breaks = 100) # histogram of the returns
