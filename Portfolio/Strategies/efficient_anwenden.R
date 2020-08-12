### efficient_anwenden function calculates the Efficient Portfolio

# Input
# RAW.Data = xts Matrix of returns of indices
# Lookback_distance = the amount of days that will be calculated, from today into the past.
# lag_days = amount of days the algorythm waits until it buys the indeces (1 is standart, to wait until tomorrow)
# 
# Output
# List of two Matrixes,
# first contains a xts matrix with all weights over the calculated period of time per index
# second list contains an xts vector of the portfolio returns

efficient_anwenden <- function(RAW.Data, Lookback_distance = 100,lag_days = 1){
  
  N <- Lookback_distance
  
  #### Create efficient Portfolio ###
  eff.port.list1 <- list()
  
  ### Multi Core
  require(parallel)
  require(doMC)
  registerDoMC(cores=detectCores())
  
  efficient_function <- function(RAW.Data,i){
    ret.cc1 <- RAW.Data[1:(nrow(RAW.Data)-i),] # Cut the Dataseries
    mu.hat.annual = apply(ret.cc1, 2, mean)*250
    cov.mat.annual = cov(ret.cc1)*250
    effi.port = efficient.portfolio(er = mu.hat.annual,cov.mat =  cov.mat.annual,target.return =  0.1)
    eff.port.list1[[i]] <- effi.port
  }
  
  eff.port.list1 <- foreach(n = 1:N) %dopar% efficient_function(RAW.Data,n)
  
  weights1 <- sapply(N:1, function(i) eff.port.list1[[i]]$weights)
  weights_xts <- xts(t(weights1),order.by = index(last(RAW.Data,ncol(weights1))))
  
  # Calculate the Portfolio returns with the weights and raw data
  return <- CalculatePortfolioReturns(RAW.Data,weights_xts,lag_days)
  
  return(list(weights=weights_xts,returns=return))
}

### Test of the portfolio return calculation
# te <- lag.xts(RAW.Data[index(weights_xts),],0)
# PoR <- Return.portfolio(te, weights = weights_xts)
# plot.xts(exp(cumsum(PoR)))
# 
# ts.plot(exp(cumsum(return)))


### Example
# library(xts)
# setwd("Studium/Bachelor Arbeit/Shiny_App/") # Path to shiny app
# source("Portfolio/functions.R")
# length <- 200000
# cols <- 20
# norm_matrix <- matrix(rnorm(length,mean = 1,sd = 10),ncol=cols)
# data <- as.xts(norm_matrix, order.by = Sys.Date()-(1:nrow(norm_matrix))) # Generate normal dist. data
# colnames(data) <- LETTERS[1:ncol(norm_matrix)] # puts Index names from 1 - 20
# matplot(data, type = "l")
# 
# lookback <- 5000
# portfolio <- efficient_anwenden(data,lookback) # call the function above with random normal dist. for testing
# 
# Lookback_distance <- 5000
# portfolio <- efficient_anwenden(RAW.Data,Lookback_distance) # call the function above with random normal dist. for testing
# 
# matplot(portfolio[[1]], type ="l") # plot the weights of the portfolio
# plot(portfolio[[2]]) # plot the returns of the portfolio
# hist(portfolio[[2]], breaks = 100) # histogram of the returns
# 
# 
