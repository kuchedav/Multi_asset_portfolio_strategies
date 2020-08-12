### euqal_weights function calculates the equal Weights Portfolio

# Input
# RAW.Data = xts Matrix of returns of indices 
# Lookback_distance = the amount of days that will be calculated, from today into the past.
# lag_days = amount of days the algorythm waits until it buys the indeces (1 is standart, to wait until tomorrow)

# Output
# List of two Matrixes, 
# first contains a xts matrix with all weights over the calculated period of time per index
# second list contains an xts vector of the portfolio returns

equal.weights_anwenden <- function(RAW.Data, Lookback_distance = 100, lag_days = 1){
  
  N <- Lookback_distance # max = 4000
  
  #### Create equal weights Portfolio ###
  dim <- ncol(RAW.Data)
  weights4 <- matrix(rep(1/dim,dim*N),ncol = N)
  rownames(weights4) <- colnames(RAW.Data)
  weights_xts <- xts(t(weights4),order.by = index(RAW.Data[nrow(RAW.Data):(nrow(RAW.Data)-(N-1)),]) )
  
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
# portfolio1 <- equal.weights_anwenden(data,lookback) # call the function above with random normal dist. for testing
# 
# lookback <- 100
# portfolio <- equal.weights_anwenden(RAW.Data,lookback) # call the function above with random normal dist. for testing
# 
# matplot(portfolio[[1]], type ="l") # plot the weights of the portfolio
# plot(portfolio[[2]]) # plot the returns of the portfolio
# hist(portfolio[[2]], breaks = 100) # histogram of the returns
