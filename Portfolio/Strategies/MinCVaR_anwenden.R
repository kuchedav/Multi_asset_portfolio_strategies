### MinCVar_anwenden function calculates the minimum ES/ Cvar Portfolio

# Input
# RAW.Data = xts Matrix of returns of indices 
# Lookback_distance = the amount of days that will be calculated, from today into the past.

# Output
# List of two Matrixes, 
# first contains a xts matrix with all weights over the calculated period of time per index
# second list contains an xts vector of the portfolio returns
 

MinCVaR_anwenden <- function(RAW.Data, Lookback_distance = 100, lag_days = 1){
  
  ### Multi core
  require(timeSeries)
  require(fPortfolio)
  require(foreach)
  require(xts)
  require(zoo)
  require(parallel)
  require(doMC)
  require(Rglpk)
  registerDoMC(coers=detectCores())
  
  RAW.Data <- RAW.Data[-(length(RAW.Data[,1])-(0:6)),]
  
  RAW.Data.org <- RAW.Data # save xts format for date , see line 46
  RAW.Data <- as.timeSeries(RAW.Data)
  
  N <- Lookback_distance 
  
# Set Specifications for the Portfolio requiderd by fPortfolio
  spec <- portfolioSpec(
    model = list(
      type = "CVaR", optimize = "minRisk",
      estimator = "covEstimator", tailRisk = list(),
      params = list(alpha = 0.1, a = 1)),
    
    
  )
  constraints <- c("LongOnly")
  setSolver(spec) <- portfolioSpec
  setType(spec) <- "CVaR"
  setOptimize(spec) <- "minRisk"
  setNFrontierPoints(spec) <- 50
  setSolver(spec)  <- "solveRglpk.CVAR"
  setParams(spec) <- list(alpha=0.95)
  
  # asset Names are set 
  assetsNames <- colnames(RAW.Data)
  
# Create the minimum CVaR portfolio
  cvar.pf.w.list <- list()
  cvar.pf.returns <- c(1:N)

  # function for weights calculation
  cvar_function <- function(RAW.Data, i){
    ret.cc1 <- RAW.Data[1:((length(RAW.Data[,1])-Lookback_distance)+i),]
    cvar.eff.pf <- efficientPortfolio(ret.cc1, spec = spec, constraints = constraints )
    cvar.pf.w.list[[i]] <- fPortfolio::getWeights(cvar.eff.pf)  
}
  # multicore function call
  cvar.pf.w.list <- foreach(n = 1:N) %dopar% cvar_function(RAW.Data,n)
  
  # function for return calculation
  cvarReturns_function <- function(RAW.Data, i){
    cvar.pf.returns[i] <- sum(cvar.pf.w.list[[i]]*RAW.Data[(length(RAW.Data[,1])-(N-i)),])
  }
  # multicore function call  
  cvar.pf.returns <- foreach(n = 1:N) %dopar% cvarReturns_function(RAW.Data, n)
  
  
  
  # dates are saved for xts transformations
  date <- index(RAW.Data.org)
  
  # returns are transformed into xts
  return <- as.xts(as.numeric(cvar.pf.returns), order.by = as.Date(date[((length(date)+1-Lookback_distance):length(date))]))
  
  # weights are transformed into xts
  weights <- matrix(NA, ncol=ncol(RAW.Data), nrow = N)
  for(i in 1:N){
  weights[i,] <- as.matrix(cvar.pf.w.list[[i]])
  }
  
  weights_xts <- as.xts(weights, order.by = as.Date(date[((length(date)+1-Lookback_distance):length(date))]))
  colnames(weights_xts) <- colnames(RAW.Data.org) # to put index names in the right place

# returns are given out
return(list(weights=weights_xts,returns=return))


}


### Example
# length <- 20000
# cols <- 20
# lookback <- 10
# 
# data <- as.xts(matrix(rnorm(length),ncol=cols), order.by = as.Date(1:(length/cols))) # Generate normal dist. data
# colnames(data) <- c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20") # puts Index names from 1 - 20
# matplot(data, type = "l")
# 
# 
# # Load all librarys
# library(fPortfolio)
# 
# portfolio <- MinCVaR_anwenden(data,lookback) # call the function above with random normal dist. for testing
# 
# matplot(portfolio[[1]], type ="l") # plot the weights of the portfolio
# plot(portfolio[[2]]) # plot the returns of the portfolio
# hist(portfolio[[2]], breaks = 100) # histogram of the returns


