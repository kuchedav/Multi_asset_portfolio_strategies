### RiskParity_anwenden


# F?r test
# Weights           = the weights of the protfolio
# PortfolioReturns  = the returns of the portfolio
# TargetVola        = the targeted volatility
# Lookback          = amount of days to look back
# LowerBoundStdev   = set lower bound for the the stadartdeviation
# UpperBoundWeight  = Upper bound of the weights
# LowerBoundWeight  = Lower bound of the weights

RiskParity_anwenden <- function(Weights, PortfolioReturns, TargetVola = 0.3, Lookback = 1000, 
                                LowerBoundStdev=0.01, UpperBoundWeight=0, LowerBoundWeight=0){
  
  require(timeSeries)
  
  len <- length(Weights[,1])
  index <- index(Weights[Lookback:length(Weights[,1])])
  
  Weights <- as.timeSeries(Weights)
  PortfolioReturns <- as.timeSeries(PortfolioReturns)
  
  # the vola is calculated here , with the lenghs of the lookback of the portfolio
  portfolioVola <- rollapplyr(PortfolioReturns, Lookback, sd)*sqrt(250)
  portfolioVola <- pmax(portfolioVola, LowerBoundStdev)
  
  # calculating the weights
  ReWeights <- TargetVola / portfolioVola
  WeightsAfterRiskParity <-  Weights[Lookback:len,] * matrix(rep(ReWeights,ncol(Weights)),ncol = ncol(Weights))  

  ## for separate controlling in the function  
  # matplot(Weights,type ="l")
  # plot(reweights, type = "l")
  # matplot(WeightsAfterRiskParity, type = "l")
  
  
  #  sets lower limit for the weight / gets ignored if bound is set to 0 
  if(LowerBoundWeight != 0){
    WeightsAfterRiskParity[WeightsAfterRiskParity < LowerBoundWeight] <- 0
  }
  #  sets upper limit for the weight / gets ignored if bound is set to 0 
  if(UpperBoundWeight != 0){
    WeightsAfterRiskParity[WeightsAfterRiskParity > UpperBoundWeight] <- UpperBoundWeight
  }
  ## for separate controlling in the function
  # matplot(WeightsAfterRiskParity, type ="l")
  
  # transformation into xts
  WeightsAfterRiskParity_xts <- as.xts(as.matrix(WeightsAfterRiskParity), order.by = as.Date(index))
  
  # WeightsAfterRiskParity_after_normalising <- WeightsAfterRiskParity_xts/rowSums(WeightsAfterRiskParity_xts)
  
  return(WeightsAfterRiskParity_xts)
}



### Example 
# 
# length <- 200000
# cols <- 20
# lookback <- 1000 # takes about 10 secends for each day ( 100 days -> ~15 min)
# 
# data <- as.xts(matrix(rnorm(length),ncol=cols), order.by = as.Date(1:(length/cols))) # Generate normal dist. data
# colnames(data) <- c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20") # puts Index names from 1 - 20
# matplot(data, type = "l")
# 
# # is neeeded for calculations
# # function to easily calculate the portfolio returns if the weights and raw data is given
# CalculatePortfolioReturns <- function(RAW.Data, Weights, lag_days = 1){
#   if(ncol(RAW.Data) > ncol(Weights)){
#     colnames(RAW.Data) <- gsub("-", ".", colnames(RAW.Data))
#     colnames(Weights) <- gsub("-", ".", colnames(Weights))
#     RAW.Data.Cut <- RAW.Data[index(Weights),colnames(RAW.Data) %in% colnames(Weights)]
#   }else{
#     RAW.Data.Cut <- RAW.Data[index(Weights),]
#   }
#   
#   return1 <-  rowSums(Weights * na.fill(lag.xts(RAW.Data.Cut[index(Weights),],(-lag_days)+1),0))
#   return <- xts(return1, order.by = index(Weights))
#   return(return)
# }
# # returns the calculated returns of the portfolio
# 
# 
# # need of the Efficiency function
# # set the way to the shiny app
# setwd("Studium/Bachelor Arbeit/Shiny_App/")
# 
# source("Portfolio/functions.R")
# source("Portfolio/Strategies/efficient_anwenden.R")
# eff.pf <- efficient_anwenden(data,5000)
# # in case raw indices data is available
# # eff.pf <- efficient_anwenden(RAW.Total,5000)
# w <- eff.pf[[1]]
# r <- eff.pf[[2]]
# matplot(w, type = "l")
# plot(r)
# 
# 
# rp.anwenden <-RiskParity_anwenden(Weights = w, PortfolioReturns = r, TargetVola = 0.2, Lookback = 200)
# targetvola_weights <- WeightsAfterRiskParity <- rp.anwenden
# 
# 
# returnsAfterRP <- WeightsAfterRiskParity * as.matrix(data[(length(data[,1])+1-length(WeightsAfterRiskParity[,1])):length(data[,1]),])
# matplot(data, type = "l")
# plot(rowSums(returnsAfterRP),type="l")
# sd(rowSums(returnsAfterRP))
# plot(rollapplyr(rowSums(returnsAfterRP), 10, sd)*sqrt(250),type = "l")
# mean(rollapplyr(rowSums(returnsAfterRP), 10, sd)*sqrt(250),type = "l")
# 
# colSums(rp.anwenden)

