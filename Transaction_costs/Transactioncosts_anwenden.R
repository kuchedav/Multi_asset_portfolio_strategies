### Transactioncosts_anwenden
# this functions adds transaction costs to a portoflio input
# it calculates returns, weights and the dates when to trade

###### Sub function ############
#
# Weights           = the weights of the protfolio
# trade.speed       = the speed with which the portoflio should be traded

Transactioncosts_date_finder <- function(Weights, trade.speed = 0.1){
  
  # go over the weights everytime the overall change in the weights exceeds a certain level
  # the strategy will save the row of the weights and restart from the new date on
  invest_positions <- lapply(1:ncol(Weights), function(j){
    invest_tracker <- numeric()
    save_Weights <- Weights[1,j]
    Weights_column <- Weights[,j]
    
    # search step by step where the next fluctuation is which is bigger than te trade speed
    # save the position (date) and restart
    for(i in 1:nrow(Weights_column)){
      if(abs(as.numeric(Weights_column[i]) - as.numeric(save_Weights)) > trade.speed){
        save_Weights <- Weights_column[i]
        invest_tracker <- c(invest_tracker,i)
      }
    }
    
    # the first day needs to be included as investing day
    c(1,invest_tracker)
  })
  
  # find the dates of the changes
  trade_time <- lapply(1:length(invest_positions), function(i) index(Weights[invest_positions[[i]],]))
}








###### Main function ############
#
# Money_input       = How much money are you investing?
# RAW.Data          = the indeces in which you want to invest
# Weights           = the weights of the protfolio you've calculated
# trade.speed       = the speed with which the portoflio should be traded
#                     after how much change in the weights should the strategy reinvest?
# tradingcosts      = The costs that get payed for every trade

Transactioncosts_anwenden <- function(Money_input = 1000000, RAW.Data, Weights, trade.speed = 0.1, trading.costs = 0.1){
  
  # Calculate the times, when the trades will occur
  trade_time <- Transactioncosts_date_finder(Weights,trade.speed)
  names(trade_time) <- colnames(Weights)
  
  # All days in wich no trade is made will be set to NA
  for(i in 1:ncol(Weights)) Weights[!(index(Weights) %in% trade_time[[i]]),i] <- NA
  
  # the costs rise, because they are growing over time and the last returns don't have any influence from the first costs in the calculations.
  costs_raw <- abs(Weights)*((1+trading.costs)*abs(sign(Weights))*abs(RAW.Data))*Money_input
  # costs_raw <- Weights * RAW.Data[index(Weights),] # scale the costs to the size of the trades (unfinished)
  cost_vector <- cumsum(rowSums(costs_raw,na.rm = T))
  
  # sort the unused indeces out of the raw data
  if(ncol(RAW.Data) > ncol(Weights)){
    colnames(RAW.Data) <- gsub("-", ".", colnames(RAW.Data))
    colnames(Weights) <- gsub("-", ".", colnames(Weights))
    RAW.Data.Cut <- RAW.Data[index(Weights),colnames(RAW.Data) %in% colnames(Weights)]
  }else{
    RAW.Data.Cut <- RAW.Data[index(Weights),]
  }
  # Fill NA's in weights with the last possible value
  weights_over_time <- na.locf(Weights)
  
  
  # Calculations of the portfolio
  returns_raw <-  cumsum(rowSums(weights_over_time * na.fill(RAW.Data.Cut[index(weights_over_time),],0))) - 
                         (cost_vector * trading.costs / Money_input)
  returns_cost <- diff(xts(returns_raw, order.by = index(RAW.Data.Cut)))[-1,]
  
  
  # save the dates when the algorithm trades and how much
  trading_dates <- xts(rowSums(abs(sign(Weights)),na.rm = T),order.by = index(Weights))
  
  # create a list with trading information about when and how much to invest into which index
  trade_info <- list()
  for(i in 1:ncol(Weights)){
    save_list <- data.frame(trade_time[[i]],Money_input*as.numeric(na.exclude(Weights[,i])))
    colnames(save_list) <- c("Trade Date","Inserted money")
    trade_info[[i]] <- save_list
  }
  names(trade_info) <- colnames(Weights)
  
  # create a dataframe
  spent_on_trading <- data.frame(sum(trading_dates), sum(trading_dates)*trading.costs, sum(trading_dates)*trading.costs/Money_input)
  colnames(spent_on_trading) <- c("Amount of trades","Spent money on trades", "Percentage of invested money spent on trades")
  
  
  # Calculate the trading costs over time
  cumsum_costs <- trade_info[[1]][,1]
  for(i in 2:length(trade_info)){
    cumsum_costs <- c(cumsum_costs,trade_info[[i]][,1])
  }
  cost_time_vector <- xts(rep(trading.costs,length(cumsum_costs)),order.by = cumsum_costs)
  rbind(cost_time_vector,xts(0,order.by = index(last(Weights))))
  # plot.xts(cumsum(cost_time_vector))
  
  
  return(list(returns_cost,weights_over_time,trade_info,spent_on_trading,cost_time_vector))
}



### Example Input Data ####################.

### Load Data
# setwd("/Users/davidkuchelmeister/Studium/Bachelor Arbeit/Shiny_App/")
# 
# Chosen_Indeces <- c("FTSE100_returns_1984-05-08" ,"Nikkei225_returns_1990-09-27" ,"SP500_returns_1982-04-23"
#                     ,"US_2Y_Note_returns_1990-06-26"
#                     ,"US_30Y_returns_1977-08-24"
#                     ,"AUD_returns_1987-01-15"
#                     ,"CHF_returns_1975-02-18"
#                     ,"GBP_returns_1975-02-18"
#                     ,"Copper_returns_1959-07-07"
#                     ,"Cotton_returns_1972-08-24"
#                     ,"Brent_Crude_returns_1993-03-19"
#                     ,"Heating_Oil_returns_bereinigt_1979-03-08"
#                     ,"Natural_Gas_returns_1990-04-05"
#                     ,"Gold_returns_1975-01-03"
#                     ,"Platinum_returns_1969-01-06"
#                     ,"Long_Gilt_returns_1990-08-21"
#                     ,"US_5Y_Note_returns_1988-05-24"
#                     ,"US_10Y_returns_1982-05-05")
# 
# 
# source("Portfolio/Load_Data.R")
# source("Portfolio/functions.R")
# 
# Data.Path <- file.path(getwd(),"data_chosen")
# RAW.Total <- RAW_Data <- RAW.Data <- Load_Data(Data.names = as.character(Chosen_Indeces))
# 
# 
# 
#
### calcualte demo portfolio
# 
# lookback <- 2000
# source("Portfolio/Strategies/efficient_anwenden.R")
# portfolio <- efficient_anwenden(RAW.Data,lookback) # call the function above with random normal dist. for testing
# 
# ## Trading cost calculations
# Weights <- portfolio[[1]]
# trade.speed = 0.01
# trading.costs = 1
# Money_input = 1000000
# 
# 
# ### Example Calculations
# A <- Transactioncosts_anwenden(Money_input = 100000,RAW.Data = RAW_Data,Weights = portfolio[[1]],trade.speed = 0.000001,trading.costs = 0)
# 
# # retuns comparison
# plot.xts(exp(cumsum(portfolio[[2]])))¶
# lines(exp(cumsum(A[[1]])),col=2)
# 
# # Analysis plot
# ts.plot(cumsum(t_costs[[1]]))
# matplot(t_costs[[2]], type = "l")
# length(t_costs[[3]]) # ... amount of trades
