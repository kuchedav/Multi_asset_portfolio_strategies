
## create universal functions and source code

# Author: David Kuchelmeister
###############################################################################



working_dir = getwd()

data.path <- paste(working_dir,"data_strategies",sep = "/")
raw.data.path <- file.path(working_dir,"data_chosen",sep = "/")

require(PerformanceAnalytics)
require(corrplot)
require(xtable)
require(futile.logger)
require(reader)
require(stockPortfolio)
require(timeSeries)
require(FRAPO)

# Used for the Parallel computing of the portfolios
require(parallel)
require(doMC)



# Sourcing other functions
source("Portfolio/calculate_portfolios.R")
source("Portfolio/Load_Data.R")
source("Target_vola/RiskParity_anwenden.R")
source("Transaction_costs/Transactioncosts_anwenden.R")
source("Portfolio/functions.R")

# If the Raw Data changes remember to change the "chosenRawData" in the UI file aswell
RAW_Data_input_names <- c("FTSE100_returns_1984-05-08"
                          ,"Nikkei225_returns_1990-09-27"
                          ,"SP500_returns_1982-04-23"
                          ,"US_2Y_Note_returns_1990-06-26"
                          ,"US_30Y_returns_1977-08-24"
                          ,"AUD_returns_1987-01-15"
                          ,"CHF_returns_1975-02-18"
                          ,"GBP_returns_1975-02-18"
                          ,"Copper_returns_1959-07-07"
                          ,"Cotton_returns_1972-08-24"
                          ,"Brent_Crude_returns_1993-03-19"
                          ,"Heating_Oil_returns_bereinigt_1979-03-08"
                          ,"Natural_Gas_returns_1990-04-05"
                          ,"Gold_returns_1975-01-03"
                          ,"Platinum_returns_1969-01-06"
                          ,"Long_Gilt_returns_1990-08-21"
                          ,"US_5Y_Note_returns_1988-05-24"
                          ,"US_10Y_returns_1982-05-05") 








# function to easily calculate the portfolio returns if the weights and raw data is given
CalculatePortfolioReturns <- function(RAW.Data, Weights, lag_days = 1){
  if(ncol(RAW.Data) > ncol(Weights)){
    colnames(RAW.Data) <- gsub("-", ".", colnames(RAW.Data))
    colnames(Weights) <- gsub("-", ".", colnames(Weights))
    RAW.Data.Cut <- RAW.Data[index(Weights),colnames(RAW.Data) %in% colnames(Weights)]
  }else{
    RAW.Data.Cut <- RAW.Data[index(Weights),]
  }
  
  return1 <-  rowSums(Weights * na.fill(lag.xts(RAW.Data.Cut[index(Weights),],(-lag_days)+1),0))
  return <- xts(return1, order.by = index(Weights))
  return(return)
}
# returns the calculated returns of the portfolio



# Is needed to actualise the loading bar
compute_data <- function(updateProgress = NULL) {
  
  # Create 0-row data frame which will be used to store data
  dat <- data.frame(x = numeric(0), y = numeric(0))
  
  for (i in 1:10) {
    # Compute new row of data
    
    new_row <- data.frame(x = rnorm(1), y = rnorm(1))
    # If we were passed a progress update function, call it
    
    if (is.function(updateProgress)) {
      text <- paste0("x:", round(new_row$x, 2), " y:", round(new_row$y, 2))
      updateProgress(detail = text)
      
    }
    # Add the new row of data
    dat <- rbind(dat, new_row)
    Sys.sleep(0.15)
  }
  dat
}





### rnorm_xts is used to generate gaussian distributed xts values
# mean  = mean of the gaussian distribution
# sd    = standart deviation of the gaussian distribution
# from  = Date format, values are generated form this date on
# until = Date format, values are generated until this date
rnorm_xts <- function(mean=0, sd=1, from="2015-01-01", until=as.Date(Sys.Date())) {
  rnorm.xts <- numeric()
  dates <- seq(from=as.Date(from), to=until, by="d")
  randoms <- rnorm(length(dates), mean=mean, sd=sd)
  rnorm.xts <- xts(randoms, order.by=dates)
  # rnorm.xts[which(cumsum(rnorm.xts) < 0)] <- 0
  return(rnorm.xts)
}
### rnorm_xts returns gaussian distributed xts values
#### Test environement ####.
# par(mfrow = c(3,3))
# for(i in 1:9){
# normalised <- rnorm_xts(mean = 0,sd = 1,from = "1990-01-01")
# ts.plot(cumsum(normalised))
# }
# 
# ts.plot(rollapply(normalised,252,sd))
# abline(h=sd(normalised))




### max.decorr calcualtes the decorrelations of a portfolio
# weight  = xts.matrix with the weights of the portfolio
# correl  = correlation of the returns of the indices
max.decorr<-function(weight, correl){
  weight <- weight / sum(weight)
  obj<- 1- (t(weight) %*% correl %*% weight)
  return(-obj)
}
### max.decorr returns the portfolio correlation
#### Test environement ####.
## to calculate portfolio weights and returns use the calcualte_portfolios function
# max.decorr(weights, cor(returns))