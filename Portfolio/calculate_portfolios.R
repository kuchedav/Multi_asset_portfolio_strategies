### the calcualte_portfolios function runs all the code to recalculate the portfolios

# backtest.length   = amount of days to look back
# plot_returns      = Do you want to plot the portfolios? (only to controll the Data)
# Chosen_Indeces    = Character String, which Indices should be used
# Chosen_Portfolios = Character String, which Potfolios should be used

calcualte_portfolios <- function(backtest.length = 4000,
                                 Chosen_Indeces,
                                 Chosen_Portfolios,
                                 gaussian.input = F,
                                 gaussian.mean = 0,
                                 gaussian.sd = 1,
                                 lag_days = 1
){
  
  # source the loading function and the used functions used in the portfolio calculations
  source("Portfolio/Load_Data.R")
  source("Portfolio/functions.R")
  
  #### get the Date to calculate the Portfolio ####
  # Load the chosen raw data from the csv files and convert the into a xts.matrix
  # cut stays always TRUE, otherwise the data will differ in length
  RAW_Data <- Load_Data(Data.names = as.character(Chosen_Indeces),gaussian = gaussian.input,
                        gauss.mean = gaussian.mean, gauss.sd = gaussian.sd/sqrt(252))
  
  
  #### Calculate The portfolio with the Data ####
  # The portfolios are calculated under the condition that their name exists in the Chosen_Portfolios Vector
  # all the portfolios are saved in the data_strategies folder
  # the returns and the weights are saved in to differnt files
  
  # needed Input in the for loop:
  # RAW_Data
  # Chosen_Portfolios
  # backtest.length
  
  for(i in 1:length(Chosen_Portfolios)){
    
    # source the code
    file <- paste(Chosen_Portfolios[i],"_anwenden.R",sep = "")
    source(paste("Portfolio/Strategies/",file,sep=""))
    
    # run the function
    strategie <- eval(parse(text = paste(Chosen_Portfolios[i],"_anwenden",sep="")))(RAW_Data,backtest.length,lag_days)
    # strategie[[2]] <- lag.xts(strategie[[2]],lag_days-1)
    print(paste(Chosen_Portfolios[i],"portfolio calculated"))
    
    # write the results in the folder data_strategies
    write.csv(x = strategie$returns,file = paste("data_strategies/",Chosen_Portfolios[i],".csv",sep = ""),
              row.names = index(strategie$returns))
    write.csv(x = strategie$weights,file = paste("data_strategies/",Chosen_Portfolios[i],"_weights.csv",sep = ""),
              row.names = index(strategie$weights))
  }
  
  return("Calculations finished")
}


## the calcualte_portfolios function doesn't return anything
## beacause all the results are saved in .csv files in a filder





############ TEST ENVIRONEMENT ##################




############ Speed load the RAW Data for testing ##################
## 
##  nothing is needed in advance to downlaod the raw data
##
####### Download RAW data #######
### set you own working directory to where the shiny app is saved on your computer
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
# backtest.length = 300
# 
# # load and save which portfolios are available in the folder
# 
# strategies.Path <- file.path(getwd(),"Portfolio/Strategies/")
# split_text <- strsplit(dir(strategies.Path),"_")
# Chosen_Strategie <- Chosen_Portfolios <- sapply(1:length(dir(strategies.Path)) , function(i) split_text[[i]][1])
# 
# strategies.Path <- file.path(getwd(),"Portfolio/Strategies/")
# dir(strategies.Path)
# lag_days <- 1
# 
# data <- RAW.gauss <- Load_Data(Data.names = as.character(Chosen_Indeces),T,0,1)


############ Plot the calculated portfolios ##################
### run only the sections with this comment at the end of the line:
## "to test application only run this line of code"
##
############ Plot the calculated portfolios ##################
## 
## to be able to plot the portfolios, they need to be calculated with the code from above
##
####### Plot the Portfolios #######
# mat <- merge.xts(efficient$returns,equal$returns,global.min$returns,tangency$returns,max.decorrelation$returns)
# mat_cumsum <- merge.xts(cumsum(efficient$returns),cumsum(equal$returns),
#                         cumsum(global.min$returns),cumsum(tangency$returns),cumsum(max.decorrelation$returns))
# 
# matplot(mat_cumsum,type="l",lty =  1)
# abline(h = 0)
# legend("bottomleft",
#        lty=c(1,1),
#        lwd=1,col=1:ncol(mat_cumsum),
#        legend = colnames(mat_cumsum),
#        cex = 0.8
# )



