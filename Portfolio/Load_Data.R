### Load_Data loads the Data from CSV Files and transforming it into a Data.frame form

# Data.names    = Character String, which indeces should be included
# Data.Path     = Folder where to find the .csv files of the raw data
# zugeschnitten = the indeces have different length, should it be cut down to the shortest?

Load_Data <- function(Data.names,cut = T,gaussian = F,gauss.mean = 0,gauss.sd = 1){
  
  require(xts)
  
  # If gaussian is TRUE, the function will return normalized returns
  if(gaussian == T){
    source("Portfolio/functions.R")
    # set.seed(2)
    
    N <- 10000
    # generate the returns
    gauss_data <- rnorm_xts(mean = gauss.mean, sd = gauss.sd, from = as.Date(Sys.Date())-N)
    # merge them in the correct format
    for(i in 1:17) gauss_data <- merge(gauss_data,rnorm_xts(mean = gauss.mean, sd = gauss.sd, from = as.Date(Sys.Date())-N))
    colnames(gauss_data) <- Data.names
    gauss_data <- gauss_data[1:6015,1:18]
    Shortes_Indeces <- read.csv("data_chosen/AUD_returns_1987-01-15.csv")
    gauss_data <- xts(gauss_data,order.by = as.Date(Shortes_Indeces[,1])[1:6015])
    
    return(gauss_data)
  }
  
  # Save the current working directory (we want to set it back at the end)
  wd <- getwd()
  # we set the datapath to the .csv files where all the raw data are saved
  Data.Path <- file.path(getwd(),"data_chosen")
  # set the new working directory
  setwd(Data.Path)
  
  # Create a List to save all the chosen indeces in 
  Data <- list()
  
  # chosen indeces are read and saved in the list
  # the format gets chagned to xts
  # the list gets reduced to a xts matrix by merging (merge.xts) the list slots together
  Dat <- Reduce(merge.xts, lapply(Data.names, function(name){
    tmp <- read.csv(paste(name,".csv",sep = ""),header = T)
    xts(tmp$Settle, order.by = as.Date(tmp$Date))
  }
  ))
  
  # set the wroking directory back
  setwd(wd)
  
  # name the xts.matrix with the correct names
  colnames(Dat) <- Data.names
  
  # Missing data is filled with the same return as the day before
  Dat <- na.locf(Dat)
  
  # if the vector is "cut", the matrix will be shortened to the shortest timeseries
  if(cut==T){
    return(na.exclude(Dat))
  }else{
    return(Dat)
  }
}




## the reutns of the Load_Data functions is a xts.matrix
## columns are the indeces, rows


# ############ TEST ENVIRONEMENT ##################
# 
############ Speed load the RAW Data for testing ##################
## 
##  nothing is needed in advance to downlaod the raw data
##
####### Download RAW data #######
### set you own working directory to where the shiny app is saved on your computer
# setwd("/Users/davidkuchelmeister/Studium/Bachelor Arbeit/Shiny_App/")
# 
# Data.names <- Chosen_Indeces <- c("FTSE100_returns_1984-05-08" ,"Nikkei225_returns_1990-09-27" ,"SP500_returns_1982-04-23"
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
# RAW_Data <- RAW.Data <- Load_Data(Data.names = as.character(Chosen_Indeces))
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
# 
#
##### Plot the Data ######.
#
# # plot the raw data
# matplot(Data,type="l")
# # Plot the cumulated log returns
# matplot(cumsum(Data),type="l")
