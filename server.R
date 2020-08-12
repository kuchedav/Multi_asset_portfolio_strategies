################################################################################
################################################################################
# Here we have the server file
# we do calculations only here
################################################################################
################################################################################

# Author: David Kuchelmeister
###############################################################################



# session has information on the user as well
shinyServer(function(input, output, session){
  
  # read the input of the start and end date
  start_date_fct <- reactive({ input$start_date })
  end_date_fct <- reactive({ input$end_date })
  
  ############## RETURN/WEIGHTS ##############  RETURN/WEIGHTS  ############## RETURN/WEIGHTS
  {
    ## xts.data.frame of ALL the returns of the portfolios
    return.mat <- reactive({
      input$goButton
      
      Saved_Strategies <- Saved_Strategies()
      
      read_mat <- lapply(1:length(Saved_Strategies),
                         function(i) read.csv(paste("data_strategies/",Saved_Strategies[i],".csv",sep = "")))
      
      xts_mat <- lapply(1:length(Saved_Strategies),
                        function(i) xts(read_mat[[i]][,2],order.by = as.Date(read_mat[[i]][,1])))
      
      return.mat <- Reduce(merge.xts,xts_mat)
      colnames(return.mat) <- Saved_Strategies
      
      return.mat
    })
    
    ###### Returns ####################.
    {
      
      # The returns need to be read multiple time, because it is not possible to
      #   display the same "portfolio choice button" multiple times.
      
      ###### Shiny application returns ####################.
      ## xts.return only of the chosen portfolio
      returns_fct <- reactive({
        if(input$include_tradingcosts){
          returns_fct <- portfolio_transactioncost()[[1]]
        }else{
          # if the target vola section is activated, the returns will be taken after the target vola calculations
          if(input$include_targetvola){
            returns_fct <- targetvola_returns()
          }else{
            if(is.null(input$path_pf_returns)){
              # if normalized Datainput is on, Input will be generated
              if(input$NormalData){
                returns_fct <- rnorm_xts(mean = input$MeanNormalData ,sd = input$VarNormalData/sqrt(252),
                                         from=start_date_fct(),until=end_date_fct())
              }else{
                return.matrix <- return.mat()
                Chosen_Strategie <- input$chosenStrategy
                returns_long <- return.matrix[,Chosen_Strategie]
                returns_fct <- na.exclude(returns_long[paste(start_date_fct(),end_date_fct(),sep = "/")])
              }
            }else{
              # if a path is activated, the portfolio input will be loaded from this folder
              file <- input$path_pf_returns
              returns_loaded <- read.csv(file$datapath)
              ret <- xts(returns_loaded[,2],order.by = as.Date(returns_loaded[,1]))
              returns_fct <- ret[paste(start_date_fct(),end_date_fct(),sep = "/")]
            }
          }
        }
      })
      
      ###### Target vola returns ####################.
      # xts.return specificly for the targetvola function
      # Weights need to be generated seperatly too
      # It is not possible to print the same button twice, therefore we need to build two
      #   inputs in which we can change the input
      targetvola_returns_fct <- reactive({
        if(is.null(input$path_pf_returns)){
          if(input$NormalData){
            targetvola_returns_fct <- rnorm_xts(mean = input$MeanNormalData ,sd = sqrt(input$VarNormalData),
                                                from=start_date_fct(),until=end_date_fct())
          }else{
            return.matrix <- return.mat()
            Chosen_Strategie <- input$set_Vola_chosenStrategy # Input chan be changed with a different button
            returns_long <- return.matrix[,Chosen_Strategie]
            targetvola_returns_fct <- na.exclude(returns_long[paste(start_date_fct(),end_date_fct(),sep = "/")])
          }
        }else{
          file <- input$path_pf_returns
          returns_loaded <- read.csv(file$datapath)
          ret <- xts(returns_loaded[,2],order.by = as.Date(returns_loaded[,1]))
          targetvola_returns_fct <- ret[paste(start_date_fct(),end_date_fct(),sep = "/")]
        }
      })
      ###### Trading costs returns ####################.
      ## xts.return only of the chosen portfolio
      transaction_costs_returns_fct <- reactive({
        if(is.null(input$path_pf_returns)){
          # if normalized Datainput is on, Input will be generated
          if(input$NormalData){
            transaction_costs_returns_fct <- rnorm_xts(mean = input$MeanNormalData ,sd = sqrt(input$VarNormalData),
                                                       from=start_date_fct(),until=end_date_fct())
          }else{
            return.matrix <- return.mat()
            Chosen_Strategie <- input$trading_costs_chosenStrategy
            returns_long <- return.matrix[,Chosen_Strategie]
            transaction_costs_returns_fct <- na.exclude(returns_long[paste(start_date_fct(),end_date_fct(),sep = "/")])
          }
        }else{
          # if a path is activated, the portfolio input will be loaded from this folder
          file <- input$path_pf_returns
          returns_loaded <- read.csv(file$datapath)
          ret <- xts(returns_loaded[,2],order.by = as.Date(returns_loaded[,1]))
          transaction_costs_returns_fct <- ret[paste(start_date_fct(),end_date_fct(),sep = "/")]
        }
      })
      
    }
    
    ###### Weights ####################.
    {
      
      # The same goes with the weights, for each "portfolio choice button" we need to read out the weights seperatly
      
      ###### Shiny application Weights ####################.
      # ts.plot(efficient$weights["1998-01-01/2016-01-01"])
      ## All weights of the chosen portfolio
      weightsfile <- reactive({
        input$goButton
        weightsfile <- read.csv(file.path(data.path,paste(input$chosenStrategy,"_weights.csv",sep = "")))
      })
      weightsfile_fct <- reactive({
        if(input$include_tradingcosts){
          weightsfile_fct <- portfolio_transactioncost()[[2]]
        }else{
          # if the target vola section is activated, the weights will be taken after the target vola calculations
          if(input$include_targetvola){
            weightsfile_fct <- targetvola_weights()
          }else{
            if(is.null(input$path_pf_weights)){
              weightsfile <- weightsfile()
              weightsfile_fct <- xts(weightsfile[,-1],
                                     order.by = as.Date(weightsfile[,1]))[paste(start_date_fct(),end_date_fct(),sep = "/")]
            }else{
              path_pf_weights <- input$path_pf_weights
              weights_loaded <- read.csv(path_pf_weights$datapath)
              wei <- xts(weights_loaded[,-1],order.by = as.Date(weights_loaded[,1]))
              weightsfile_fct <- wei
            }
          }
        }
      })
      ###### Target vola returns ####################.
      # Weights specificly read out for the target vola section
      targetvola_weightsfile <- reactive({
        targetvola_weightsfile <- read.csv(file.path(data.path,paste(input$set_Vola_chosenStrategy,
                                                                     "_weights.csv",sep = "")))
      })
      targetvola_weightsfile_fct <- reactive({
        if(is.null(input$path_pf_weights)){
          targetvola_weightsfile <- targetvola_weightsfile()
          targetvola_weightsfile_fct <- xts(targetvola_weightsfile[,-1],
                                            order.by = as.Date(targetvola_weightsfile[,1]))[paste(start_date_fct(),
                                                                                                  end_date_fct(),sep = "/")]
        }else{
          path_pf_weights <- input$path_pf_weights
          weights_loaded <- read.csv(path_pf_weights$datapath)
          wei <- xts(weights_loaded[,-1],order.by = as.Date(weights_loaded[,1]))
          targetvola_weightsfile_fct <- wei
        }
      })
      ###### Trading Costs returns ####################.
      tradingcosts_weightsfile <- reactive({
        tradingcosts_weightsfile <- read.csv(file.path(data.path,paste(input$trading_costs_chosenStrategy,"_weights.csv",sep = "")))
      })
      tradingcosts_weightsfile_fct <- reactive({
        # if the target vola section is activated, the weights will be taken after the target vola calculations
        if(input$include_targetvola){
          tradingcosts_weightsfile_fct <- targetvola_weights()
        }else{
          if(is.null(input$path_pf_weights)){
            tradingcosts_weightsfile <- tradingcosts_weightsfile()
            tradingcosts_weightsfile_fct <- xts(tradingcosts_weightsfile[,-1],
                                                order.by = as.Date(tradingcosts_weightsfile[,1]))[paste(start_date_fct(),
                                                                                                        end_date_fct(),sep = "/")]
          }else{
            path_pf_weights <- input$path_pf_weights
            weights_loaded <- read.csv(path_pf_weights$datapath)
            wei <- xts(weights_loaded[,-1],order.by = as.Date(weights_loaded[,1]))
            tradingcosts_weightsfile_fct <- wei
          }
        }
      })
      
    }
    
    
    
    
    # Folder recognition (which Portoflios are already calculated and available)
    # We read out the Saved strategies to generically implement the data
    Saved_Strategies <- reactive({
      input$goButton
      strategies.Path <- file.path(getwd(),"data_strategies/")
      strategies.csv <- gsub(".csv","_csv",dir(strategies.Path))
      split_text <- strsplit(strategies.csv,"_")
      Names_Strategie <- sapply(1:length(dir(strategies.Path)) , function(i) split_text[[i]][1])
      Saved_Strategies <- unique(Names_Strategie)
      Saved_Strategies
    })
    
  }
  
  ##############  BENCHMARK     ##############    BENCHMARK     ##############  BENCHMARK
  {
    
    # The benachmark ist seperately read
    #   therefore he is replacable with any other wished time series.
    
    # returns of the Benchmark (standart = equal weights portfolio)
    benchmark.raw <- reactive({
      benchmark.raw <- read.csv(file.path("Benchmark","benchmark.csv"))
      benchmark.raw
    })
    
    benchmark <- reactive({
      if(is.null(input$path_benchmark)){
        returns_fct <- returns_fct()
        benchmark.raw <- benchmark.raw()
        benchmark.uncut <- xts(benchmark.raw[,2], order.by = as.Date(benchmark.raw[,1]))
        colnames(benchmark.uncut) <- "Benchmark"
        benchmark.uncut
        # benchmark <- benchmark.uncut
        # benchmark <- benchmark.uncut[paste(start_date_fct(),end_date_fct(),sep = "/")]
        benchmark <- benchmark.uncut[paste(index(first(returns_fct)), index(last(returns_fct)),sep = "/")]
      }else{
        path_benchmark <- input$path_benchmark
        benchmark_loaded <- read.csv(path_benchmark$datapath)
        ben <- xts(benchmark_loaded[,2],order.by = as.Date(benchmark_loaded[,1]))
        benchmark <- ben[paste(start_date_fct(),end_date_fct(),sep = "/")]
      }
    })
    
  }
  
  ##############  RAW DATA      ##############   RAW DATA       ##############  RAW DATA
  {
    
    # Raw data is needed in different forms, therefore we read the data in different ways
    
    # xts.data.frame of all the raw data used to calculate the portfolio
    RAW.Total <- reactive({ RAW.Total <- Load_Data(cut = T,Data.names = RAW_Data_input_names) })
    # all the Raw Data cut to the length of the portfolios (stratdate, enddate)
    RAW.Cut <- reactive({ RAW.Cut <- RAW.Total()[paste(start_date_fct(),end_date_fct(),sep = "/")] })
    
    # the chosen Data file from the tab (raw Data)
    RAW.data <- reactive({ RAW.data <- read.csv(file.path(raw.data.path,input$chosenRawData )) })
    # All the RAW Data as dataframe in xts
    RAW.data.xts <- reactive({ RAW.data.xts <- xts(RAW.data()$Settle ,order.by =  as.Date(RAW.data()$Date))  })
  }
  
  
  
  ############## Recalculate portfolio ##############    TAB 0      ############## TAB 0
  {
    
    #------------  Recalculate portfolio  -------------
    {
      ### recalculate the portfolios
      
      # a small feedback for the user to know, that the calculations are finished
      output$TextIndeces <- renderText({
        if(input$Recalc_Data){
          "Calculations finished"
          # input$Chosen_Indeces
        }
      })
      
      
      # Recalculating the portfolios
      # obersEvent is neeeded because the function does not give any value back
      #   the function only loads csv files calculated portfolios and saves the results in different csv files
      observeEvent(input$Recalc_Data, {
        
        
        # Creating the loading bar:
        progress <- shiny::Progress$new()
        progress$set(message = "Computing data", value = 0)
        # Close the progress when this reactive exits (even if there's an error)
        on.exit(progress$close())
        # create a loading bar, which shows the progress of the calculations
        updateProgress <- function(value = NULL, detail = NULL) {
          if (is.null(value)) {
            value <- progress$getValue()
            value <- value + (progress$getMax() - value) / 5
          }
          progress$set(value = value, detail = detail)
        }
        compute_data(updateProgress)
        
        
        # actually calculate the new portfolios:
        calcualte.portfolios <- calcualte_portfolios(
          backtest.length = input$lookback_distance,
          Chosen_Indeces = input$Chosen_Indeces,
          Chosen_Portfolios = input$Chosen_Strategie,
          gaussian.input = input$gaussian,
          gaussian.mean = input$gaussian.mean,
          gaussian.sd = input$gaussian.sd,
          lag_days = input$delay_in_days
        )
      })
      
      
      # Chosen_Strategie
      # Create a radio buttons to choose from the available strategies, which should be calculated
      output$Chosen_Strategie <- renderUI({
        strategies.Path <- file.path(getwd(),"Portfolio/Strategies/")
        split_text <- strsplit(dir(strategies.Path),"_")
        cut_text <- sapply(1:length(dir(strategies.Path)) , function(i) split_text[[i]][1])
        
        radioButtons(inputId = "Chosen_Strategie",
                     label = "Please chose which portfolios should be recalculated",
                     choices = cut_text
        )
      })
      
      
      # Stop the application if button is pressed
      observe({
        if(input$stop_app){
          stopApp()
        }
      })
    }
    
    #------------  Target volaility  -------------
    {
      
      #### Buttons #################################.
      
      #### Portfolio Choice
      # Radiobuttons to choose, which portoflio should be overlayed with a target vola strategy
      output$set_Vola_chosenStrategy <- renderUI({
        Saved_Strategies <- Saved_Strategies()
        radioButtons("set_Vola_chosenStrategy",
                     label = h3("Choose strategy"), Saved_Strategies, selected = input$chosenStrategy)
      })
      
      #### length of Loockback
      # Numeric Input for the amount of days to look back
      output$targetvol_lookback <- renderUI({
        returns_before_target_vola <- returns_before_target_vola()
        numericInput("targetvol_lookback","Rolling width of the target volatility", value=100, step =100)
      })
      
      #### Set volatility
      output$targetvol_volatility <- renderUI({
        numericInput("targetvol_volatility","Target volatility in percent", value = 10,step = 1)
      })
      
      
      # In case normalised portfolio input is activated this gives a warning
      # (only when portfolio is replaced with gaussian input, not when indices are replaced with gaussien input)
      output$comment_normalised <- renderUI({
        if(input$NormalData){
          h2("turn off the normalised input to calculate a target volatility", style = "color:red")
        }
      })
      
      
      # These are advanced manipulations
      output$LowerBoundStdev_targetvola <- renderUI({
        numericInput("LowerBoundStdev_targetvola","Set the lower bound of the standard deviation", value = 0.01,step = 0.01)
      })
      output$UpperBoundWeight_targetvola <- renderUI({
        numericInput("UpperBoundWeight_targetvola","Set the upper bound of the weights", value = 0,step = 0.01)
      })
      output$LowerBoundWeight_targetvola <- renderUI({
        numericInput("LowerBoundWeight_targetvola","Set the lower bound of the weights", value = 0,step = 0.01)
      })
      ##############################################
      
      
      
      
      
      #### Load the data ###########################.
      
      # read the returns of the correct portfolio from the returns matrix with all the portfolio returns
      returns_before_target_vola <- reactive({
        returns_before_target_vola <- targetvola_returns_fct()
      })
      
      # load the weights of the correct portoflio
      weights_before_target_vola <- reactive({
        weights_before_target_vola <- targetvola_weightsfile_fct()
      })
      ##############################################
      
      
      
      
      #### Calculate ##############################.
      
      # Apply of the RiskParity function to set a target volatility
      targetvola_weights <- reactive({
        weightsfile_fct <- weights_before_target_vola()
        returns_fct <- returns_before_target_vola()
        targetvol_lookback <- input$targetvol_lookback
        
        targetvola_weights <- RiskParity_anwenden(Weights = weightsfile_fct, PortfolioReturns = returns_fct,
                                                  Lookback = targetvol_lookback,
                                                  TargetVola = (input$targetvol_volatility/100),
                                                  LowerBoundStdev = input$LowerBoundStdev_targetvola,
                                                  UpperBoundWeight = input$UpperBoundWeight_targetvola,
                                                  LowerBoundWeight = input$LowerBoundWeight_targetvola)
        targetvola_weights
      })
      
      # Calculate the returns based on the Weights from the Target vola calculations
      targetvola_returns <- reactive({
        # read in the necessairy data
        targetvola_weights <- targetvola_weights()
        RAW.Total <- RAW.Total()
        
        
        # If the portfolios are calculated this part of the code generates the "random gaussian distribution" (set.seed(2))
        #   We need this, because we only retrieve the weights and need raw data to calcualted the returns.
        #   This is no problem with indeces, because they are saved in our database, but we need to regenerate the random values.
        if(input$gauss_input_data){
          source("Portfolio/functions.R")
          set.seed(2)
          N <- 10000
          # generate the returns
          gauss_data <- rnorm_xts(mean = input$gaussian.mean, sd = input$gaussian.sd/sqrt(252), from = as.Date(Sys.Date())-N)
          # merge them in the correct format
          for(i in 1:17) gauss_data <- merge(gauss_data,rnorm_xts(mean = input$gaussian.mean,
                                                                  sd = input$gaussian.sd/sqrt(252), from = as.Date(Sys.Date())-N))
          colnames(gauss_data) <- colnames(RAW.Total)
          gauss_data <- gauss_data[1:6015,1:18]
          Shortes_Indeces <- read.csv("data_chosen/AUD_returns_1987-01-15.csv")
          RAW.Data <- xts(gauss_data,order.by = as.Date(Shortes_Indeces[,1])[1:6015])
        }else{
          RAW.Data <- RAW.Total
        }
        
        # gsub is to make sure all the colnames are written correctly and can be compared/joined
        colnames(RAW.Data) <- gsub("-", ".", colnames(RAW.Data))
        
        # we may need to cut off unnecessairy columns of the raw data (in case we did not use all the indices)
        if(sum(colnames(RAW.Data) %in% colnames(targetvola_weights)) != 0 ){
          RAW.Cut <- RAW.Data[,colnames(RAW.Data) %in% colnames(targetvola_weights)]
        }else{
          RAW.Cut <- RAW.Data
        }
        
        # we calculate the returns
        return1 <-  rowSums(targetvola_weights * na.fill(RAW.Cut[index(targetvola_weights),],0))
        return <- xts(return1, order.by = index(targetvola_weights)[1:length(return1)])
        
      })
      
      
      ##############################################
      
      
      
      
      
      #### Plot data #############################################################.
      
      
      #### Comparison of the Strategies ##########################.
      
      #### Both returns in one Plot
      output$targetvalue_comparison <- renderPlot({
        returns_before_target_vola <- returns_before_target_vola()
        targetvola_returns <- targetvola_returns()
        
        return.mat <- merge(returns_before_target_vola,targetvola_returns)
        
        layout(t(c(1,1,1,2)))
        chart.CumReturns(return.mat, wealth.index=TRUE,
                         ylab="cum. returns",
                         main="Comparison of both portfolios", col=1:ncol(return.mat))
        plot(NULL,xlim=c(-1,1),ylim=c(-1,1), yaxt="n",ylab="",xlab="",xaxt="n",frame=F)
        legend("center",legend = colnames(return.mat),
               lty=c(1,1), # gives the legend appropriate symbols (lines)
               lwd=c(5,5),
               cex = 1.8,
               col=1:ncol(return.mat) # gives the legend lines the correct color and width
        )
      })
      
      #### relative performance
      # Plot the relativ performance, comparing before and after the target vola
      output$targetvola_RelativePerformance <- renderPlot({
        returns_before_target_vola <- returns_before_target_vola()
        targetvola_returns <- targetvola_returns()
        PerformanceAnalytics::chart.RelativePerformance(targetvola_returns,returns_before_target_vola)
      })
      
      
      #### Rolling vola
      # calculate the rolling volatility of the target vola returns
      targetvola_rolling_vola_fct = reactive({
        targetvola_returns <- targetvola_returns()
        targetvola_rolling_days = input$targetvola_rolling_days
        
        targetvola_rolling_vola_fct = compute_rolling_sd(targetvola_returns, window_length=targetvola_rolling_days)
        targetvola_rolling_vola_fct
      })
      portfolio_rolling_vola_fct = reactive({
        targetvola_rolling_vola_fct <- targetvola_rolling_vola_fct()
        portfolio_returns <- returns_before_target_vola()
        targetvola_rolling_days = input$targetvola_rolling_days

        portfolio_rolling_vola_fct = compute_rolling_sd(portfolio_returns, window_length=targetvola_rolling_days)
        portfolio_rolling_vola_fct
      })
      
      # plot the rolling volatility of the target vola returns
      output$targetvola_rolling <- renderPlot({
        targetvola_rolling_days = input$targetvola_rolling_days
        targetvola_rolling_vola_fct <- targetvola_rolling_vola_fct()
        portfolio_rolling_vola_fct <- portfolio_rolling_vola_fct()[index(targetvola_rolling_vola_fct),]
        
        min_plot <- min(portfolio_rolling_vola_fct,targetvola_rolling_vola_fct)
        max_plot <- max(portfolio_rolling_vola_fct,targetvola_rolling_vola_fct)
        
        plot.xts(as.zoo(targetvola_rolling_vola_fct),ylab="vola",xlab="Date", ylim= c(min_plot,max_plot),
             main=paste("Rolling volatility of daily returns of the targetvola strategy. (",targetvola_rolling_days,"d)"))
        lines(as.zoo(portfolio_rolling_vola_fct),col="blue")
        abline(h=input$targetvol_volatility/100,col="red")
        legend("topleft",c("target volatility","Rolling volatility input portfolio","Rolling volatility after target vola"),
               col=c("red","blue","black"),lwd = 2)
      })
      
      
      
      
      #### Single Plot ##################################################################.
      ####### before target vola
      ### input returns
      output$returns_before_target_vola_matplot <- renderPlot({
        returns_before_target_vola <- returns_before_target_vola()
        plot.xts(exp(cumsum(returns_before_target_vola)),main="Input portfolio")
      })
      ### input weights
      output$weights_before_target_vola_matplot <- renderPlot({
        weights_before_target_vola <- weights_before_target_vola()
        col <- rainbow(n = ncol(weights_before_target_vola))
        
        xts::plot.xts(weights_before_target_vola[,1],ylim=c(min(weights_before_target_vola),
                                                            max(weights_before_target_vola))
                      ,main="Weights before setting a target volatility")
        for(i in 2:ncol(weights_before_target_vola)) lines(weights_before_target_vola[,i], col=col[i])
      })
      
      ####### after target vola
      ### targetvola returns
      output$targetvola_returns_plot <- renderPlot({
        targetvola_returns <- targetvola_returns()
        plot.xts(exp(cumsum(targetvola_returns)),main="Target vola portfolio")
      })
      ### targetvola weights
      output$targetvola_weights_matplot <- renderPlot({
        targetvola_weights <- targetvola_weights()
        col <- rainbow(n = ncol(targetvola_weights))
        
        xts::plot.xts(targetvola_weights[,1],ylim=c(min(targetvola_weights),max(targetvola_weights)),
                      main="Weights before normalisation")
        for(i in 2:ncol(targetvola_weights)) lines(targetvola_weights[,i], col=col[i])
      })
      
      ####################################################################################
    }
    
    #------------  Trading Costs  -------------
    {
      #### Buttons #################################.
      
      # Radiobuttons to choose, which portoflio should be overlayed with a target vola strategy
      output$trading_costs_chosenStrategy <- renderUI({
        Saved_Strategies <- Saved_Strategies()
        radioButtons("trading_costs_chosenStrategy",
                     label = h3("Choose strategy"), Saved_Strategies, selected = input$set_Vola_chosenStrategy)
      })
      ##############################################
      
      
      #### Load the data ###########################.
      
      # load the weights of the correct portoflio
      weights_before_tradingcosts <- reactive({
        weights_before_tradingcosts <- tradingcosts_weightsfile_fct()
      })
      ##############################################
      
      
      #### Calculate ###############################.
      
      # calculate the portfolio with tradingcosts
      portfolio_transactioncost <- reactive({
        portfolio_transactioncost <- Transactioncosts_anwenden(Money_input = input$money_input, RAW.Data = RAW.Total(),
                                                               Weights = weights_before_tradingcosts(), 
                                                               trade.speed = input$trade.speed,
                                                               trading.costs = input$trading.costs)
      })
      ##############################################
      
      
      
      #### Comparison ##############################.
      
      # Plot the cumulated returns after the trading costs
      output$returns_transactioncost <- renderPlot({
        # Import used data
        RAW.Data <- RAW.Total()
        portfolio_transactioncost <- exp(cumsum(portfolio_transactioncost()[[1]]))
        weights_before_tradingcosts <- weights_before_tradingcosts()
        
        # Calculate the portfolio returns with the weights and raw data
        transaction_costs_raw_returns <- CalculatePortfolioReturns(RAW.Data,weights_before_tradingcosts,input$tradingcosts_delay)
        # transaction_costs_raw_returns <- lag.xts(transaction_costs_raw_returns,input$tradingcosts_delay-1)
        transaction_costs_returns_fct <- exp(cumsum(transaction_costs_raw_returns))
        
        # Find smallest and largest number in both vectors for the size of the plot
        minimum_size <- min(min(transaction_costs_returns_fct),min(portfolio_transactioncost))
        maximum_size <- max(max(transaction_costs_returns_fct),max(portfolio_transactioncost))
        
        # plot the comparison of before and after the trading kosts
        layout(t(c(1,1,1,2)))
        plot.xts(transaction_costs_returns_fct, ylim=c(minimum_size,maximum_size),main="Comparison of the two portfolios")
        lines(portfolio_transactioncost,col="red")
        plot(NULL,xlim=c(-1,1),ylim=c(-1,1), yaxt="n",ylab="",xlab="",xaxt="n",frame=F)
        legend("center",legend = c("input portfolio","portfolio with transactionscosts"),
               lty=c(1,1), # gives the legend appropriate symbols (lines)
               lwd=c(5,5),
               cex = 1.8,
               col=c("black","red") # gives the legend lines the correct color and width
        )
        
      })
      ##############################################
      
      
      #### Single Plot #############################.
      
      # Weights plot of the currently chosen portfolio
      output$weights_transactioncost <- output$weights_plot2 <- renderPlot({
        weights_with_tcosts <- portfolio_transactioncost()[[2]]
        col <- rainbow(n = ncol(weights_with_tcosts))
        layout(t(c(1,1,1,2)))
        
        xts::plot.xts(weights_with_tcosts[,1],ylim=c(min(weights_with_tcosts),max(weights_with_tcosts)),
                      main="Weights after adding transactions costs")
        for(i in 2:ncol(weights_with_tcosts)) lines(weights_with_tcosts[,i], col=col[i])
        
        plot(NULL,xlim=c(-1,1),ylim=c(-1,1), yaxt="n",ylab="",xlab="",xaxt="n",frame=F)
        legend("center",legend = colnames(weights_with_tcosts),
               lty=c(1,1), # gives the legend appropriate symbols (lines)
               lwd=c(3,3),
               cex = 1.4,
               col=col # gives the legend lines the correct color and width
        )
      })
      
      
      # Generate Graph to show how the costs developed over time
      output$tradingcosts_over_time <- renderPlot({
        tradingcosts_time <- portfolio_transactioncost()[[5]]
        layout(t(c(1,1,1,2)))
        if(index(first(tradingcosts_time)) == index(last(tradingcosts_time))){
          plot(NULL,xlim=c(-1,1),ylim=c(-1,1), yaxt="n",ylab="",xlab="",xaxt="n",frame=F)
          text(0.5,0.5,"There were no trades",cex=5,pos=2)
        }else{
          plot.xts(cumsum(tradingcosts_time),main="cumulated transaction costs over time")
        }
      })
      
      
      
      # Output of the Information about the Trades
      output$trade_info <- renderPrint({
        portfolio_transactioncost <- portfolio_transactioncost()[[3]]
        portfolio_transactioncost
      })
      output$spent_on_trading <- renderPrint({
        portfolio_transactioncost <- portfolio_transactioncost()[[4]]
        portfolio_transactioncost
      })
      
      
      ##############################################
      
      
      
    }
    
    
    
  }
  
  ############## Analyse portfolio ##################    TAB 1      ############## TAB 1
  {
    #------------   RD Panel 1  Overview -------------
    {
      
      #------------   RD Reactive Text Output -------------
      {
        # To describe each of the standart portoflios properly we dispaly a fitting text for each of them
        #   depending which portfolio is chosen a different expleneations pops up
        
        ### Title output dependent from the chosen porfolio
        output$pf_title <- renderUI({
          switch(
            if(input$include_targetvola){
              input$set_Vola_chosenStrategy
            }else{
              input$chosenStrategy
            },
            "equal.weights"={
              h3("Equal weights portfolio",style = "font-family: 'times'; font-si16pt")
                 },
                 "efficient"={
                   h3("Efficient portfolio",style = "font-family: 'times'; font-si16pt")
                 },
                 "min.vola"={
                   h3("Minimum volatitlity portfolio",style = "font-family: 'times'; font-si16pt")
                 },
                 "tangency"={
                   h3("Max sharpe ratio portfolio",style = "font-family: 'times'; font-si16pt")
                 },
                 "max.decorrelation"={
                   h3("Max decorrelations portfolio",style = "font-family: 'times'; font-si16pt")
                 },
                 "MinDD"={
                   h3("Minimum Drawdown",style = "font-family: 'times'; font-si16pt")
                 },
                 "MinCVaR"={
                   h3("Minimum Value at Risk",style = "font-family: 'times'; font-si16pt")
                 },
                 {
                   h5("No Standartportfolio was selected")
                 }
          )
        })
        
        ### Description output dependent from the chosen porfolio
        output$pf_description <- renderUI({
          switch(
            if(input$include_targetvola){
              input$set_Vola_chosenStrategy
            }else{
              input$chosenStrategy
            },
                 "equal.weights"={
                   h4("The equal weighted portfolio is the most simple portfolio. All the indices are weighted exaclty the same. If the portfolio contains 100 indices, the weight of one index is 1/100. This portfolio performs good. The portfolio generates an annualize return of 2 % with a standard deviation of 9 %. 
                      This portfolio is used as the benchmark.",style = "font-family: 'times'; font-si16pt")
                 },
                 "efficient"={
                   h4("The efficient portfolio in this application is a maximum return weighted portfolio with a stable return of 10 %. It generates an effective annualized return of 3 % with a standard deviation of 7 %. An area is calculated with all possible combinations that this portfolio could possibly generate. All the efficient portfolios are located on the left border of this area. To calculate all portfolios on the frontier line differet optimisation algorithm are used. The volatility is minimized at this return rate. In the graphic below is located on the top curve of the frontier of an optimized portfolio at a return value of 10 %. This portfolio generally performs really well. In times with high volatility and a downward trend the portfolio still performs better than the benchmark. Furthermore, it generates higher returns in upward trending periods than the benchmark. An equal weighted portfolio is set as the benchmark.",style = "font-family: 'times'; font-si16pt")
                 },
                 "min.vola"={
                   h4("This portfolio is a minimum variance portfolio. The calculations optimize the returns of the portfolio at the minimum volatility. In the graphic below, it is located on the left side of the curve. This portfolio is really stable, it has a standard deviation of only 1 % and a maximum drawdown of 3 % (see risk analysis tab for more detail). For risk averse investers, this is the perfect portfolio. However, the annualized return is at 0 %.",style = "font-family: 'times'; font-si16pt")
                 },
                 "tangency"={
                   h4("The tangency portfolio is the efficient portfolio at the risk free rate. A area is calculated with all possible combinations this portfolio could possibly generate. All the efficient portfolios are located on the left boarder of this area. To calculate all portfolios on the frontier line differen optimisation algorythmes are used. Now the risk free rate is set as a tangecy to this border. The portfolio will generate weights to reach the risk and the returns at the point where the tangency and the border curve of efficient portfolios touch. This portfolio is pretty stable. It has a drawdown risk of 6 % and a standard deviation of 2 %.  
                      The annualized retuns are 3 %.",style = "font-family: 'times'; font-si16pt")
                 },
                 "max.decorrelation"={
                   h4("The maximum decorrelation strategy optimizes the weights so that the correlation between the invested indices in the portfolio is at the minimum. Similar to the minimum volatility portfolio it has a standard deviation of 9 % and an annualized return of 1 %. The maximum drawdown is at 39 %. This portfolio performs the worst compared to the benchmark.",style = "font-family: 'times'; font-si16pt")
                 },
                 "MinDD"={
                   h4("The minimum drawdown portfolio optimizes the drawdowns of the portfolio so a minimum. The indices are weighted optimal at this minimum value. This portfolio tries to minimize the risk of losing more than  winning. Therefore it is expected to be really stable and steady upward trending. However, the performance of the portfolio is not too great. The reason for this is that it can ocure that the optimisation problem for the drawdown is not convex, hence it is difficult to find and optimal allocation.",style = "font-family: 'times'; font-si16pt")
                 },
                 "MinCVaR"={
                   h4("The Value at Risk is minimized in this portfolio. Similar to the minimum draw down portfolio it tries to minimize a risk mesurement. It makes only small losses but does not provide from negative returns. 
The optimisation of the Value at Risk functiones like an efficientcy portfolio, with the Value at Risk as its risk mesurement. The efficient portfolio is generated as a minimum risk portfolio.",style = "font-family: 'times'; font-si16pt")
                 },
                 {
                   
                 }
          )
        })
        
        
      }
      
      
      # Radio buttons for the strategies that are saved inside the application
      output$chosenStrategy <- renderUI({
        if(input$include_targetvola){
          "To change the input portfolio, go back to the targetvolatility tab"
        }else if(input$NormalData){
          "normalised input is activated"
        }else if(input$include_tradingcosts){
          "To change the input portfolio, go back to the tradingcosts tab"
        }else{
          Saved_Strategies <- Saved_Strategies()
          radioButtons("chosenStrategy", label = h3("Choose strategy"), Saved_Strategies,
                       selected=input$set_Vola_chosenStrategy)
        }
      })
      
      
      
      # Overview over the currently chosen portfolio
      output$cum_returns <- renderPlot({
        charts.PerformanceSummary(returns_fct(),main="cumulated portfolio returns",geometric = T,wealth.index = T)
      })
      
      # Weights plot of hte currently chosen portfolio
      output$weights_plot <- output$weights_plot2 <- renderPlot({
        weightsfile_fct <- weightsfile_fct()
        col <- rainbow(n = ncol(weightsfile_fct))
        layout(t(c(1,1,1,2)))
        
        xts::plot.xts(weightsfile_fct[,1],ylim=c(min(weightsfile_fct),max(weightsfile_fct)),main="weights over time")
        for(i in 2:ncol(weightsfile_fct)) lines(weightsfile_fct[,i], col=col[i])
        
        plot(NULL,xlim=c(-1,1),ylim=c(-1,1), yaxt="n",ylab="",xlab="",xaxt="n",frame=F)
        legend("center",legend = colnames(weightsfile_fct),
               lty=c(1,1), # gives the legend appropriate symbols (lines)
               lwd=c(3,3),
               cex = 1.4,
               col=col # gives the legend lines the correct color and width
        )
      })
      
      
    }
    
    #------------   RD Panel 2  Performance Tables -------------
    {
      
      output$Annualized_Returns_table <- renderTable({
        strategy_returns = returns_fct()
        table.AnnualizedReturns(strategy_returns,
                                scale = 252,
                                digits = 2)
      })# end of output Annualized_Returns_table
      
      
      output$TrailingPeriods_table <- renderTable({	
        monthly_pf_returns <- xts::to.monthly(returns_fct())
        table.TrailingPeriods(monthly_pf_returns*100,
                              periods = subset(c(1,2,3,6, 12, 36, 60, 120),
                                               c(1,2,3,6, 12, 36, 60, 120) <
                                                 length(as.matrix(monthly_pf_returns))), 
                              FUNCS = c("sum"),
                              funcs.names = c("Arithmetic return (%)"), digits = 2)
      })# end of output TrailingPeriods_table
      
      
      output$CalendarReturns_table <- renderTable({		
        monthly_pf_returns_fct <- xts::to.monthly(returns_fct())
        table.CalendarReturns(monthly_pf_returns_fct, digits = 1, as.perc = TRUE, geometric = TRUE)
      })# end of output TrailingPeriods_table
      
    }
    
    #------------   RD Panel 3  Strategy return analysis -------------
    {
      output$show_raw_data_portfolio <- renderPlot({
        returns_fct <- exp(cumsum(returns_fct()))
        plot.xts(returns_fct,main="Cumulated portfolio returns")
        abline(v=.index(returns_fct)[last(which(as.Date(input$slider_Date_portfolio-input$width_date_portfolio)
                                                > index(returns_fct)))], col="red")
        abline(v=.index(returns_fct)[last(which(as.Date(input$slider_Date_portfolio+input$width_date_portfolio)
                                                > index(returns_fct)))], col="red")
      })# end of output show_raw_data
      
      # Create input to customise the zoom plot (with and position)
      output$slider_Date_portfolio <- renderUI({
        returns_fct <- exp(cumsum(returns_fct()))
        start_value <- median(as.numeric(index(returns_fct)))
        sliderInput(inputId = "slider_Date_portfolio",label = "Date",min = as.numeric(index(first(returns_fct))),
                    max = as.numeric(index(last(returns_fct))),value = start_value,width = 1200,step = 10)
      }) # end of slider_Date_portfolio
      output$width_date_portfolio_2 <- renderUI({
        returns_fct <- exp(cumsum(returns_fct()))
        width_value <- length(returns_fct)/10
        step_value <- as.numeric(index(returns_fct))/500
        numericInput("width_date_portfolio","Width of the zoom window",value = width_value,step=step_value)
      })
      
      # necessairy information to plot the zoom-plot
      Zoom_plot_data <- reactive({
        returns_fct <- exp(cumsum(returns_fct()))
        Zoom_plot_data <- returns_fct[paste(as.Date(input$slider_Date_portfolio-input$width_date_portfolio),
                                            as.Date(input$slider_Date_portfolio+input$width_date_portfolio),sep = "/")]
        Zoom_plot_data
      }) # end of Zoom_plot_data
      output$zoom_raw_data_portfolio <- renderPlot({
        Zoom_plot_data <- Zoom_plot_data()
        plot.xts(Zoom_plot_data, main= "zoom of the portfolio returns")
        abline(h=input$height_of_zoomed_Volume,col=2)
      }) # end of output show_raw_data
      h_abline_mean <- reactive({
        Zoom_plot_data <- Zoom_plot_data()
        abline_hight <- mean(Zoom_plot_data)
        abline_hight
      }) # end of h_abline_mean
      output$height_of_zoomed_Volume <- renderUI({
        h_abline <- round(h_abline_mean(),3)
        numericInput("height_of_zoomed_Volume","measure the height",
                     value = h_abline ,step=0.01)
      }) # end of height_of_zoomed_Volume
      
      
      # This graphic shows the daily returns of the strategy
      output$strategy_return_timeseries <- renderPlot({
        strategy_returns = returns_fct()
        plot(as.zoo(strategy_returns),ylab="strategy returns",xlab="Date",
             main="Daily returns of the strategy.")
      })# end of output return_timeseries
      
      
      # calculate the rolling volatility of the returns
      strategy_rolling_vola_fct = reactive({
        strategy_returns = returns_fct()
        vola_rolling_days = input$vola_rolling_days
        
        strategy_rolling_vola = compute_rolling_sd(strategy_returns,
                                                   window_length=vola_rolling_days)
        strategy_rolling_vola
      }) # end of strategy_rolling_vola_fct
      # plot the rollin volatility of the returns
      output$strategy_vola_plot <- renderPlot({
        vola_rolling_days = input$vola_rolling_days
        strategy_rolling_vola = strategy_rolling_vola_fct()
        plot(as.zoo(strategy_rolling_vola),ylab="vola",xlab="Date",
             main=paste0("Volatility of daily returns of the strategy. (",vola_rolling_days,"d)"))
        print("finished computing return_timeseries chart")
      })# end of output strategy_vola_plot
      
      
      # This histogram lists all returns of the strategy.
      output$strategy_return_histogramm <- renderPlot({
        strategy_returns = returns_fct()
        strategy_returns = na.omit(strategy_returns)
        chart.Histogram(strategy_returns, 
                        methods = c("add.density", "add.normal"),
                        probability=T, breaks=50)
        abline(v=quantile(strategy_returns,0.05),col="green",lwd=2)
        abline(v=0,col="red")
      })# end of output return histogramm
      
      # Here can be tested if the strategy retuns are distributed in a normal distribution
      output$strategy_return_qq_plot <- renderPlot({
        strategy_returns = returns_fct()
        chart.QQPlot(strategy_returns,envelope=TRUE,distribution = 'norm')
      })# end of output return_qq_plot
      
    }
    
    #------------   RD Panel 3  Risk analysis -------------
    {
      
      # Display risk in many different ways using tables.
      
      output$DownsideRisk_table <- renderTable({
        table.DownsideRisk(returns_fct(), scale = 252, digits = 2)
      })# end of output Downside_Risk_table
      
      
      output$Drawdowns_table <- renderTable({
        dd_table = table.Drawdowns(returns_fct(), digits = 2)
        # below is needed because xtable has a bug where dates are printed as numbers and not as characters
        dd_table[,1] = as.character(dd_table[,1])
        dd_table[,2] = as.character(dd_table[,2])
        dd_table[,3] = as.character(dd_table[,3])
        dd_table = xtable(dd_table, display =c("d","s","s","s","f","d","d","d"))
        dd_table
      }, display =c("d","s","s","s","f","d","d","d"))# end of output Drawdowns_table
      
      output$Snail_Trail_Plot <- renderPlot({
        ## num of days mit numeric input erstellen
        # Snail Trail Plot
        num.of.days <- input$num_of_days
        first.d <- length(returns_fct())-num.of.days
        last.d <- length(returns_fct())
        PerformanceAnalytics::chart.SnailTrail(returns_fct()[first.d:last.d,1])
        # Shows the behaviour of the index for the last number of days.
        # It is really easy to track the development of annualized returns and annualized risk (volatility).
      })
    }
    
    #------------   RD Panel 4  Weight analysis -------------
    {
      # Buttons to chose which weight should be analysed.
      output$radiobuttons_weights <- renderUI({
        sidebarPanel(
          radioButtons("chosenRawDatatest", label = h5("Choose ticker"),
                       colnames(weightsfile_fct())
          ),
          width=20
        )
      }) # end of radiobuttons_weights
      
      
      # list of the variance of the weights, to see how volatile which weight ist.
      output$properties_weights <- renderPrint({
        cutout_weights = weightsfile_fct()
        colVars_weights <- as.data.frame(colVars(cutout_weights))
        colnames(colVars_weights) <- "Variance of the indeces"
        colVars_weights
      }) # end of properties_weights
      
      # load the data of the weights
      cutout_weights <- reactive({
        cutout_weights = weightsfile_fct()[,input$chosenRawDatatest]
        cutout_weights
      })
      # Show single weights
      output$show_single_weights <- renderPlot({
        cutout_weights <- cutout_weights()
        if(input$sell.buy){
          barplot(sign(cutout_weights),col="lightblue1",ylim=c(-1,1),border=NA, space=0,axes=F,axisnames=F)
          par(new=T)
        }
        plot.xts(cutout_weights,main="Cumulated portfolio returns")
        abline(v=.index(cutout_weights)[last(which(as.Date(input$slider_Date_weights-input$width_date_weights)
                                                   > index(cutout_weights)))], col="red",lwd=2)
        abline(v=.index(cutout_weights)[last(which(as.Date(input$slider_Date_weights+input$width_date_weights)
                                                   > index(cutout_weights)))], col="red",lwd=2)
      })# end of output show_single_weights
      
      
      output$width_date_weights <- renderUI({
        cutout_weights <- cutout_weights()
        width_value <- length(cutout_weights)/10
        step_value <- as.numeric(index(cutout_weights))/1000
        numericInput("width_date_weights","width of the zoom",width_value,step = step_value)
      }) # end of width_date_weights
      output$slider_Date_weights <- renderUI({
        weights_fct <- cutout_weights()
        start_value <- median(as.numeric(index(weights_fct)))
        sliderInput(inputId = "slider_Date_weights",label = "Date",min = as.numeric(index(first(weights_fct))),
                    max = as.numeric(index(last(weights_fct))),value = start_value,width = 1200,step = 10)
      }) # end of slider_Date_weights
      
      
      # Create a Zooom plot for the single weights to analyse them with a closer look
      Zoom_plot_data_weights <- reactive({
        weights_fct <- cutout_weights()
        Zoom_plot_data_weights <- weights_fct[paste(as.Date(input$slider_Date_weights-input$width_date_weights),
                                                    as.Date(input$slider_Date_weights+input$width_date_weights),sep = "/")]
        Zoom_plot_data_weights
      }) # end of Zoom_plot_data_weights
      output$zoom_raw_data_portfolio_weights <- renderPlot({
        Zoom_plot_data_weights <- Zoom_plot_data_weights()
        plot.xts(Zoom_plot_data_weights, main= "zoom of the portfolio returns")
        abline(h=input$height_of_zoomed_Volume_weights,col=2)
      }) # end of zoom_raw_data_portfolio_weights
      h_abline_mean_weights <- reactive({
        Zoom_plot_data_weights <- Zoom_plot_data_weights()
        h_abline_mean_weights <- mean(Zoom_plot_data_weights)
        h_abline_mean_weights
      }) # end of h_abline_mean_weights
      output$height_of_zoomed_Volume_weights <- renderUI({
        h_abline_weights <- round(h_abline_mean_weights(),3)
        numericInput("height_of_zoomed_Volume_weights","measure the height",
                     value = h_abline_weights ,step=0.001)
      }) # end of height_of_zoomed_Volume_weights
      
      
    }
    
    #------------   RD Panel 5  VaR and ES -------------
    {
      
      # show the plot of the VaR and ES over the whole time period
      output$VaR_chart <- renderPlot({
        returns_fct <- returns_fct()
        plot(returns_fct[quantile(returns_fct,input$Var_number)<returns_fct],
             type="p",ylim=c(min(returns_fct),max(returns_fct)), main="VaR and ES")
        points(returns_fct[quantile(returns_fct,input$Var_number)>returns_fct],col=2)
        abline(h=quantile(returns_fct,input$Var_number),col=2)
        par(usr = c(0, 1, 0, 1))
        text(0.1,0.1, paste("VaR = ",as.character(round(quantile(returns_fct,input$Var_number),4)),sep=""),
             col="red",cex=1.5)
        text(0.2,0.1, paste("ES = ", round(ES(returns_fct,p = input$Var_number),3) ,sep=""),
             col="blue",cex=1.5)
      })
      
      
      # show the dynamic plot of the VaR and ES
      output$BarVaR_chart <- renderPlot({
        if(input$VaR_and_ES){
          strategy_returns= returns_fct()
          chart.BarVaR(strategy_returns,width=input$Var_days,methods="HistoricalES",
                       show.horizontal=TRUE,show.endvalue=TRUE,
                       show.symmetric=TRUE,show.greenredbars=TRUE,
                       ylab="strategy returns",xlab="Date",p = input$Var_number_2)
        }
      })# end of output BarVaR Chart
      
    }
  }
  
  ############## Advanced analysis ##################    TAB 2      ############## TAB 2
  {
    #------------  Benchmark analysis -------------
    {
      
      # show the realtive performance of the input portfolio compared to the Benchmark
      output$RelativePerformance <- renderPlot({
        PerformanceAnalytics::chart.RelativePerformance(returns_fct(),benchmark())
      })
      
      # direct comparison of both performances
      output$ReturnComparison <- renderPlot({
        returns_fct <- returns_fct()
        benchmark <- benchmark()
        
        # Cutting the portfolio and the Benchmark to the same length
        min_length <- min(length(benchmark),length(returns_fct))
        benchmark <- benchmark[(length(benchmark)-min_length):length(benchmark)]
        returns_fct <- returns_fct[(length(returns_fct)-min_length):length(returns_fct)]
        
        returns_cumsum <- cumsum(returns_fct)
        benchmark_cumsum <- cumsum(benchmark)
        plot.xts(returns_cumsum,ylim=c(min(returns_cumsum,benchmark_cumsum),max(returns_cumsum,benchmark_cumsum)),
                 main="compared performance")
        lines(benchmark_cumsum,col=2)
      })
      
      # comparison of the benchmark to all the given raw data
      output$BenchmarkAndRaw <- renderPlot({
        RAW.Cut <- RAW.Cut()
        benchmark <- benchmark()
        par(usr = c(0, 1, 0, 1))
        benchmark_cumsum <- cumsum(benchmark)
        RAW.Total.cumsum <- cumsum(RAW.Cut)
        plot.xts(benchmark_cumsum, main="Raw data compared with benchmark",
                 ylim=c(min(RAW.Total.cumsum,benchmark_cumsum),max(RAW.Total.cumsum,benchmark_cumsum)))
        for(i in 1:ncol(RAW.Total.cumsum)) lines(RAW.Total.cumsum[,i],col="grey")
        lines(benchmark_cumsum,col=2)
      })
      
      # Capture ratio plot to see the risk and performance of the portfolio reltive to the benchamrk
      output$CaputreRatios_benchmark <- renderPlot({
        returns_fct <- returns_fct()
        benchmark <- benchmark()
        colnames(returns_fct) <- as.character(input$chosenStrategy)
        PerformanceAnalytics::chart.CaptureRatios(Ra = returns_fct, Rb = benchmark,colorset = "red",
                                                  benchmark.color = "blue")
        text(x=1,y=0.9,"Benchmark",pos = 4,cex=1.5,col="blue")
      })
      
    }
    
    #------------  Correlation analysis -------------
    {
      
      # rollin correlation between the benchmark and the portfolio
      output$roll_corr_benchmark <- renderPlot({
        returns_fct <- returns_fct()
        benchmark <- benchmark()
        
        combined_returns = merge(benchmark,returns_fct)
        # your rolling correlation can be at most the length of your timeseries
        max_width = length(index(combined_returns))-2
        chart.multiRollingCorrelation(combined_returns[,1],
                                      combined_returns[,2],
                                      width= min(input$roll_corr_days, max_width),
                                      ylab="correlation",
                                      main=paste0("Correlation strategy returns vs asset returns (",
                                                  min(input$roll_corr_days, max_width),"d)")
        )
      })# end of output roll_corr_ret_strategy
      
      
      
      # Plot the Correlations graphics between the portfolio and the benchmark
      output$corr_ret_strategy <- renderPlot({
        returns_fct <- returns_fct()
        benchmark <- benchmark()
        combined_returns = merge(benchmark,returns_fct)
        chart.Correlation(combined_returns , histogram=TRUE, pch=20,lwd=1)
      })# end of output corr_ret_strategy
      
      
    }
    
    #------------  portfolio Comparison-------------
    {
      
      #### Buttons #################################.
      # create checkboxgourp to chose, which portfolios should be compared.
      output$portfolio_comparison <- renderUI({
        Saved_Strategies <- Saved_Strategies()
        checkboxGroupInput("portfolio_comparison",
                           "Choose the portfolio which to be compared",
                           choices = Saved_Strategies,
                           selected = Saved_Strategies
        )
      })
      ##############################################.
      
      
      
      
      #### Calculations / load Data ################.
      
      return.mat.targetvola.tradingcost <- reactive({
          return.mat.targetvola.tradingcost <- cumsum(na.exclude(return.mat()[,input$portfolio_comparison]))
      })
      
      ##############################################.
      
      
      
      
      #### Plots #################################.
      # plot the chosen portfolio to compare them in one graphic
      output$Portfolio_comparison <- renderPlot({
        return.mat <- return.mat.targetvola.tradingcost()
        
        layout(t(c(1,1,1,2)))
        matplot(exp(return.mat),type="l",axes=F,lty = 1)
        grid(nx = 10, ny = 10, col = "black", lty = "dotted", equilogs = TRUE)
        axis(1, 1:nrow(return.mat), index(return.mat))
        axis(2)

        plot(NULL,xlim=c(-1,1),ylim=c(-1,1), yaxt="n",ylab="",xlab="",xaxt="n",frame=F)
        legend("center",legend = colnames(return.mat),
               lty=c(1,1), # gives the legend appropriate symbols (lines)
               lwd=c(5,5),
               cex = 1.8,
               col=1:ncol(return.mat) # gives the legend lines the correct color and width
        )
      })
      
      
      # create an correlation plot for the portfolios
      output$corr_strategy <- renderPlot({
        return.mat <- na.exclude(return.mat()[,input$portfolio_comparison])
        plot_correlation_matrix(na.exclude(return.mat))
      })# end of output corr_strategy 
      
      # create a second correlation plot for the portfolios, for better visualisation
      output$corr_strategy_second <- renderPlot({
        return.mat <- na.exclude(return.mat()[,input$portfolio_comparison])
        chart.Correlation(return.mat , histogram=TRUE, pch=20,lwd=1)
      })# end of output corr_strategy_second 
      
      # Display the Annualized returns, volatility and Sharpe Ratio for comaprison
      output$Annualized_Returns_table_comparison <- renderTable({
        return.mat <- na.exclude(return.mat()[,input$portfolio_comparison])
        table.AnnualizedReturns(return.mat,
                                scale = 252,
                                digits = 2)
      })# end of output Annualized_Returns_table_comparison
      
      # Display the downside Risk of the compared portfolios
      output$DownsideRisk_table_comparison <- renderTable({
        return.mat <- na.exclude(return.mat()[,input$portfolio_comparison])
        table.DownsideRisk(return.mat, scale = 252, digits = 2)
      })# end of output DownsideRisk_table_comparison
      #############################################.
      
      
    }
    
  }
  
  ############## Analyse raw data ###################    TAB 3      ############## TAB 3
  {
    
    #------------   RD Panel 9  Raw Data analysis -------------
    {
      
      ######### UI Input Programmieren! ##########.
      
      ########## Calculations ##############.
      RAW.data.xts.returns <- reactive({
        RAW.data.xts.returns <- diff(log(RAW.data.xts()))
      })
      
      
      ########## Plots ##############.
      output$show_raw_data <- renderPlot({
        RAW.data.xts <- RAW.data.xts()
        plot.xts(RAW.data.xts,main="Raw Index Plot")
        abline(v=.index(RAW.data.xts)[last(which(as.Date(input$sliderDate-input$width_date) > index(RAW.data.xts)))],
               col="red")
        abline(v=.index(RAW.data.xts)[last(which(as.Date(input$sliderDate+input$width_date) > index(RAW.data.xts)))],
               col="red")
      })# end of output show_raw_data
      
      output$slider_Date <- renderUI({
        sliderInput(inputId = "sliderDate",label = "Date",min = as.numeric(index(first(RAW.data.xts()))),
                    max = as.numeric(index(last(RAW.data.xts()))),value = 14000,width = 1200,step = 10)
      })
      output$show_raw_data_zoom <- renderPlot({
        RAW.data.xts <- RAW.data.xts()
        plot.xts(RAW.data.xts[paste(as.Date(input$sliderDate-input$width_date),
                                    as.Date(input$sliderDate+input$width_date),sep = "/")],main="Zoom of the Index")
        abline(h=input$height_of_zoomed_Raw_Data,col=2)
      })# end of output show_raw_data
      
      Raw_data_zoom_mean <- reactive({
        RAW.data.xts <- RAW.data.xts()
        RAW.data.xts[paste(as.Date(input$sliderDate-input$width_date),
                           as.Date(input$sliderDate+input$width_date),sep = "/")]
      })
      output$height_of_zoomed_Raw_Data <- renderUI({
        n <- 10
        h_abline_raw <- round(Raw_data_zoom_mean()/n)*n
        numericInput("height_of_zoomed_Raw_Data","measure the height",
                     value = h_abline_raw ,step=50)
      })
      
      
      output$show_raw_returns <- renderPlot({
        RAW_Data_xts <- RAW.data.xts()
        charts.PerformanceSummary(diff(log(RAW_Data_xts)),geometric=F)
      })# end of output show_raw_returns
      
      
      output$histogramm_RAW <- renderPlot({
        RAW.data.xts.returns <- RAW.data.xts.returns()
        strategy_returns = na.omit(RAW.data.xts.returns)
        chart.Histogram(strategy_returns, 
                        methods = c("add.density", "add.normal"),
                        probability=T, breaks=100)
        abline(v=quantile(strategy_returns,0.05),col="green",lwd=2)
        abline(v=0,col="red")
      })# end of output return histogramm
      
      
      
      ########## Tables ##############.
      
      output$DownsideRisk_table_RAW <- renderTable({
        RAW.data.xts.returns <- RAW.data.xts.returns()
        table.DownsideRisk(RAW.data.xts.returns, scale = 252, digits = 2)
      })# end of output Downside_Risk_table
      
      
      output$Drawdowns_table_RAW <- renderTable({
        RAW.data.xts.returns <- RAW.data.xts.returns()
        dd_table = table.Drawdowns(RAW.data.xts.returns, digits = 2)
        # below is needed because xtable has a bug where dates are printed as numbers and not as characters
        dd_table[,1] = as.character(dd_table[,1])
        dd_table[,2] = as.character(dd_table[,2])
        dd_table[,3] = as.character(dd_table[,3])
        dd_table = xtable(dd_table, display =c("d","s","s","s","f","d","d","d"))
        dd_table
      }, display =c("d","s","s","s","f","d","d","d"))# end of output Drawdowns_table
      
      
      
      output$Annualized_Returns_table_RAW <- renderTable({
        strategy_returns = RAW.data.xts.returns()
        table.AnnualizedReturns(strategy_returns, 
                                scale = 252, 
                                digits = 2)
      })# end of output Annualized_Returns_table
      
      
      output$TrailingPeriods_table_RAW <- renderTable({
        monthly_pf_returns <- xts::to.monthly(RAW.data.xts.returns())
        table.TrailingPeriods(monthly_pf_returns*100,
                              periods = subset(c(1,2,3,6, 12, 36, 60, 120),
                                               c(1,2,3,6, 12, 36, 60, 120) <
                                                 length(as.matrix(monthly_pf_returns))),
                              FUNCS = c("sum"),
                              funcs.names = c("Arithmetic return (%)"), digits = 2)
      })# end of output TrailingPeriods_table
      
      
      output$CalendarReturns_table_RAW <- renderTable({		
        monthly_pf_returns_fct <- xts::to.monthly(RAW.data.xts.returns())
        table.CalendarReturns(monthly_pf_returns_fct, digits = 0, as.perc = TRUE, geometric = TRUE)
      })# end of output TrailingPeriods_table
      
      
    }
    
    #------------   RD Panel 10  Compare All Raw Data -------------
    {
      
      # Compare all the raw data
      output$RawComparison <-  renderPlot({
        layout(t(c(1,1,1,2)))
        RAW.Total <- RAW.Total()
        RAW.Total.cumsum <- cumsum(RAW.Total)
        
        plot.xts(RAW.Total.cumsum[,1] , ylim = c(min(RAW.Total.cumsum),max(RAW.Total.cumsum)))
        colo <- rainbow(ncol(RAW.Total.cumsum))
        for(i in 1:ncol(RAW.Total.cumsum)) lines(RAW.Total.cumsum[,i], col=colo[i])
        
        plot(NULL,xlim=c(-1,1),ylim=c(-1,1), yaxt="n",ylab="",xlab="",xaxt="n",frame=F)
        legend("center",legend = colnames(RAW.Total.cumsum),
               lty=c(1,1), # gives the legend appropriate symbols (lines)
               lwd=c(3,3),
               cex = 1.4,
               col=colo # gives the legend lines the correct color and width
        )
      })
      
      # Show the distribution of all the raw data in form of a boxplot.
      #   the boxplots are sorter in distribution.
      output$chart_Boxplots <-  renderPlot({
        RAW.Total <- RAW.Total()
        PerformanceAnalytics::chart.Boxplot(RAW.Total,sort.by = "variance")
      })
      
      # Text to show in the correlation plot which colum and which row belongs to which index
      output$corr_raw_data_text1 <- renderPlot({
        if(input$Corr_plot_raw){
          RAW.Total <- RAW.Total()
          col_names_raw <- colnames(RAW.Total)
          par(mar=c(0,2,0,2)) # unten - links - oben - rechts
          plot(NULL,xlim=c(0,1),ylim=c(0,1), yaxt="n",ylab="",xlab="",xaxt="n",frame=F)
          text(seq(0,1,by = 1/(length(col_names_raw)-1)),0,col_names_raw,pos=4,srt = 90)
        }
      })
      output$corr_raw_data_text2 <- renderPlot({
        if(input$Corr_plot_raw){
          RAW.Total <- RAW.Total()
          col_names_raw <- colnames(RAW.Total)
          par(mar=c(2,0,2,0)) # unten - links - oben - rechts
          plot(NULL,xlim=c(0,1),ylim=c(0,1), yaxt="n",ylab="",xlab="",xaxt="n",frame=F)
          text(0,seq(0,1,by = 1/(length(col_names_raw)-1)),rev(col_names_raw),pos=4)
        }
      })
      # actually plot the correlation plot
      output$corr_raw_data <- renderPlot({
        if(input$Corr_plot_raw){
          RAW.Total <- RAW.Total()
          chart.Correlation(RAW.Total , histogram=TRUE, pch=20,lwd=1)
        }
      })# end of output corr_raw_data 
    }
    
  }
  
  
})# End of shiny server!
