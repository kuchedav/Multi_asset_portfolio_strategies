################################################################################
################################################################################
# Here we have the UI file
# the user interface gets created in this file
################################################################################
################################################################################

# Author: David Kuchelmeister
###############################################################################


shinyUI(
  navbarPage("Overview diversification strategies",
             
             tabPanel("Build portfolio",
                      fluidPage(
                        ############# Main Panel #############
                        mainPanel(
                          tabsetPanel(type="tab", #Tabs are generated in this environment
                                      
                                      #################### Roberto David #####################
                                      
                                      #------------   RD Panel 1  -------------
                                      tabPanel("Calculate portfolio",
                                               
                                               # The portfolios can be recalculated.
                                               # Furthermore, it is possible to select which indices should be included
                                               #  in the new portoflio.
                                               
                                               column(12,
                                                      h2("Calculate the portfolio", align = "center",style = "color:blue"),
                                                      h4("Create your own portfolio by choosing which indices, properties and strategy
                                                         you wish to calculate with.", align = "center"),
                                                      helpText("A message will appear, when the calculations are finished",
                                                               align = "center"),
                                                      
                                                      # h4("The calculation of a new portfolio does not work in the online application.",
                                                         # align = "center", style = "color:red"),
                                                      br(),br(),br()
                                               ),
                                               column(1,br(),br(),br(),br(),br(),br()),
                                               
                                               
                                               
                                               # Selection of the indices, which should be included.
                                               column(4,
                                                      h3("Indices", align="left",style = "color:blue"),
                                                      checkboxGroupInput(inputId = "Chosen_Indeces",
                                                                         label = "Please choose which from the following
                                                                    indices should be included to calculate the new portfolio",
                                                                         choices = RAW_Data_input_names,
                                                                         selected = RAW_Data_input_names)
                                               ),
                                               
                                               # Choose the portfolio and the amount of steps, it should calculate back in time.
                                               # It's also possible to change the input for the strategies to a gaussian distributet index.
                                               column(4,
                                                      h3("Properties", align="left",style = "color:blue"),
                                                      numericInput(inputId = "lookback_distance",
                                                                   label = "The number of days that are concidered in the
                                                              calculation of the portfolio",
                                                                   value = 5000,step = 100,max = 5000,min = 50),
                                                      helpText("Minimum 500, maximum 5,000 steps back"),
                                                      numericInput("delay_in_days", "How many days delay should the algorithm have?", 
                                                                   1,min=0),
                                                      br(),
                                                      
                                                      h3("Strategy", align="left",style = "color:blue"),
                                                      uiOutput("Chosen_Strategie"),
                                                      br()
                                               ),
                                               
                                               
                                               column(3,
                                                      # Execute the calculations.
                                                      h3(strong("Calculate the portfolios"), align="left",
                                                         style = "color:blue"),
                                                      actionButton("Recalc_Data",strong("Start calculations",
                                                                                        style = "color:blue"),
                                                                   align = "center"),
                                                      h4(textOutput("calcualte.portfolios")),
                                                      textOutput("TextIndeces"),
                                                      br(),br(),br(),br(),
                                                      
                                                      
                                                      # HTML buttons to quickly switch to the wished tab.
                                                      
                                                      h4("Set a target volatility for the portfolio"),
                                                      "Overlay your portfolio with a target volatility",
                                                      br(),
                                                      # HTML button, whoch directs the user directly to a chosen tab.
                                                      HTML("<div id='linkTo_target_vola'><FORM><INPUT Type='BUTTON'
                                                           VALUE='Target volatility'></FORM></div>"),
                                                      tags$script("$('#linkTo_target_vola').click(function()
                                                          { tabs = $('.tabbable .nav.nav-tabs li a'); $(tabs[1]).click();
                                                      })"),
                                                      helpText("Directly switch to the target volatility tab"),
                                                      br(),
                                                      
                                                      h4("Include transaction costs"),
                                                      "Include the transaction costs for the portoflios",
                                                      # HTML Button, whoch directs the user directly to a chosen tab.
                                                      HTML("<div id='linkTo_transcation_cost'><FORM><INPUT Type='BUTTON'
                                                           VALUE='Transaction costs'></FORM></div>"),
                                                      tags$script("$('#linkTo_transcation_cost').click(function()
                                                                  { tabs = $('.tabbable .nav.nav-tabs li a');
                                                          $(tabs[2]).click();
                                                      })"),
                                                      helpText("Directly switch to the transaction costs tab"),
                                                      
                                                      br(),br(),br(),br(),br(),br(),br(),
                                                      
                                                      
                                                      # Stop the application by pressing this button.
                                                      h3(strong("Stop the application"), align="left", style = "color:red"),
                                                      actionButton("stop_app", strong("Terminate shiny application",
                                                                                      style = "color:red"))
                                               ),
                                               
                                               
                                               
                                               # These columns are used sometimes to arrange the buttons nicely.
                                               column(12,hr(),br(),br(),br()),
                                               
                                               column(12,
                                                      h2("Upload your own data", align = "center"),
                                                      h4("It is possible to upload your own data. This will overwrite
                                                    the current test data",
                                                         align = "center"),
                                                      helpText("The application needs to be restarted to go back to the test data",
                                                               align = "center"),
                                                      br(),br(),br()
                                               ),
                                               column(2,br(),br(),br()),
                                               
                                               column(2,
                                                      # It is possible to upload your own portfolio data, which you can
                                                      #  analyse with our application.
                                                      h4("Upload portfolio returns"),
                                                      
                                                      # Uploading the returns for the portfolio.
                                                      helpText("Upload .csv files where the first column consists of time-dates
                                                          and the second column of the returns"),
                                                      fileInput("path_pf_returns", "Upload the returns"),
                                                      br()
                                               ),
                                               column(1,br(),br(),br()),
                                               
                                               column(2,
                                                      # Uploading the weights for the portfolio.
                                                      h4("Upload portfolio weights"),
                                                      helpText("Upload .csv files where the first column consists of time-dates 
                                                          and the second column of the weights"),
                                                      fileInput("path_pf_weights","Upload the weights"),
                                                      br()
                                               ),
                                               
                                               column(1,br(),br(),br()),
                                               
                                               column(2,
                                                      # Uploading a benchmark to compare the performance of the potfolio with.
                                                      h4("Upload benchmark"),
                                                      helpText("Upload .csv files where the first column consists of time-dates
                                                          and the second column of the benchmark returns"),
                                                      fileInput("path_benchmark","Upload the benchmark returns")
                                               ),
                                               
                                               
                                               
                                               column(12,hr(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br()),
                                               
                                               column(12,
                                                      # Enter normalized data instead of the portfolio returns.
                                                      h2("Gaussian input", align = "center"),
                                                      # Tick the box, if you want to replace all the portfolio data with a gaussian input.
                                                      h4("Use gaussian input instead", align = "center"),
                                                      helpText("the application needs to be restarted to go back to the example data",
                                                               align = "center"),
                                                      br()
                                               ),
                                               
                                               column(3,br()),
                                               
                                               
                                               column(3,
                                                      # Replace input indeces with gaussian distributet data.
                                                      h3("Gauss indices", align="left"),
                                                      helpText("The indices used as input for the portfolios will be replaced with a gaussian input"),
                                                      br(),
                                                      # Properties of the gaussian distributet input.
                                                      numericInput("gaussian.mean", "Mean of the normal distribution",0),
                                                      numericInput("gaussian.sd", "Standard deviation of the normal distribution (yearly)" ,
                                                                   value=0.2),
                                                      checkboxInput("gaussian","Gaussian indices")
                                               ),
                                               
                                               column(3,
                                                      # Replace input portfolio with gaussian distributet data.
                                                      h3("Gauss portfolio", align="left"),
                                                      helpText("Portfolios will be replaced with a gaussian input"),
                                                      br(),br(),
                                                      # The properties of the input chan be chosen with these thwo numericInputs.
                                                      numericInput("MeanNormalData",0,step=0.01, label="Mean of the normal distribution"),
                                                      numericInput("VarNormalData", label="Standard deviation of the normal distribution (yearly)",
                                                                   value = 1,step=0.1),
                                                      checkboxInput("NormalData", "Gaussian portfolio", value = FALSE)
                                                      
                                                      
                                               ),
                                               
                                               
                                               column(12,hr(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br())
                                               
                                      ),
                                      
                                      
                                      
                                      
                                      
                                      
                                      
                                      
                                      #------------   Add target volatility  -------------
                                      tabPanel("Add target volatility",
                                               br(),br(),
                                               helpText("Calculations may take a few seconds"),
                                               uiOutput("comment_normalised"),
                                               
                                               sidebarLayout(
                                                 sidebarPanel(
                                                   h3("Include target volatility"),
                                                   checkboxInput("include_targetvola",
                                                                 strong("Add the target volatility to your portfolio")),
                                                   
                                                   # Choosing which strategy should be shown.
                                                   uiOutput("set_Vola_chosenStrategy"),
                                                   # Define de properties of the target volatility layer.
                                                   uiOutput("targetvol_lookback"),
                                                   uiOutput("targetvol_volatility"),
                                                   hr(),br(),br(),br(),
                                                   h3("Advanced manipulation"),
                                                   uiOutput("LowerBoundStdev_targetvola"),
                                                   uiOutput("UpperBoundWeight_targetvola"),
                                                   uiOutput("LowerBoundWeight_targetvola"),
                                                   helpText("To cancel the limitation, set the values to zero"),
                                                   
                                                   br(),br(),br(),
                                                   checkboxInput("gauss_input_data","Is the portfolio calculated with gaussian input?"),
                                                   width=2
                                                 ),#End of sidebar Panel
                                                 
                                                 
                                                 mainPanel(
                                                   h1("Target volatility", style = "font-family: 'times'; font-si16pt"),
                                                   br(),
                                                   column(9,
                                                          column(12,h4("As an additional adjustment of the portfolio a target volatility can be set.
                                                              This process takes the calculated portfolio returns from the different strategies
                                                              and weights them new, so that a target volatility of the portfolio is reached.",
                                                                       align="left", style = "font-family: 'times'; font-si16pt")
                                                          ),
                                                          
                                                          column(12,img(src='Target_Vola_Formula.png', align = "left",widht=10,height=50)),
                                                          
                                                          column(12,h4("By setting a small target-volatility, the portfolio fluctuation
                                                                        is regulated by the amount of invested capital. This influences
                                                                        the portfolio returns by damping their fluctuation to the wished value.",
                                                                       align="left", style = "font-family: 'times'; font-si16pt")
                                                          )
                                                          
                                                          
                                                   ),br(),br(),br(),br(),br(),br(),
                                                   
                                                   column(12,
                                                          plotOutput("targetvalue_comparison"), br(),
                                                          plotOutput("targetvola_RelativePerformance")
                                                   ),
                                                   
                                                   br(),br(),br(),
                                                   column(12,
                                                          h2("Rolling volatility",
                                                             align="left", style = "font-family: 'times'; font-si16pt"),
                                                          h4("The following graph shows the volatility over time of a raw portfolio (blue)
                                                      and after applying the target volatility (black). It is clearly visible, that
                                                      the volatility after the target volatility is applied fluctuates around the set
                                                      target volatility (red line).",
                                                             align="left", style = "font-family: 'times'; font-si16pt"),
                                                          plotOutput("targetvola_rolling")
                                                   ),
                                                   
                                                   column(3,br()),
                                                   
                                                   column(9,
                                                          sliderInput("targetvola_rolling_days",
                                                                      "Days of rolling width:", width = 500,
                                                                      min = 2,  max = 504, value = 252, step=4),
                                                          helpText("common error: input is larger than the timeseries")
                                                   )
                                                   ,width=10)
                                               ),	
                                               hr(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),
                                               column(6,
                                                      h1("Input portfolio",align = "center"),
                                                      plotOutput("returns_before_target_vola_matplot"),
                                                      plotOutput("weights_before_target_vola_matplot")
                                               ),
                                               column(6,
                                                      h1("Targetvola portfolio",align = "center"),
                                                      plotOutput("targetvola_returns_plot"),
                                                      plotOutput("targetvola_weights_matplot")
                                               )
                                      ), # end tabPanel Add target volatility
                                      
                                      
                                      
                                      
                                      
                                      
                                      #------------   Add transaction cost  -------------
                                      tabPanel("Add transaction cost",
                                               br(),br(),
                                               helpText("Calculations may take a few seconds"),
                                               sidebarLayout(
                                                 sidebarPanel(
                                                   h3("Include transaction costs"),
                                                   checkboxInput("include_tradingcosts",
                                                                 strong("Add the transaction costs to your portfolio")),
                                                   
                                                   # Choosing which strategy should be shown.
                                                   uiOutput("trading_costs_chosenStrategy"),
                                                   numericInput("money_input","Tnvested amount of money",value = 100000, min = 0, step = 1000),
                                                   numericInput("trading.costs","Cost per trade",value = 1, min = 0, step = 1),
                                                   numericInput("trade.speed","Reinvesting rate",value = 0.5, min = 0.01, step = 0.01),
                                                   helpText("The algorithm reinvests in a single index if the index changes more than
                                                            the reinvesting rate"),
                                                   br(),
                                                   numericInput("tradingcosts_delay","How many days of delay does the input portfolio have?",
                                                                value = 1, min = 0),
                                                   width=2
                                                 ),#End of sidebar Panel
                                                 
                                                 mainPanel(
                                                   h1("Transaction costs", style = "font-family: 'times'; font-si16pt"),
                                                   column(9,
                                                          h4("All implemented strategies calculate their weights and returns on a daily basis.
                                                             Yet, in reality the reinvestment rate of the portfolios differs for every investor.
                                                             Most investors do not invest that often, because the trading costs are too high
                                                             compared to the profit it would gain. Therefore it is useful to implement
                                                             transaction costs and reinvesting rate into a portfolio.",
                                                             align="left", style = "font-family: 'times'; font-si16pt")
                                                   ),br(),br(),br(),br(),br(),br(),
                                                   
                                                   plotOutput("returns_transactioncost"),br(),br(),br(),
                                                   
                                                   column(9,
                                                          h4("The transaction costs prevent the
                                                              algorithm from reinvesting daily in the indices. From now on the algorithm
                                                              only reinvests, when the changes of the weights over time exceed the reinvesting rate.
                                                              In case an index fluctuates inside the given reinvesting rate and never exceeds
                                                              the given limit, then the index will not be traded. Every transaction costs
                                                              money, therefore it is possible to enter the amount of invested money and
                                                              the actual costs of every trade.",
                                                             align="left", style = "font-family: 'times'; font-si16pt")
                                                   ),br(),br(),br(),br(),br(),br(),
                                                   
                                                   plotOutput("weights_transactioncost"),br(),br(),br(),
                                                   
                                                   column(9,
                                                          h4("The graph below shows the cumulated costs over time.",
                                                             align="left", style = "font-family: 'times'; font-si16pt")
                                                   ),
                                                   plotOutput("tradingcosts_over_time"),
                                                   hr(),br(),br(),br(),
                                                   h1("Informations regarding the Transactions"),
                                                   strong("spent money in transactions"),
                                                   verbatimTextOutput("spent_on_trading"),
                                                   strong("A List of all the trades."),
                                                   verbatimTextOutput("trade_info")
                                                   ,width=10)
                                               )
                                      ), # end tabPanel Transaction cost
                                      
                                      
                                      #------------   Add transaction cost  -------------
                                      tabPanel("Instructions",
                                               
                                               
                                               column(7,
                                                      
                                                      # In this part of the shiny application is the introduction
                                                      # If a User is not sure how to operate the application or what the possibilities are
                                                      #   he can go to the "Instruction" tab and inform himself
                                                      
                                                      
                                                      
                                                      #------------ Usage of the shiny application ----------------
                                                      
                                                      h1("Usage of the Shiny application", style = "font-family: 'times'; font-si16pt"),
                                                      
                                                      h4("This section explains the handling of the shiny application.",
                                                         style = "font-family: 'times'; font-si16pt"),br(),br(),
                                                      
                                                      
                                                      h2("1. Overview over the application", style = "font-family: 'times'; font-si16pt"),
                                                      h4("The application is built so that the user can easily find his way through the tabs.
                                                          It is ordered in an instinctive way so a quick and logical handling of the application
                                                          is guaranteed. Every tab has an introduction and explanations about the graphics.",
                                                         style = "font-family: 'times'; font-si16pt"),
                                                      
                                                      h4("On the right side a tree diagram shows all the primary and sub tabs (Figure 1). 
                                                          It is meant to guide the user from top to bottom through the application.
                                                          The 'Overview diversification strategies' is the name of the shiny application.
                                                          The level 2 categories are main tabs, visible on top of the application",
                                                         style = "font-family: 'times'; font-si16pt"),
                                                      column(12,img(src='Structure_Shiny2.png', align = "left",widht=100,height=50),br(),br(),br()),
                                                      column(12,
                                                             h4("Level 3 categories are shown as tabs inside the level 3 tabs",
                                                                style = "font-family: 'times'; font-si16pt"),
                                                             img(src='Structure_Shiny3.png', align = "left",widht=100,height=60)
                                                      ),
                                                      column(12,br(),br(),br(),br(),br(),br(),br(),hr()),
                                                      
                                                      
                                                      
                                                      #------------ Categories explenation ----------------
                                                      
                                                      h2("2. Categorie description", style = "font-family: 'times'; font-si16pt"),
                                                      h3("2.1 Build portfolio", style = "font-family: 'times'; font-si16pt"),
                                                      h4("The first primary tab is where the portfolios get created
                                                                  and specified to the wishes of the user.",
                                                         style = "font-family: 'times'; font-si16pt"),
                                                      
                                                      
                                                      h4(strong("Calculate portfolio"), style = "font-family: 'times'; font-si16pt"),
                                                      h4("The “Calculate Portfolio” tab is the first tab which appears when the application
                                                          is started. To start building a portfolio the user can select the indices he
                                                          desires to include. One can choose the amount of days the portfolio should calculate
                                                          back in time and the amount of days the portfolio lags behind with investing at the market.
                                                          Finally, the strategy that should be used has to be chosen.",
                                                         style = "font-family: 'times'; font-si16pt"),
                                                      
                                                      
                                                      h4(strong("Add target volatility"), style = "font-family: 'times'; font-si16pt"),
                                                      h4("The target volatility tab allows to overlay the basic
                                                                    portfolio with a target volatility.
                                                                    The weights and therefore the investment will be changed to a level where the
                                                                    desired volatility is reached. ",
                                                         style = "font-family: 'times'; font-si16pt"),
                                                      
                                                      
                                                      h4(strong("Add transaction costs"), style = "font-family: 'times'; font-si16pt"),
                                                      h4("Because most portfolios don’t get traded every day and trades cost money,
                                                          we need to add an option where the portfolio can be simulated how it would
                                                          react depending on a reinvesting rate and the costs of a single trade.",
                                                         style = "font-family: 'times'; font-si16pt"),
                                                      
                                                      
                                                      
                                                      h3("2.2 Analyse portfolio", style = "font-family: 'times'; font-si16pt"),
                                                      h4("After successfully creating a portfolio all interesting properties can
                                                         be analysed throughout the sub tabs. The first tab gives an overview
                                                         over the chosen portfolio. The rest of the tabs can be used to
                                                         analyse the portfolio concerning returns, disrtribution, weights and
                                                         many different risk measurments.",
                                                         style = "font-family: 'times'; font-si16pt"),
                                                      
                                                      h3("2.3 Advanced analysis", style = "font-family: 'times'; font-si16pt"),
                                                      h4("This tab allows to compare the created portfolio to a benchmark,
                                                          concerning performance and correlation. This benchmark is normally set
                                                          to the Equal-weighted portfolio but it is possible to load your own benchmark
                                                          into the application. Furthermore, all the different strategies can be compared
                                                          with each other. This tab gives an overview about the
                                                          characteristics of all portfolios.",
                                                         style = "font-family: 'times'; font-si16pt"),
                                                      
                                                      h3("2.4 Analyse raw data", style = "font-family: 'times'; font-si16pt"),
                                                      h4("It is not always useful to diversify the portfolio as much as possible.
                                                         Sometimes it is better to leave some indices apart. Therefore the application
                                                         offers the possibility to analyse the raw data. With the necessairy information
                                                         about the behaviour of the single indixes the investor can choose it’s
                                                         own composition of raw data. Furthermore, all the raw indeces are compared
                                                         with each, concerning returns, distribution and correlation.",
                                                         style = "font-family: 'times'; font-si16pt"),br(),br(),br(),br(),br(),hr(),
                                                      
                                                      
                                                      
                                                      #------------ Categories explenation ----------------
                                                      h2("3. implement your own strategy", style = "font-family: 'times'; font-si16pt"),
                                                      
                                                      h4("The application is built in a generic way. Therefore it is possible to
                                                         implement a new portfolio strategy with simple drag and drop actions.
                                                         The R file with the new strategy needs to be moved to the Portfolio/Strategies Folder.",
                                                         style = "font-family: 'times'; font-si16pt"),
                                                      
                                                      h4("The R-Script needs to fulfil three simple conditions:",
                                                         style = "font-family: 'times'; font-si16pt"),br(),
                                                      
                                                      column(12,img(src='Structure_Shiny4.png', align = "left",widht=450,height=450)),
                                                      # Make a huge space at the end of the text
                                                      # It enables to scroll the text to the middle of the window, because it is very annoying
                                                      #   if the text is glued to the bottom of the screen
                                                      br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),
                                                      br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br()
                                                      
                                               ),
                                               column(4,
                                                      #------------ Show Graphic on the right side of the text --------
                                                      br(),br(),br(),
                                                      column(12,img(src='Structure_Shiny.png', align = "right",widht=110,height=700)),
                                                      helpText("Figure 1: Structure of the shiny application",align ="center")
                                               )
                                      )
                                      
                                      
                                      
                                      
                                      
                          )  
                          ,width=12) # end of mainPanel
                      ) # end of fluidPage
                      
             ), # end of tabPanel
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             tabPanel(
               "Analyse portfolio",
               
               fluidPage(
                 headerPanel(h1("Analyse portfolio",style = "font-family: 'times'; font-si16pt"), windowTitle = "strategy"),
                 hr(),br(),br(),
                 sidebarLayout(
                   sidebarPanel(
                     # Choosing which strategy should be shown.
                     uiOutput("chosenStrategy"),
                     br(),
                     
                     # Activated the display of the portfolios with the data input.
                     actionButton("goButton","Update data"),
                     helpText("Data in the application will be synchronised with the databank"),
                     # Date input from when to when the calculated data should be shown.
                     dateInput("start_date", "Start date:", value = as.POSIXlt("1990-01-01",tz="UTC")),
                     dateInput("end_date", "End date:", value = as.POSIXlt("2016-02-16",tz="UTC") ),
                     width=2
                   ),#End of sidebar Panel
                   
                   
                   
                   ############# Main Panel #############
                   mainPanel(
                     tabsetPanel(type="tab", #Tabs are generated in this environment
                                 
                                 
                                 
                                 #################### Roberto David #####################
                                 
                                 #------------   RD Panel 1  -------------
                                 tabPanel("Overview",
                                          
                                          # We give a general overview of the portfolio.
                                          h1("Overview of the portfolio",
                                             style = "font-family: 'times'; font-si16pt"),
                                          
                                          h4("This tab gives a general overview of the portfolio. It shows the portfolio returns
                                             and the weights. The specific tabs can be opened for more details.",
                                             align="left",
                                             style = "font-family: 'times'; font-si16pt"),
                                          
                                          # The description changes, depending on which portfolio is chosen.
                                          uiOutput("pf_title"),
                                          uiOutput("pf_description"),
                                          br(),
                                          
                                          # A general overview of the portoflio is plotted, with the raw,
                                          #   cumulated log returns
                                          #   and with the draw downs. all in one plot.
                                          h3("Portfolio returns", style = "font-family: 'times'; font-si16pt"),
                                          # Geometric or arithmetic returns.
                                          h4("The first graphic shows the performance of the chosen strategy.  It shows the
                                            cumulated returns. The scale starts at 1. A return with the height of 1.15 is equal to
                                            a win of 15 %.",
                                             style = "font-family: 'times'; font-si16pt"),
                                          h4("The second time series shows the returns of the strategy. Days with exceptional high
                                            or low returns can be examined by searching the height peaks, reaching the borders
                                            of the graphic.",
                                             style = "font-family: 'times'; font-si16pt"),
                                          h4("The third tool displays a chart that shows the drawdown history (including the
                                            maximum drawdown). The drawdown is the amount of loss that the strategy has incurred since
                                            its previous peak. Zero drawdown indicates a new peak.",
                                             style = "font-family: 'times'; font-si16pt"),
                                          
                                          
                                          plotOutput("cum_returns", height = 800),br(),br(),br(),
                                          
                                          # Display of the weights.
                                          h3("Portfolio weights", style = "font-family: 'times'; font-si16pt"),
                                          h4("This plot shows the development of the weights over time to reach the returns
                                            calculated for the strategy.", 
                                             style = "font-family: 'times'; font-si16pt"),
                                          plotOutput("weights_plot"),
                                          br()
                                          
                                 ),
                                 
                                 
                                 
                                 
                                 #------------   RD Panel 2  -------------
                                 tabPanel("Performance tables",
                                          # The performance table show the numerical feedback to the portfolio.
                                          h1("Performance tables of the portfolio", align="left",
                                             style = "font-family: 'times'; font-si16pt"),
                                          h4("In this tab, all the performances of the chosen portfolio are shown.", 
                                             align="left", style = "font-family: 'times'; font-si16pt"),
                                          hr(),br(),br(),br(),
                                          fluidRow(
                                            column(4,
                                                   # The annualized portfolio returns show how it performed over the selected time.
                                                   h3("Annualized portfolio returns", align="left",
                                                      style = "font-family: 'times'; font-si16pt"),
                                                   "Table of annualized return, annualized std dev",
                                                   br(),
                                                   "and annualized Sharpe ratio",
                                                   tableOutput("Annualized_Returns_table")
                                            ), # end of column
                                            column(8,
                                                   # Trailing peridos show the performance in the last time periods.
                                                   h3("Trailing periods", align="left",
                                                      style = "font-family: 'times'; font-si16pt"),
                                                   "A table of estimates of rolling period return measures",
                                                   tableOutput("TrailingPeriods_table")
                                            ) # end of column
                                          ), # end of fluidRow
                                          fluidRow(
                                            hr(),br(),br(),br(),
                                            # The calendart returns show the performance in specific months.
                                            h3("Calendar returns", align="left",style = "font-family: 'times'; font-si16pt"),
                                            "Calender returns show the performance in average of each month",
                                            tableOutput("CalendarReturns_table")
                                          ) # end of fluidRow
                                 ),# end of tabPanel Performance tables
                                 
                                 
                                 
                                 
                                 #------------   RD Panel 3  -------------
                                 tabPanel("Strategy return analysis",
                                          
                                          # This part shows the log returns of the portfolio.
                                          # It is possible to analyse a specific time period with the zoom plot.
                                          h1("Analysis of the strategy returns", align="left",
                                             style = "font-family: 'times'; font-si16pt"),
                                          h4("On this page the strategy returns are shown in different ways.", align="left",
                                             style = "font-family: 'times'; font-si16pt"),
                                          h3("Strategy return analysis",align="left",
                                             style = "font-family: 'times'; font-si16pt"),
                                          plotOutput("show_raw_data_portfolio"),
                                          column(12,offset = 1,uiOutput("slider_Date_portfolio")),
                                          column(3,
                                                 br(),br(),br(),br(),
                                                 uiOutput("width_date_portfolio_2"),
                                                 br(),br(),
                                                 h4("Use this height line to easily measure specific points in the zoomed plot.",
                                                    style = "font-family: 'times'; font-si16pt"),
                                                 uiOutput("height_of_zoomed_Volume")
                                          ),
                                          column(9, plotOutput("zoom_raw_data_portfolio")),
                                          column(12, hr(),br(),br(),br(),br(),br()),
                                          
                                          
                                          # This plot shows the raw returns.
                                          h3("Strategy return timeseries",style = "font-family: 'times'; font-si16pt"),
                                          h4("This graphic shows the daily returns of the strategy.", align="left",
                                             style = "font-family: 'times'; font-si16pt"),
                                          plotOutput("strategy_return_timeseries"),
                                          
                                          # The volatility plot shows the volatility over time.
                                          # This is not the normal overall volatility, it is calculated in a "rolling" form.
                                          # The rolling width can be modified with the slider input.
                                          h3("Volatility of strategy return timeseries", align="left",
                                             style = "font-family: 'times'; font-si16pt"),
                                          h4("The rolling volatility of the strategy returns is shown in this graphic.
                                              The width of the rolling period can be addjusted with the slidebar beneath
                                              the graphic. The standard value of this period is 252 days, representing
                                              one year.", style = "font-family: 'times'; font-si16pt"),
                                          plotOutput("strategy_vola_plot"),
                                          sliderInput("vola_rolling_days",
                                                      "Days of rolling width:",
                                                      min = 2,  max = 504, value = 252, step=4),
                                          hr(),br(),
                                          
                                          # The histogramm shows how the retuns are distributed.
                                          # It tries to fit a normal distributed distribution and shows the empirical
                                          #   distribution aswell.
                                          h3("Strategy return histogram", align="left",
                                             style = "font-family: 'times'; font-si16pt"),
                                          h4("This histogram lists all returns of the strategy. If the peack of the
                                              distribution is on the right side of the red line at 0, the strategy has more
                                              positive returns than negative. If the peak is on the left, the strategy generates
                                              more negative retuns than positive.
                                              On the left side the 95 % percentile is marked with a thick green line. It seperates
                                              the biggest 5 % losses in the chosen period on the left side from the other returns.
                                              A distribution is fitted over the histogram. The blue line is a fitted normal
                                              distribution and the purple line shows the densety.", 
                                             style = "font-family: 'times'; font-si16pt"),
                                          plotOutput("strategy_return_histogramm"),  
                                          hr(),
                                          br(),
                                          
                                          # With the strategy return qq-plot can a normal-distributino be fitted to the returns.
                                          h3("Strategy return qq-plot", align="left",
                                             style = "font-family: 'times'; font-si16pt"),
                                          h4("Here can be tested if the strategy retuns are distributed in a normal distribution.
                                              If both tails are over the line the distribution is right skewed, if both tails are under the line the
                                              distribution is left skewed. The higher the scattering of the dots at the ends of the
                                              line is, the heavier is the tail of the distribution.",
                                             style = "font-family: 'times'; font-si16pt"),
                                          plotOutput("strategy_return_qq_plot"),
                                          
                                          hr()
                                 ),# end of tabPanel "Return analysis"	
                                 
                                 
                                 
                                 #------------   RD Panel 4  -------------
                                 tabPanel("Risk analysis",
                                          # This panel analyses the risk of the portfolio.
                                          h1("Analysis of the risk", align="left",
                                             style = "font-family: 'times'; font-si16pt"),
                                          h4("The risk analysis page shows the downside risk, drawdowns and the behavior of
                                              risk compared to the strategy returns.", align="left",
                                             style = "font-family: 'times'; font-si16pt"),
                                          
                                          column(1),
                                          column(4,
                                                 # The first table shows teh downside risk.
                                                 # How porbable is it, that the portfolio will go down?
                                                 h3("Downside risk", align="left",
                                                    style = "font-family: 'times'; font-si16pt"),
                                                 tableOutput("DownsideRisk_table")
                                          ), # end of column
                                          
                                          column(7,
                                                 # The second output shows the biggest 5 periods of the drawdowns
                                                 # from when to when did the prtfolio perform the weakest.
                                                 h3("Drawdowns (daily)", align="left",
                                                    style = "font-family: 'times'; font-si16pt"),
                                                 tableOutput("Drawdowns_table")
                                          ), # end of column
                                          
                                          column(12,
                                                 hr(),
                                                 br(),br(),
                                                 
                                                 
                                                 # The snail trail plot shows the past of annualized risk and returns.
                                                 # The amount of steps to look back can be modified with the numeric input.
                                                 # The best description what a snail trail plot is can be found below
                                                 #  in the h4 text.
                                                 
                                                 h3("Snail trail plot",
                                                    style = "font-family: 'times'; font-si16pt"),
                                                 h4("This graphic shows the developent of the index. The returns are compared
                                                      against the risk, over the chosen period of time. The history starts with the
                                                      light gray line, and gets darker as it gets closer to the last day.
                                                      Different historical lengths can be compared so that a general change in
                                                      risk can be noticed. The volatility is taken as the risk measurement. ",
                                                    style = "font-family: 'times'; font-si16pt"),
                                                 br(),
                                                 numericInput("num_of_days","Number of days",value = 200,step = 100),
                                                 plotOutput("Snail_Trail_Plot",height = 700)
                                          ),
                                          column(12,br(),br(),br(),br(),br(),br(),br(),br(),br(),br())
                                          
                                 ),
                                 
                                 
                                 
                                 #------------   RD Panel 5  -------------
                                 tabPanel("Weight analysis",
                                          
                                          # The weights analysis tab consists only of the displayal of the weights.
                                          # The first plot shows all the weights together.
                                          # Below the single indices can be chosen and analysed closely.
                                          
                                          # Analyse of all the weights.
                                          column(12,
                                                 h1("Analysis of the weights", align="left",
                                                    style = "font-family: 'times'; font-si16pt"),
                                                 h4("The weights of the indices used in the portfolio are shown.", align="left",
                                                    style = "font-family: 'times'; font-si16pt"),
                                                 
                                                 h3("Weight",style = "font-family: 'times'; font-si16pt"),
                                                 h4("This graphic shows the development of the weights over time.
                                                    The rolling period for the weights is 252 days."
                                                    ,style = "font-family: 'times'; font-si16pt"),
                                                 
                                                 plotOutput("weights_plot2"),
                                                 br(),
                                                 column(6,
                                                        verbatimTextOutput("properties_weights")
                                                 )
                                          ),
                                          
                                          
                                          
                                          # Single analysation of the weights.
                                          column(12,hr(),br(),br(),br(),br(),
                                                 h1("Single analysation of the weights",
                                                    style = "font-family: 'times'; font-si16pt")
                                          ),
                                          
                                          column(3,uiOutput("radiobuttons_weights")),
                                          
                                          column(9,
                                                 h3("Weights of the chosen index", style = "font-family: 'times'; font-si16pt"),
                                                 h4("The following plot shows the weights of the indices over time. The blue
                                                      background shows whether the index was bought or sold at this point of time.",
                                                    style = "font-family: 'times'; font-si16pt"),
                                                 checkboxInput("sell.buy", "show buy/sell backgound",value = T),
                                                 plotOutput("show_single_weights"),
                                                 column(10,offset = 1,uiOutput("slider_Date_weights")),
                                                 br(),
                                                 
                                                 column(12,
                                                        h3("Zoom plot", style = "font-family: 'times'; font-si16pt"),
                                                        h4("In this graphic, it is possible to take a close look at events from the
                                               upper graphic. The selected section will be shown as a zoom.",
                                                           style = "font-family: 'times'; font-si16pt")
                                                 ),
                                                 column(3,
                                                        br(),br(),br(),br(),
                                                        uiOutput("width_date_weights"),
                                                        br(),br(),
                                                        h4("Use this height line to easily measure specific points in the zoomed
                                                            plot.", style = "font-family: 'times'; font-si16pt"),
                                                        uiOutput("height_of_zoomed_Volume_weights")
                                                 ),
                                                 column(9, plotOutput("zoom_raw_data_portfolio_weights"))
                                          )
                                          
                                 ),# end of tabPanel "Weight analysis"
                                 
                                 
                                 
                                 
                                 #------------   RD Panel 6  -------------
                                 tabPanel("VaR and ES",
                                          
                                          # The first plot shows the Value at Risk and the Expected Shortfall of the portfolio 
                                          #   calculations were running over all the data, which means, that possible maverics can
                                          #   influence the overall VaR.
                                          # For a precicer analyzation we need to have an rolling calculatino.
                                          
                                          h1("Plots of the Value at Risk and Expected Shortfall", align="left",
                                             style = "font-family: 'times'; font-si16pt"),
                                          h4("This tab analyses the Value at Risk and the Expected Shortfall.", align="left",
                                             style = "font-family: 'times'; font-si16pt"),
                                          
                                          column(12,
                                                 h3("VaR of the portfolio", align="left",
                                                    style = "font-family: 'times'; font-si16pt"),
                                                 h4("This graphic is a simplified version of the lower graphic.
                                          It showes the Value at Risk (VaR) for the chosen percentile.
                                          The blue number is the VaR for the complete period of time."
                                                    ,style = "font-family: 'times'; font-si16pt"),
                                                 
                                                 
                                                 
                                                 numericInput("Var_number",label = "Enter percentile for VaR",
                                                              value = 0.05,step = 0.01),
                                                 plotOutput("VaR_chart")
                                          ),
                                          column(12,hr(),br(),br(),br(),br()),
                                          
                                          
                                          # The missing rolling funcitno is inplemented below the first Plot.
                                          # It take very long to calculate therefore the calculations only start by clicking on
                                          #  the checkbox.
                                          # the Parameters of the Plot can be modified with the numeric input.
                                          #  preferably before clicking on the checkbox.
                                          column(12,
                                                 h3("Expected shortfall", align="left",
                                                    style = "font-family: 'times'; font-si16pt"),
                                                 h4("In this chart, Expected Shortfall is shown more precisely than in the
                                                      graphic above. This graphic shows the probability of the strategy loosing a
                                                      certain percentage for the chosen period of time. 
                                                      This is the expected loss during the chosen period."
                                                    ,style = "font-family: 'times'; font-si16pt"),
                                                 checkboxInput("VaR_and_ES", "Show the advanced plot for ES", 
                                                               value = FALSE),
                                                 helpText("The calculation of the advanced plot can take a while.
                                                            The higher the width for the VaR, the longer it takes.",
                                                          style = "font-family: 'times'; font-si16pt"),
                                                 br(),
                                                 numericInput("Var_number_2",label = "Enter percentile for VaR",
                                                              value = 0.05,step = 0.01),
                                                 numericInput("Var_days",label = "Enter width for VaR (in days)",
                                                              value = 252,step = 1),
                                                 plotOutput("BarVaR_chart")
                                          )
                                          
                                 ) # end of tabPanel "VaR and ES"	
                                 
                     ) # end of tabsetpanel 
                     ,width=10) # end of mainPanel
                 ) # end of sidebarLayout
               ) # end of fluidPage
               
             ), # end of tabPanel
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             tabPanel("Advanced analysis",
                      
                      fluidPage(
                        headerPanel(h1("Advanced analysis",style = "font-family: 'times'; font-si16pt"), windowTitle = "Strategy"),
                        hr(),br(),br(),
                        
                        
                        ############# Main Panel #############
                        mainPanel(
                          tabsetPanel(type="tab", # Tabs are generated in this environment
                                      
                                      
                                      
                                      
                                      
                                      
                                      #------------   RD Panel 7  -------------
                                      tabPanel("Portfolio comparison",
                                               
                                               h1("Comparison between the different portfolio strategies",
                                                  style = "font-family: 'times'; font-si16pt"),
                                               h4("This tab compares all portfolio strategies.",
                                                  style = "font-family: 'times'; font-si16pt"),
                                               
                                               sidebarLayout(
                                                 sidebarPanel(
                                                   uiOutput("portfolio_comparison")
                                                   ,width=3), # End of sidebar Panel
                                                 
                                                 mainPanel(
                                                   # In this tab we display all the portfolios thogether.
                                                   # They can be compared and analysed with each other.
                                                   
                                                   # The first plot shows all the portoflios together in one graphic.
                                                   h3("Plot the returns of the portfolios", align="left",
                                                      style = "font-family: 'times'; font-si16pt"),
                                                   h4("The returns of all portfolios are plotted in one graphic. The behavior and
                                                      performance of the different portfolios can be analysed here.", align="left",
                                                      style = "font-family: 'times'; font-si16pt"),
                                                   plotOutput("Portfolio_comparison"),
                                                   
                                                   
                                                   # The second and the third plot display the correlation of all the portfolios
                                                   #  with each other.
                                                   column(12,hr(),br(),br(),br()),
                                                   h3("Correlation",style = "font-family: 'times'; font-si16pt"),
                                                   h4("Below is a visual representation of the correlations between the log-returns.",
                                                      style = "font-family: 'times'; font-si16pt"),
                                                   br(),
                                                   h4("Left graphic: This graphic shows a matrix of correlations between
                                                      the portfolio strategies. The bigger the dot, the more correlated the
                                                      two strategies.", style = "font-family: 'times'; font-si16pt"),
                                                   h4("Right graphic: This graphic shows the distributions and the correlations
                                                      for the different strategies in realtion to each other. The numbers symbolize the
                                                      correlation between two portfolios, the bigger the size, the higher the
                                                      correlation.",style = "font-family: 'times'; font-si16pt"),
                                                   
                                                   column(4,
                                                          h3("Correlation between the portfolios", align="left",
                                                             style = "font-family: 'times'; font-si16pt"),
                                                          plotOutput("corr_strategy")
                                                   ),
                                                   column(8,
                                                          h3("Correlation between the portfolios", align="left",
                                                             style = "font-family: 'times'; font-si16pt"),
                                                          plotOutput("corr_strategy_second")
                                                   ),
                                                   column(12,hr(),br(),br(),br(),br(),br()),
                                                   column(12,
                                                          h3("Table output", align="left",
                                                             style = "font-family: 'times'; font-si16pt"),
                                                          h4("Comaprison of the annulaized returns and volatility", align="left",
                                                             style = "font-family: 'times'; font-si16pt"),
                                                          tableOutput("Annualized_Returns_table_comparison"),
                                                          h4("Comaprison of the drawdowns", align="left",
                                                             style = "font-family: 'times'; font-si16pt"),
                                                          tableOutput("DownsideRisk_table_comparison")
                                                   )
                                                   ,width=9)
                                               )
                                               
                                      ), # end of tabPanel "Portfolio Comparison"
                                      
                                      
                                      
                                      #------------   RD Panel 8  -------------
                                      tabPanel("Benchmark analysis",
                                               
                                               # In this tab we will compare our portoflio to a benchmark.
                                               # The first plot shows the difference between the portfolio and the benchmark,
                                               #  to see if it performed better or worse than.
                                               
                                               h1("Benchmark analysis",style = "font-family: 'times'; font-si16pt"),
                                               h4("The portfolio strategy is compared to the benchmark (the benchmark is set as equal
                                                    weights portfolio).",
                                                  style = "font-family: 'times'; font-si16pt"),
                                               
                                               h3("Relative performance of the strategy in comparison to the benchmark",
                                                  style = "font-family: 'times'; font-si16pt"),
                                               h4("This graph shows how the relative performance of the strategy behaves
                                                    in comparison to the benchmark.",style = "font-family: 'times'; font-si16pt"),
                                               h4("The equal weights portfolio is the default setting as the benchmark",
                                                  style = "font-family: 'times'; font-si16pt"),
                                               plotOutput("RelativePerformance"),
                                               
                                               
                                               # The second graphic shows both.
                                               
                                               h3("Strategy returns compared to the benchmark",
                                                  style = "font-family: 'times'; font-si16pt"),
                                               h4("In this graphic the strategy returns and the benchmark returns are compared.",
                                                  style = "font-family: 'times'; font-si16pt"),
                                               h4("The equal weights portfolio is set as benchmark",
                                                  style = "font-family: 'times'; font-si16pt"),
                                               plotOutput("ReturnComparison"),
                                               
                                               
                                               # In the following graphic we can how the benchmark performes compared to all the raw data.
                                               
                                               h3("Returns of the indices compared to the benchmark",
                                                  style = "font-family: 'times'; font-si16pt"),
                                               h4("In this graphic the returns of the indices and the benchmark returns
                                                    are compared.",style = "font-family: 'times'; font-si16pt"),
                                               h4("The equal weights portfolio is set as benchmark",
                                                  style = "font-family: 'times'; font-si16pt"),
                                               plotOutput("BenchmarkAndRaw"),
                                               
                                               
                                               # The capture ratio plot shows the comparison of the downside caputre to the
                                               #  upside caputre.
                                               
                                               h3("Capture ratios",style = "font-family: 'times'; font-si16pt"),
                                               h4("The capture ratio is the statistical measure of an investment manager's overall
                                                    performance. The capture ratio is used to evaluate how well an investment manager
                                                    performed relative to an index during periods when that index has risen or fallen.
                                                    The ratio is calculated by dividing the manager's returns by the returns of the
                                                    index, and multiplying that factor by 100.",
                                                  style = "font-family: 'times'; font-si16pt"),
                                               h4("This graph shows how the capture ratio of the chosen portfolio (red) is in upside
                                                    and downside movements. If the strategy is beneath the diagonal line it means that
                                                    it performs better during downward trend than upward trend periods, compared to
                                                    the benchmark (blue). If it is located above the diagonal line it means the opposite.",
                                                  style = "font-family: 'times'; font-si16pt"),
                                               plotOutput("CaputreRatios_benchmark")
                                               
                                               
                                      ), # end of tabPanel "Benchmark analysis"
                                      
                                      
                                      
                                      #------------   RD Panel 9  -------------
                                      tabPanel("Correlation analysis",
                                               
                                               
                                               # The Correlation tab displays many different ways to compare the portfolio
                                               #  to the Benchmark.
                                               
                                               h1("Correlation analysis of the portfolio strategy",align="left",
                                                  style = "font-family: 'times'; font-si16pt"),
                                               h4("This tab analyses the correlation between portfolio and the single indices."
                                                  ,style = "font-family: 'times'; font-si16pt"),
                                               
                                               # The rolling correlation displays how the correlations developes over time.
                                               hr(),
                                               h3("Rolling correlation",style = "font-family: 'times'; font-si16pt"),
                                               h4("The graphic below shows the rolling correlation of the portfolio returns and 
                                                    the benchmark over a chosen number of days. 252 days are set as the standard value."
                                                  ,style = "font-family: 'times'; font-si16pt"),
                                               plotOutput("roll_corr_benchmark"),
                                               sliderInput("roll_corr_days",
                                                           "Days for rolling correlation :",
                                                           min = 2,  max = 504, value = 252, step=4),
                                               hr(),
                                               
                                               # The correlatino plot is for a good visualisation of the correlation.
                                               h3("Correlation plot of the benchmark returns vs. strategy returns",align="left",
                                                  style = "font-family: 'times'; font-si16pt"),
                                               h4("Top left panel: Distribution function of the benchmark.",
                                                  style = "font-family: 'times'; font-si16pt"),
                                               h4("Top right panel: Correlation value between benchmark and strategy.",
                                                  style = "font-family: 'times'; font-si16pt"),
                                               h4("Buttom left panel: Correlation plot (see previous graphic for more detail.)",
                                                  style = "font-family: 'times'; font-si16pt"),
                                               h4("Buttom right panel: Distribution function of the strategy.",
                                                  style = "font-family: 'times'; font-si16pt"),
                                               
                                               plotOutput("corr_ret_strategy"),
                                               hr()
                                               
                                      ) # end of tabPanel "Correlation analysis"
                                      
                                      
                                      
                                      
                                      
                          ) # end of tabsetPanel
                          ,width=12) # end of main panel
                      ) # end of fluidPage
             ), # end tabPanel "Advanced analysis"
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             tabPanel("Analyse raw data",
                      fluidPage(
                        headerPanel(h1("Analyse raw data",style = "font-family: 'times'; font-si16pt"), windowTitle = "strategy"),
                        hr(),br(),br(),
                        
                        ############# Main Panel #############
                        mainPanel(
                          tabsetPanel(type="tab", # Tabs are generated in this environment.
                                      
                                      
                                      #------------   RD Panel 10  -------------
                                      tabPanel("Single index analysis",
                                               
                                               # The single index analysis diaplys only one index at a time
                                               # If there are doubts about how a index perfomes or it's characterisitcs
                                               #  they can be analysed in this tab
                                               
                                               h1("Display of the raw data",style = "font-family: 'times'; font-si16pt"),
                                               h4("One single index can be analysed in this tab.",
                                                  style = "font-family: 'times'; font-si16pt"),
                                               
                                               br(),
                                               
                                               # Which index should be analysed?
                                               sidebarPanel(
                                                 radioButtons("chosenRawData", label = h4("Choose ticker"), 
                                                              c("FTSE100" = "/FTSE100_1984-05-04.csv", 
                                                                "Nikkei225" = "/Nikkei225_1990-09-26.csv",
                                                                "SP500" = "/SP500_1982-04-22.csv",
                                                                "US_2Y" = "/US_2Y_Note_1990-06-25.csv",
                                                                "US_5Y" = "/US_5Y_Note_1988-05-23.csv",
                                                                "US_10Y" = "/US_10Y_1982-05-04.csv",
                                                                "US_30Y" = "/US_30Y_1977-08-23.csv",
                                                                "Long_Gilt" = "/Long_Gilt_1990-08-20.csv",
                                                                "AUD" = "/AUD_1987-01-14.csv",
                                                                "CHF" = "/CHF_1975-02-14.csv",
                                                                "GBD" = "/GBP_1975-02-14.csv",
                                                                "Copper" = "/Copper_1959-07-06.csv",
                                                                "Cotton" = "/Cotton_1972-08-23.csv",
                                                                "Brent_Crude" = "/Brent_Crude_1993-03-18.csv",
                                                                "Heating Oil" = "/Heating_Oil_1979-03-07.csv",
                                                                "Natural_Gas" = "/Natural_Gas_1990-04-04.csv",
                                                                "Gold" = "/Gold_1975-01-02.csv",
                                                                "Platinum" = "/Platinum_1969-01-03.csv"
                                                              )
                                                 ),
                                                 width=2
                                               ),
                                               
                                               
                                               
                                               
                                               
                                               
                                               # The first graph shows the volume of the index.
                                               # Below is the function for a zoom plot implemented,
                                               #  the same way as in weights analyzation.
                                               column(10,
                                                      h3("Volume of the chosen index",
                                                         style = "font-family: 'times'; font-si16pt"),
                                                      plotOutput("show_raw_data"),
                                                      column(10,offset = 1,uiOutput("slider_Date")),
                                                      br(),
                                                      
                                                      column(12,
                                                             h3("Zoom plot",
                                                                style = "font-family: 'times'; font-si16pt"),
                                                             h4("In this graphic, it is possible to take a close look at events from the
                                                                upper graphic. The selected section will be shown as a zoom.",
                                                                style = "font-family: 'times'; font-si16pt")
                                                      ),
                                                      column(3,
                                                             br(),br(),br(),br(),
                                                             numericInput("width_date","Width of the zoom",1000,step = 100),
                                                             br(),br(),
                                                             h4("Use this height line to easily measure specific points in the zoomed
                                                                plot.", style = "font-family: 'times'; font-si16pt"),
                                                             uiOutput("height_of_zoomed_Raw_Data")
                                                      ),
                                                      column(9, plotOutput("show_raw_data_zoom"))
                                               ),
                                               
                                               # A quik overview of the index is shown.
                                               # Displayed is the cumulated log returns, the raw returns and the draw downs.
                                               column(12,
                                                      hr(),br(),br(),
                                                      h3("Return analysis plots",
                                                         style = "font-family: 'times'; font-si16pt"),
                                                      h4("This graph shows the cummulated returns of the index on top.
                                                         In the middle plot, the daily returns are shown. Furthermore,
                                                         in the last graph the daily drawdown is deployed.",
                                                         style = "font-family: 'times'; font-si16pt"),
                                                      plotOutput("show_raw_returns",height = 800),
                                                      br(),br(),
                                                      h3("Histogram of the raw data",
                                                         style = "font-family: 'times'; font-si16pt"),
                                                      h4("The histogram show how the returns of the indices are distributed",
                                                         style = "font-family: 'times'; font-si16pt"),
                                                      plotOutput("histogramm_RAW"),
                                                      hr(),br(),br()
                                                      
                                               ),
                                               column(12,br(),br(),br(),br(),br(),br(),br(),br(),br(),br()),
                                               
                                               
                                               
                                               # This panel analyses the risk of the portfolio.
                                               column(12,
                                                      h2("Analysis of the risk", align="center",
                                                         style = "font-family: 'times'; font-si16pt"),
                                                      hr(),
                                                      h4("The risk analysis page shows the downside risk, drawdowns and the behavior of
                                                           risk compared to the strategy returns.", align="left",
                                                         style = "font-family: 'times'; font-si16pt")
                                               ),
                                               
                                               column(1),
                                               column(4,
                                                      # The first table shows the downside risk.
                                                      # How porbable is it, that the portfolio will go down?
                                                      h3("Downside risk", align="left",
                                                         style = "font-family: 'times'; font-si16pt"),
                                                      tableOutput("DownsideRisk_table_RAW")
                                               ), # end of column
                                               
                                               column(7,
                                                      # The second output shows the biggest 5 periods of the drawdowns
                                                      #   from when to when did the prtfolio perform the weakest.
                                                      h3("Drawdowns (daily)", align="left",
                                                         style = "font-family: 'times'; font-si16pt"),
                                                      tableOutput("Drawdowns_table_RAW")
                                               ), # end of column
                                               
                                               
                                               column(12,br(),br(),br(),br()),
                                               
                                               
                                               # The performance table show the numerical feedback to the portfolio.
                                               column(12,
                                                      h2("Performance tables of the raw data", align="center",
                                                         style = "font-family: 'times'; font-si16pt"),
                                                      hr(),
                                                      h4("In this tab, all the performances of the chosen portfolio is shown.", 
                                                         align="left",
                                                         style = "font-family: 'times'; font-si16pt")
                                               ),
                                               fluidRow(
                                                 column(4,
                                                        # The annualized portfolio returns show how it
                                                        #   performed over the selected time.
                                                        h3("Annualized portfolio returns", align="left",
                                                           style = "font-family: 'times'; font-si16pt"),
                                                        tableOutput("Annualized_Returns_table_RAW")
                                                 ), # end of column
                                                 column(12,
                                                        # trailing peridos show the performance in the last time periods.
                                                        h3("Trailing periods", align="left",
                                                           style = "font-family: 'times'; font-si16pt"),
                                                        tableOutput("TrailingPeriods_table_RAW")
                                                 ) # end of column
                                               ), # end of fluidRow
                                               fluidRow(
                                                 # the calendart returns show the performance in specific months.
                                                 h3("Calendar returns", align="left",style = "font-family: 'times'; font-si16pt"),
                                                 tableOutput("CalendarReturns_table_RAW")
                                               ) # end of fluidRow
                                               
                                      ), # end of tabPanel "Test Environment"
                                      
                                      
                                      tabPanel("Comparison of all indices",
                                               
                                               # Comparison of all indices was built to get a good overvie over
                                               #  all the indices compared to each other.
                                               # It can take a while until all the plots are generated, because there is a
                                               #  huge amount of data to be analyszed.
                                               
                                               helpText("this tab my take some seconds to load"),
                                               h1("Comparison of all indices",style = "font-family: 'times'; font-si16pt"),
                                               h4("All indices can be compared to each other.",
                                                  style = "font-family: 'times'; font-si16pt"),
                                               
                                               br(),
                                               
                                               
                                               # All the RAW data is ploted in one graphic to  see wich one performed the best
                                               #  and which one performed the worst.
                                               
                                               h3("Comparison of the raw data",
                                                  style = "font-family: 'times'; font-si16pt"),
                                               h4("In the following plot, all the raw data inputs are plotted in one graphic.
                                                  It is useful to be able to see which indices have which influence on the
                                                  overall portfolio.",
                                                  style = "font-family: 'times'; font-si16pt"),
                                               plotOutput("RawComparison",height = 500),
                                               hr(),br(),br(),
                                               
                                               
                                               # We take the distribution plot to see how big the distributino of the
                                               #  indexes is compared to each other.
                                               
                                               h3("Distribution of the returns comparison",
                                                  style = "font-family: 'times'; font-si16pt"),
                                               h4("The distribution of the indix returns are deployed in a boxplot.
                                                  The distribution can be compared. The wider the spread, the more volatile the index.
                                                  The black dots on the side represent the
                                                  statistical outlayers. The graphic is ordered by decreasing volatility.",
                                                  style = "font-family: 'times'; font-si16pt"),
                                               plotOutput("chart_Boxplots",height = 500),
                                               
                                               h3("Correlation plot",
                                                  style = "font-family: 'times'; font-si16pt"),
                                               h4("The correlation plot shows the correlations between all the chosen raw data",
                                                  style = "font-family: 'times'; font-si16pt"),
                                               helpText("The plot will take approximately 20 seconds to load"),
                                               checkboxInput("Corr_plot_raw",label = "show correlation plot"),
                                               column(10,plotOutput("corr_raw_data_text1")),
                                               column(10,plotOutput("corr_raw_data", height = 900)),
                                               column(2,plotOutput("corr_raw_data_text2", height = 900))
                                      )
                                      
                                      
                                      
                                      
                                      
                          ) # end of tabsetPanel
                          ,width=12)#end of main panel
                      ) # end of fluidPage      
             ) # end tabPanel "Analyse Raw data"
  ) # end navbarPage "Overview diversification strategies"
) # end of shinyUI

