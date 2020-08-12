##########################################################################.
##########################################################################.
#
# This File contains all the code which was used from external sources.
# 
##########################################################################.
##########################################################################.




################## Code from Dr. Osterrieder ##################.
################## ZHAW School of Applied Science #############.

compute_rolling_sd = function(returns, 
                              annualize=TRUE, 
                              window_length=300, 
                              min_annual_vola = 0.01,
                              dates = NA # explicitly pass in dates, otherwise use index of returns)
)
{
  flog.info("computing rolling sd, with special treatment of NAs, forward and backward filled!!")
  rolling_sd = rollapply(returns, 
                         window_length, 
                         sd, 
                         align="right",
                         fill=NA)
  #length(rolling_sd )
  # do not remove leading NAs
  flog.info("finished rolling sd")
  rolling_sd = na.locf(rolling_sd, na.rm=FALSE) 
  flog.info("finished rolling sd rm NA")
  rolling_sd = na.locf(rolling_sd, fromLast=TRUE, na.rm=FALSE) 
  flog.info("finished rolling sd fromLast")
  
  rolling_sd = pmax(min_annual_vola/sqrt(252), rolling_sd)
  flog.info("finished rolling sd min vola")
  
  if(annualize){
    rolling_sd = rolling_sd * sqrt(252)
  }
  
  flog.info("finished computing rolling sd, with special treatment of NAs, forward and backward filled!!")
  if(is.na(dates[1]))
  {
    dates = index(returns)
  }
  rolling_sd = as.xts(rolling_sd, order.by=dates)
  return(rolling_sd)	
}



chart.multiRollingCorrelation <- #This function is originally provided from PerformanceAnalysis
  function (Ra, Rb, width = 12, xaxis = TRUE, #just some litle changes werde made to make it more flexible
            legend.loc = NULL, colorset = (1:12), 
            ylim= c(-1,1), ...)
  { # @author Peter Carl
    # DESCRIPTION:
    # A wrapper to create a chart of rolling correlation metrics in a line chart
    # FUNCTION:
    # Transform input data to a matrix
    Ra = checkData(Ra)
    Rb = checkData(Rb)
    # Get dimensions and labels
    columns.a = ncol(Ra)
    columns.b = ncol(Rb)
    columnnames.a = colnames(Ra)
    columnnames.b = colnames(Rb)
    # Calculate
    for(column.a in 1:columns.a) { # for each asset passed in as R
      for(column.b in 1:columns.b) { # against each asset passed in as Rb
        merged.assets = merge(Ra[,column.a,drop=FALSE], Rb[,column.b,drop=FALSE])
        column.calc = rollapply(na.omit(merged.assets[,,drop=FALSE]), 
                                width = width, 
                                FUN= function(x) cor(x[,1,drop=FALSE], x[,2,drop=FALSE]), by = 1, by.column = FALSE, align = "right")
        
        # some backflips to name the single column zoo object
        column.calc.tmp = xts(column.calc)
        colnames(column.calc.tmp) = paste(columnnames.a[column.a], columnnames.b[column.b], sep = " to ")
        column.calc = xts(column.calc.tmp, order.by = time(column.calc))
        if(column.a == 1 & column.b == 1)
          Result.calc = column.calc
        else
          Result.calc = merge(Result.calc, column.calc)
      }
    }
    print("plotting multiple correlations")
    chart.TimeSeries(Result.calc, xaxis = xaxis, 
                     #			colorset = colorset, 
                     legend.loc = legend.loc, 
                     ylim = ylim, ...)
  }


plot_correlation_matrix=function(returns, 
                                 sig.level=1, # set to 1 if you want to disable it 
                                 mixed=FALSE, # plot mixed matrix, upper triangle with circles, lower one with numbers
                                 method="number", # choose from number or circle. more available
                                 order="original" # choose from original (i.e. unchanged) or e.g. hclust
){
  
  M = cor(returns)
  ## specialize the insignificant value according to the significant level
  
  col1 <- colorRampPalette(c("#7F0000", "red", "#FF7F00", "yellow", "white", "cyan", 
                             "#007FFF", "blue", "#00007F"))
  # plot mixed matrix, upper triangle with circles, lower one with numbers
  if(mixed){
    if(sig.level == 1){
      corrplot.mixed(M,
                     order = order, 
                     addrect = 5,
                     col=col1(100)
      )
    }
    else{
      res <- cor.mtest(returns, 0.95)
      corrplot.mixed(M, p.mat = res[[1]], sig.level = sig.level,
                     order = order, addrect = 5,insig="blank",
                     col=col1(100))
    }
  }
  else{
    if(sig.level == 1){
      corrplot(M, 
               order = order, addrect = 5,
               col=col1(100),type="upper")
    }
    else{
      res <- cor.mtest(returns, 0.95)
      corrplot(M, p.mat = res[[1]], sig.level = sig.level,
               order = order, addrect = 5,insig="blank",
               col=col1(100),type="upper")
    }
  }
}













################## Code from the Internet ##################.
## Website:
# http://masterr.org
## direct Link:
# http://faculty.washington.edu/ezivot/econ424/portfolio.r



# portfolio.r
# 
# Functions for portfolio analysis
# to be used in Introduction to Computational Finance & Financial Econometrics
# last updated: August 11, 2011 by Eric Zivot
#
# Functions:
#	1. efficient.portfolio			compute minimum variance portfolio
#							                subject to target return
#	2. globalMin.portfolio			compute global minimum variance portfolio
#	3. tangency.portfolio			  compute tangency portfolio
#	4. efficient.frontier			  compute Markowitz bullet
#	5. getPortfolio					    create portfolio object
#

getPortfolio <- function(er, cov.mat, weights) {
  # contruct portfolio object
  #
  # inputs:
  # er				   N x 1 vector of expected returns
  # cov.mat  		 N x N covariance matrix of returns
  # weights			 N x 1 vector of portfolio weights
  #
  # output is portfolio object with the following elements
  # call				original function call
  # er				  portfolio expected return
  # sd				  portfolio standard deviation
  # weights			N x 1 vector of portfolio weights
  #
  call <- match.call()
  
  #
  # check for valid inputs
  #
  asset.names <- names(er)
  weights <- as.vector(weights)
  names(weights) = names(er)
  er <- as.vector(er)					# assign names if none exist
  if(length(er) != length(weights))
    stop("dimensions of er and weights do not match")
  cov.mat <- as.matrix(cov.mat)
  if(length(er) != nrow(cov.mat))
    stop("dimensions of er and cov.mat do not match")
  if(any(diag(chol(cov.mat)) <= 0))
    stop("Covariance matrix not positive definite")
  
  #
  # create portfolio
  #
  er.port <- crossprod(er,weights)
  sd.port <- sqrt(weights %*% cov.mat %*% weights)
  ans <- list("call" = call,
              "er" = as.vector(er.port),
              "sd" = as.vector(sd.port),
              "weights" = weights) 
  class(ans) <- "portfolio"
  ans
}

efficient.portfolio <- function(er, cov.mat, target.return) {
  # compute minimum variance portfolio subject to target return
  #
  # inputs:
  # er					    N x 1 vector of expected returns
  # cov.mat  			  N x N covariance matrix of returns
  # target.return	  scalar, target expected return
  #
  # output is portfolio object with the following elements
  # call				    original function call
  # er					    portfolio expected return
  # sd					    portfolio standard deviation
  # weights			    N x 1 vector of portfolio weights
  #
  call <- match.call()
  
  #
  # check for valid inputs
  #
  asset.names <- names(er)
  er <- as.vector(er)					# assign names if none exist
  cov.mat <- as.matrix(cov.mat)
  if(length(er) != nrow(cov.mat))
    stop("invalid inputs")
  if(any(diag(chol(cov.mat)) <= 0))
    stop("Covariance matrix not positive definite")
  # remark: could use generalized inverse if cov.mat is positive semidefinite
  
  #
  # compute efficient portfolio
  # Values for demonstration:
  ## er <- c(0.3,0.2)
  ## cov.mat <- matrix(c(1,0.4,0.4,1),ncol=2, byrow=T)
  ## target.return <- 0.1
  ones <- rep(1, length(er)) # generates vector with length of er filled with ones
  top <- cbind(2*cov.mat, er, ones) # binds matrix with the vec. er and ones on the right side
  bot <- cbind(rbind(er, ones), matrix(0,2,2)) # tilts top matrix and copies same sice with 0 on the right
  A <- rbind(top, bot) # creates matrix with er and ones vectors on the right side and the bottom
  b.target <- as.matrix(c(rep(0, length(er)), target.return, 1)) # binding vectors from top
  x <- solve(A, b.target) # solving the matrix with the target vola as right side (b)
  w <- x[1:length(er)] # "weights"
  names(w) <- asset.names
  
  #
  # compute portfolio expected returns and variance
  #
  er.port <- crossprod(er,w)
  sd.port <- sqrt(w %*% cov.mat %*% w)
  ans <- list("call" = call,
              "er" = as.vector(er.port),
              "sd" = as.vector(sd.port),
              "weights" = w) 
  class(ans) <- "portfolio"
  ans
}

globalMin.portfolio <- function(er, cov.mat) {
  # Compute global minimum variance portfolio
  #
  # inputs:
  # er				N x 1 vector of expected returns
  # cov.mat		N x N return covariance matrix
  #
  # output is portfolio object with the following elements
  # call			original function call
  # er				portfolio expected return
  # sd				portfolio standard deviation
  # weights		N x 1 vector of portfolio weights
  call <- match.call()
  
  #
  # check for valid inputs
  #
  asset.names <- names(er)
  er <- as.vector(er)					# assign names if none exist
  cov.mat <- as.matrix(cov.mat)
  if(length(er) != nrow(cov.mat))
    stop("invalid inputs")
  if(any(diag(chol(cov.mat)) <= 0))
    stop("Covariance matrix not positive definite")
  # remark: could use generalized inverse if cov.mat is positive semi-definite
  
  #
  # compute global minimum portfolio
  # Analytic version of MV CAPM minimum vola
  cov.mat.inv <- solve(cov.mat) # solving the covariance matrix
  one.vec <- rep(1,length(er))
  #  w.gmin <- cov.mat.inv %*% one.vec/as.vector(one.vec %*% cov.mat.inv %*% one.vec)
  w.gmin <- rowSums(cov.mat.inv) / sum(cov.mat.inv) # serches global minimum
  w.gmin <- as.vector(w.gmin)
  names(w.gmin) <- asset.names
  er.gmin <- crossprod(w.gmin,er) # calculation of the expected return of global minimum
  sd.gmin <- sqrt(t(w.gmin) %*% cov.mat %*% w.gmin)
  gmin.port <- list("call" = call,
                    "er" = as.vector(er.gmin),
                    "sd" = as.vector(sd.gmin),
                    "weights" = w.gmin)
  class(gmin.port) <- "portfolio"
  gmin.port
}

tangency.portfolio <- function(er,cov.mat,risk.free) {
  # compute tangency portfolio
  #
  # inputs:
  # er				   N x 1 vector of expected returns
  # cov.mat		   N x N return covariance matrix
  # risk.free		 scalar, risk-free rate
  #
  # output is portfolio object with the following elements
  # call			  captures function call
  # er				  portfolio expected return
  # sd				  portfolio standard deviation
  # weights		 N x 1 vector of portfolio weights
  call <- match.call()
  
  #
  # check for valid inputs
  #
  asset.names <- names(er)
  # checksif risk free rate is biger than 0
  if(risk.free < 0)
    stop("Risk-free rate must be positive")
  er <- as.vector(er)
  cov.mat <- as.matrix(cov.mat)
  if(length(er) != nrow(cov.mat))
    stop("invalid inputs")
  if(any(diag(chol(cov.mat)) <= 0))
    stop("Covariance matrix not positive definite")
  # remark: could use generalized inverse if cov.mat is positive semi-definite
  
  #
  # compute global minimum variance portfolio
  #
  gmin.port <- globalMin.portfolio(er,cov.mat)
  if(gmin.port$er < risk.free)
    stop("Risk-free rate greater than avg return on global minimum variance portfolio")
  
  # 
  # compute tangency portfolio
  # risk.free <- .12
  cov.mat.inv <- solve(cov.mat)
  w.t <- cov.mat.inv %*% (er - risk.free) # tangency portfolio
  w.t <- as.vector(w.t/sum(w.t))	# normalize weights
  names(w.t) <- asset.names
  er.t <- crossprod(w.t,er) # expected returs of tnagency portfoli
  sd.t <- sqrt(t(w.t) %*% cov.mat %*% w.t)
  # setting together list for output
  tan.port <- list("call" = call,
                   "er" = as.vector(er.t),
                   "sd" = as.vector(sd.t),
                   "weights" = w.t)
  class(tan.port) <- "portfolio"
  tan.port
}

efficient.frontier <- function(er, cov.mat, nport=20, alpha.min=-0.5, alpha.max=1.5) {
  # Compute efficient frontier with no short-sales constraints
  #
  # inputs:
  # er			  N x 1 vector of expected returns
  # cov.mat	  N x N return covariance matrix
  # nport		  scalar, number of efficient portfolios to compute
  #
  # output is a Markowitz object with the following elements
  # call		  captures function call
  # er			  nport x 1 vector of expected returns on efficient porfolios
  # sd			  nport x 1 vector of std deviations on efficient portfolios
  # weights 	nport x N matrix of weights on efficient portfolios 
  call <- match.call()
  
  #
  # check for valid inputs
  # values for test
  ## nport <- 1
  ## alpha.min <- 0.1
  ## alpha.max <- 0.8
  asset.names <- names(er) # name replacement
  er <- as.vector(er)
  cov.mat <- as.matrix(cov.mat)
  if(length(er) != nrow(cov.mat))
    stop("invalid inputs")
  if(any(diag(chol(cov.mat)) <= 0))
    stop("Covariance matrix not positive definite")
  
  #
  # create portfolio names
  #
  port.names <- rep("port",nport)
  ns <- seq(1,nport)
  port.names <- paste(port.names,ns)
  
  #
  # compute global minimum variance portfolio
  #
  cov.mat.inv <- solve(cov.mat) # inverse of the covarinace matrix
  one.vec <- rep(1,length(er))
  port.gmin <- globalMin.portfolio(er,cov.mat)
  w.gmin <- port.gmin$weights
  
  #
  # compute efficient frontier as convex combinations of two efficient portfolios
  # 1st efficient port: global min var portfolio
  # 2nd efficient port: min var port with ER = max of ER for all assets
  #
  er.max <- max(er)
  port.max <- efficient.portfolio(er,cov.mat,er.max) # generating the efficient portfolio with function
  w.max <- port.max$weights # max weight
  a <- seq(from=alpha.min,to=alpha.max,length=nport) # convex combinations
  we.mat <- a %o% w.gmin + (1-a) %o% w.max	# rows are efficient portfolios
  er.e <- we.mat %*% er	# expected returns of efficient portfolios
  er.e <- as.vector(er.e)
  names(er.e) <- port.names
  cov.e <- we.mat %*% cov.mat %*% t(we.mat) # cov mat of efficient portfolios
  sd.e <- sqrt(diag(cov.e))	# std devs of efficient portfolios
  sd.e <- as.vector(sd.e) # std devs of efficient portfolios
  names(sd.e) <- port.names # put names of portfolio in order
  dimnames(we.mat) <- list(port.names,asset.names)
  
  # 
  # summarize results
  # make list for output
  #
  ans <- list("call" = call,
              "er" = er.e,
              "sd" = sd.e,
              "weights" = we.mat)
  class(ans) <- "Markowitz"
  ans
}


# print method for portfolio object
print.portfolio <- function(x, ...) {
  cat("Call:\n")
  print(x$call, ...)
  cat("\nPortfolio expected return:    ", format(x$er, ...), "\n")
  cat("Portfolio standard deviation: ", format(x$sd, ...), "\n")
  cat("Portfolio weights:\n")
  print(round(x$weights,4), ...)
  invisible(x)
}


# summary method for portfolio object
summary.portfolio <- function(object, risk.free=NULL, ...) {
  cat("Call:\n")
  print(object$call)
  cat("\nPortfolio expected return:    ", format(object$er, ...), "\n")
  cat(  "Portfolio standard deviation: ", format(object$sd, ...), "\n")
  if(!is.null(risk.free)) {
    SharpeRatio <- (object$er - risk.free)/object$sd
    cat("Portfolio Sharpe Ratio:       ", format(SharpeRatio), "\n")
  }
  cat("Portfolio weights:\n")
  print(round(object$weights,4), ...)
  invisible(object)
}
# risk.free			risk-free rate. If not null then compute and print Sharpe ratio
# hard-coded 4 digits; prefer to let user control, via ... or other argument


# plot method for portfolio object
plot.portfolio <- function(object, ...) {
  asset.names <- names(object$weights)
  barplot(object$weights, names=asset.names,
          xlab="Assets", ylab="Weight", main="Portfolio Weights", ...)
  invisible()
}


# print method for Markowitz object
print.Markowitz <- function(x, ...) {
  cat("Call:\n")
  print(x$call)
  xx <- rbind(x$er,x$sd)
  dimnames(xx)[[1]] <- c("ER","SD")
  cat("\nFrontier portfolios' expected returns and standard deviations\n")
  print(round(xx,4), ...)
  invisible(x)
}
# hard-coded 4, should let user control


# summary method for Markowitz object
summary.Markowitz <- function(object, risk.free=NULL) {
  call <- object$call
  asset.names <- colnames(object$weights)
  port.names <- rownames(object$weights)
  if(!is.null(risk.free)) {
    # compute efficient portfolios with a risk-free asset
    nport <- length(object$er)
    sd.max <- object$sd[1]
    sd.e <- seq(from=0,to=sd.max,length=nport)	
    names(sd.e) <- port.names
    
    #
    # get original er and cov.mat data from call 
    er <- eval(object$call$er)
    cov.mat <- eval(object$call$cov.mat)
    
    #
    # compute tangency portfolio
    tan.port <- tangency.portfolio(er,cov.mat,risk.free)
    x.t <- sd.e/tan.port$sd		# weights in tangency port
    rf <- 1 - x.t			# weights in t-bills
    er.e <- risk.free + x.t*(tan.port$er - risk.free)
    names(er.e) <- port.names
    we.mat <- x.t %o% tan.port$weights	# rows are efficient portfolios
    dimnames(we.mat) <- list(port.names, asset.names)
    we.mat <- cbind(rf,we.mat) 
  }
  else {
    er.e <- object$er
    sd.e <- object$sd
    we.mat <- object$weights
  }
  ans <- list("call" = call,
              "er"=er.e,
              "sd"=sd.e,
              "weights"=we.mat)
  class(ans) <- "summary.Markowitz"	
  ans
}


print.summary.Markowitz <- function(x, ...) {
  xx <- rbind(x$er,x$sd)
  port.names <- names(x$er)
  asset.names <- colnames(x$weights)
  dimnames(xx)[[1]] <- c("ER","SD")
  cat("Frontier portfolios' expected returns and standard deviations\n")
  print(round(xx,4), ...)
  cat("\nPortfolio weights:\n")
  print(round(x$weights,4), ...)
  invisible(x)
}
# hard-coded 4, should let user control


# plot efficient frontier
#
# things to add: plot original assets with names
# tangency portfolio
# global min portfolio
# risk free asset and line connecting rf to tangency portfolio
plot.Markowitz <- function(object, plot.assets=FALSE, ...) {
  if (!plot.assets) {
    y.lim=c(0,max(object$er))
    x.lim=c(0,max(object$sd))
    plot(object$sd,object$er,type="b",xlim=x.lim, ylim=y.lim,
         xlab="Portfolio SD", ylab="Portfolio ER", 
         main="Efficient Frontier", ...)
  }
  else {
    call = object$call
    mu.vals = eval(call$er)
    sd.vals = sqrt( diag( eval(call$cov.mat) ) )
    y.lim = range(c(0,mu.vals,object$er))
    x.lim = range(c(0,sd.vals,object$sd))
    plot(object$sd,object$er,type="b", xlim=x.lim, ylim=y.lim,
         xlab="Portfolio SD", ylab="Portfolio ER", 
         main="Efficient Frontier", ...)
    text(sd.vals, mu.vals, labels=names(mu.vals))
  }
  invisible()
}
# plot.assets		logical. If true then plot asset sd and er





