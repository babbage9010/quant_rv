### rv_vs_rtns_0.0.2.R by babbage9010 and friends
# update code from v1 to produce plots for blog post: 
# https://babbage9010.wordpress.com/2023/09/11/there-is-a-low-vol-anomaly-in-spy/
# Not intuitive, but it works, like this: 
# 1) to reproduce the B&W plots (1 lookback period)
#   a) set "switchy" (Line 118) to FALSE
#   b) choose vol signal as x_dat (L 48-52)
#   c) use numcols (L 73) to choose 2 or 3 column plot
#   
# 2) to reproduce the color, lookback spectrum plots   
#   a) set "switchy" (L 118) to TRUE
#   b) choose vol signal as x_dat (L 178-183)
#   c) play with maximum lookback if you want (L 163)
#   d) to exactly duplicate Y scales, see notes (L 126-137)
#       and set them manually, e.g., (0.0,8.4)
#
### released under MIT License

# Step 1: Load libraries and data
library(quantmod)
library(PerformanceAnalytics)

start_date <- as.Date("2006-07-01") #SPY goes back to Jan 1993
end_date <- as.Date("2019-12-31")

getSymbols("SPY", src = "yahoo", from = start_date, to = end_date, auto.assign = FALSE) -> gspc_data
pricesAd <- na.omit( Ad(gspc_data) )
pricesOp <- na.omit( Op(gspc_data) )
pricesCl <- na.omit( Cl(gspc_data) )
pricesHi <- na.omit( Hi(gspc_data) )
pricesLo <- na.omit( Lo(gspc_data) )
# choose one of those here
trade_prices <- pricesOp
signal_prices <- pricesAd
bench_prices <- pricesAd


#plot it
roc1 <- ROC(signal_prices, n = 1, type = "continuous")
lookbk <- 20
rv <- runSD(roc1, n = lookbk) * sqrt(252)
rs   <- volatility(gspc_data, n=lookbk, calc="rogers.satchell")
gkyz <- volatility(gspc_data, n=lookbk, calc="gk.yz")
park <- volatility(gspc_data, n=lookbk, calc="parkinson")
avgvol <- (rv+rs+gkyz+park) / 4

#choose one of these to uncomment
x_dat  <- rv; x_dat_label = "C2C" 
#x_dat <- rs; x_dat_label = "RS" 
#x_dat <- gkyz; x_dat_label = "GKYZ" 
#x_dat <- park; x_dat_label = "Park" 
#x_dat <- avgvol; x_dat_label = "AVE4" 

vollabel = paste(x_dat_label," ",lookbk, "d vol",sep="")

#y - SPY open lagged returns
roc_trade1 <- ROC(trade_prices, n = 1, type = "continuous")
returns_spy_open <- roc_trade1 
returns_spy_open <- stats::lag(returns_spy_open, -2) #lag is -2 to properly align with vol signal 
# see v1 code for details
y_dat <- returns_spy_open #lagged, as above

#rid of NAs to even up the data (tip: this avoids NA-related errors)
dat <- as.xts(y_dat)
dat <- cbind(dat,x_dat)
dat <- na.omit(dat)
datcore <- coredata(dat)


# Set for six graphs, or nine
# to recreate the black&white plots in the blog post
numrows <- 3 
numcols <- 2 #either 2 or 3 columns please
# Set up 2x2 graphical window
par(mfrow = c(numrows, numcols))

#qnums <- c(10,30,100) #number of quantiles (buckets) (eg 10 for deciles)
qnums <- c(2,10,100) #number of quantiles (buckets) (eg 10 for deciles)
for(q in 1:3){
  qnum <- qnums[q] 
  xlabel = paste(vollabel," with ",qnum," vol buckets",sep="")
  decs <- unname(quantile(datcore[,2], probs = seq(1/qnum, 1-1/qnum, by = 1/qnum)))
  decs[qnum] <- max(decs) + 1
  decsmin <- min(decs) - 1
  #loop through volatility buckets to get summed and risked returns
  sums <- c()
  annvols <- c()
  riskdRtns <- c()
  for(i in 1:qnum){
    # datx = data segment from x_dat[,1] (returns) to summarize
    lowbound <- ifelse(i == 1, decsmin, decs[i-1])
    hibound <- decs[i]
    datx <- ifelse( datcore[,2] >= lowbound & datcore[,2] < hibound, datcore[,1], NA)
    datx <- na.omit(datx)
    sums[i] <- sum(datx)
    annvols[i] <- sd(datx) * sqrt(252)
    riskdRtns[i] <- sums[i] / annvols[i]
    #print( paste("decile",i,"mean:",means[i],"vol range:",lowval,"-",hival) )
  }
  barplot(sums,xlab=xlabel,ylab="Sum of SPY daily returns (log)",main="Sum of daily SPY log returns per volatility bucket",sub="low vol on left, high vol on right")
  if(numrows >= 3){
    #barplot(wins,xlab=xlabel,ylab="SPY mean daily return",main="Daily win % for SPY returns per vol bucket",sub="low vol on left, high vol on right")
    #abline(h=c(0.54),col="red")
    barplot(annvols,xlab=xlabel,ylab="SPY annualized standard deviation",main="Std Dev (ann) for SPY returns per vol bucket",sub="low vol on left, high vol on right")
    if(numcols >= 3){
      barplot(riskdRtns,xlab=xlabel,ylab="SPY risked returns (relative)",main="Relative risked returns per volatility bucket",sub="low vol on left, high vol on right")
    }
  }
}

#
# HERE BE THE COLOR BAR CHARTS
#
# Use this switch to turn on(T)/off(F) this section
#  for some (R Studio?) reason it also seems to turn off the earlier plots
#  so you only get one or the other plots based on this switch
#
switchy <- TRUE #FALSE OR TRUE

if(switchy){
  # function generates the bar graph from loop below
  barr = function(wha="net",whichdec,signalname,value=""){
    x <- lbaks
    if(wha=="net") { y <- decnets[whichdec,]
    titl <- paste(signalname,": net returns per quantile by lookback (Y scale varies)") 
    ystuff <- list(title = "net return for Vol bucket",  range = c(min(decnets[whichdec,]),max(decnets[whichdec,])))  #this automates the range max
    #ystuff <- list(title = "net return for Vol bucket",  range = c(0.0,0.6))  #this lets you specify the range 
    }
    if(wha=="vol") { y <- decvols[whichdec,]
    titl <- paste(signalname,": avg volatility per quantile by lookback (Y scale varies)") 
    ystuff <- list(title = "volatility for Vol bucket", range = c(min(decvols[whichdec,]),max(decvols[whichdec,]))) 
    #ystuff <- list(title = "volatility for Vol bucket", range = c(0.0,0.25))  #specified 
    }
    if(wha=="rat") { y <- decrats[whichdec,]
    titl <- paste(signalname,": Return/Volatility per quantile by lookback (Y scale varies)") 
    ystuff <- list(title = "reward/risk ratio for Vol bucket", range = c(min(decrats[whichdec,]),max(decrats[whichdec,]))) 
    #ystuff <- list(title = "reward/risk ratio for Vol bucket", range = c(0.0,8.4)) #specified 
    }
    
    text= y
    data = data.frame(x=factor(x,levels=x),text,y)
    
    fig <- plot_ly(
      data, name = paste("Q",whichdec,value), type = "bar",
      x = ~x, textposition = "outside", y= ~y, text =~text) 
    
    fig <- fig %>%
      layout(title = titl,
             xaxis = list(title = "Volatility lookback period (days)"),
             #not working: yaxis = list(title = paste("SPY lagged O-O net return for Vol Decile ",whichdec)),
             yaxis = ystuff,
             autosize = TRUE,
             showlegend = TRUE)
    
    return(fig)
  }
  
  # Now set dec (quantiles) and lookbacks range
  # dec is the number of quantiles to use
  #  (Y scale hard to read when dec > 10)
  dec <- 2
  #lookbacks plot ok as high as 100 or 200
  lbaks <- c(2:30) #lookbacks
  decnets <- c() #matrix of decile Net Return data
  decvols <- c() #matrix of decile returns volatility data annualized
  decrats <- c() #matrix of decile Ratios of returns/volatility
  n <- 0
  for(lb in lbaks){
    qnum <- dec #how many quantiles this loop
    n <- n+1
    #use roc1 as above (non-lagged)
    rv   <- runSD(roc1, n = lb) * sqrt(252)
    rs   <- volatility(gspc_data, n=lb, calc="rogers.satchell")
    gkyz <- volatility(gspc_data, n=lb, calc="gk.yz")
    park <- volatility(gspc_data, n=lb, calc="parkinson")
    avg4 <- (rv+rs+gkyz+park) / 4
    
    #choose your vol measure here! using #
    x_dat2  <- rv; x_dat_label <- "Close2Close" 
    #x_dat2  <- rs; x_dat_label <- "Rogers-Satchell" 
    #x_dat2  <- park; x_dat_label <- "Parkinson" 
    #x_dat2  <- gkyz; x_dat_label <- "GK-YZ" 
    #x_dat2  <- avg4; x_dat_label <- "Average4" 
    
    vollabel = paste(x_dat_label," ",lb, "d vol",sep="")
    xlabel = paste(vollabel," with ",qnum," vol buckets",sep="")
    datp <- as.xts(y_dat) #y_dat is lagged, as above
    datp <- cbind(datp,x_dat2)
    datp <- na.omit(datp)
    datcore2 <- coredata(datp)
    decs2 <- unname(quantile(datcore2[,2], probs = seq(1/qnum, 1-1/qnum, by = 1/qnum)))
    decs2[qnum] <- max(decs2) + 1
    decsmin <- min(decs2) - 1
    #loop through volatility buckets to get returns data
    netgain2 <- c()
    annvols <- c()
    rvratios <- c()
    for(i in 1:qnum){
      # datx = data segment from x_dat[,1] (returns) to summarize
      lowbound <- ifelse(i == 1, decsmin, decs2[i-1])
      hibound <- decs2[i]
      datx <- ifelse( datcore2[,2] >= lowbound & datcore2[,2] < hibound, datcore2[,1], NA)
      datx <- na.omit(datx)
      netgain2[i] <- sum(datx) 
      annvols[i] <- sd(datx) * sqrt(252)
      rvratios[i] <- netgain2[i] / annvols[i]
    }
    decnets <- cbind(decnets,netgain2)
    decvols <- cbind(decvols,annvols)
    decrats <- cbind(decrats,rvratios)
  }
  figs1 <- list()
  figs2 <- list()
  figs3 <- list()
  for(x in 1:dec){
    figs1[x] <- barr("net",x, x_dat_label, paste(":",round(mean(decnets[x,]),2)))
    figs2[x] <- barr("vol",x, x_dat_label, paste(":",round(mean(decvols[x,]),2)))
    figs3[x] <- barr("rat",x, x_dat_label, paste(":",round(mean(decrats[x,]),2)))
    print(paste("Dec:",x,"Sum:",sum(decnets[x,]),"Avg:",mean(decnets[x,])))
    #print(paste("Dec:",x,"Vol:",sum(decvols[x,]),"S/V:",sum(decnets[x,])/sum(decvols[x,])))
    print(paste("Dec:",x,"Vol:",sum(decvols[x,]),"S/V:",mean(decrats[x,])))
  }
  figgy <- subplot(figs1, nrows = length(figs1), shareX = TRUE)
  ziggy <- subplot(figs2, nrows = length(figs2), shareX = TRUE)
  biggy <- subplot(figs3, nrows = length(figs3), shareX = TRUE)
  print(figgy) #figgy is the summed (net) returns per quantile
  print(ziggy) #ziggy is the std dev annualized 
  print(biggy) #biggy is the risk-adj returns used in the blog post
  
} #end of switchy statement
par(mfrow = c(1, 1))

