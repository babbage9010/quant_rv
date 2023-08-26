### rv_vs_rtns_0.0.1.R by babbage9010 and friends
# CORRECTED CODE BELOW (see #COMMENT blocks)
# initial release
# this code is really weak, I barely knew what I was doing when I 
# started, but it's my start.
### released under MIT License

# Step 1: Load libraries and data
library(quantmod)
library(PerformanceAnalytics)

start_date <- as.Date("2006-07-01")
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
roc1 <- ROC(signal_prices, n = 1, type = "discrete")
lookbk <- 20
rv20 <- runSD(roc1, n = lookbk) * sqrt(252)
rs20   <- volatility(gspc_data, n=lookbk, calc="rogers.satchell")
gkyz20 <- volatility(gspc_data, n=lookbk, calc="gk.yz")
park20 <- volatility(gspc_data, n=lookbk, calc="parkinson")

#choose one of these to uncomment
x_dat  <- rv20; x_dat_label = "C2C" 
#x_dat <- rs20; x_dat_label = "RS" 
#x_dat <- gkyz20; x_dat_label = "GKYZ" 
#x_dat <- park20; x_dat_label = "Park" 

vollabel = paste(x_dat_label," ",lookbk, "d vol",sep="")

#y - SPY open lagged returns
roc_trade1 <- ROC(trade_prices, n = 1, type = "discrete")
returns_spy_open <- roc_trade1 
#CORRECTION: lag here was used incorrectly
#NO! ORIGINAL LINE:  returns_spy_open <- stats::lag(returns_spy_open, 2)
returns_spy_open <- stats::lag(returns_spy_open, -2)
# We normally use a two-day lag(x,2) on a signal to match it properly with Open-Open 
#  returns from two days in the future, corresponding to reading a signal after a Close
#  then trading as needed on the following Open (ie, in the morning for this case).
#  BUT I accidentally applied the same +2 open to align the RETURNS to the volatility
#  measures here. This pushes the returns forward two days instead of pushing the signal
#  forward... meaning the signal was aligning with an Open-Open return from two days 
#  previous, giving us an unrealistically gorgeous correlation and low vol anomaly.
#  Run it yourself to see that now it looks pretty close to random with this setup.
#  More exploration to come. 
y_dat <- returns_spy_open

#rid of NAs to even up the data (tip: this avoids NA-related errors)
dat <- as.xts(y_dat)
dat <- cbind(dat,x_dat)
dat <- na.omit(dat)
datcore <- coredata(dat)


# barcharts of SPY returns per volatility bucket
pl1 <- plot(x=datcore[,2],y=datcore[,1], sub="daily data, 2006/07/01 to 2019/12/31",main = paste(sep="", vollabel," (x-axis) vs lagged SPY Open returns (y-axis)"))
pl1 <- abline(reg = lm(datcore[,1] ~ datcore[,2]), col = "red", lwd = 2)

# Set for four graphs, or seven including 3 mean daily SPY returns plots
numrows <- 4 #either 2 or 4 please
# Set up 2x2 graphical window
par(mfrow = c(numrows, 2))

# Recreate all four/seven plots
pl1 <- plot(x=datcore[,2],y=datcore[,1], sub=paste(sep="","daily data, ",start_date," to ",end_date),main = paste(sep="", vollabel," (x-axis) vs lagged SPY Open returns (y-axis)"))
pl1 <- abline(reg = lm(datcore[,1] ~ datcore[,2]), col = "red", lwd = 2)

if(numrows == 4){
  pl2 <- plot.new()
}
#helper function
winpc <- function(vec){ sum(vec > 0) / sum(vec != 0) }

qnums <- c(10,30,100) #number of quantiles (buckets) (eg 10 for deciles)
for(q in 1:3){
  qnum <- qnums[q] 
  xlabel = paste(vollabel," with ",qnum," vol buckets",sep="")
  decs <- unname(quantile(datcore[,2], probs = seq(1/qnum, 1-1/qnum, by = 1/qnum)))
  decs[qnum] <- max(decs) + 1
  decsmin <- min(decs) - 1
  #loop through volatility buckets to get mean returns
  means <- c()
  wins <- c()
  for(i in 1:qnum){
    # datx = data segment from x_dat[,1] (returns) to summarize
    lowbound <- ifelse(i == 1, decsmin, decs[i-1])
    hibound <- decs[i]
    datx <- ifelse( datcore[,2] >= lowbound & datcore[,2] < hibound, datcore[,1], NA)
    datx <- na.omit(datx)
    means[i] <- mean(datx)
    wins[i] <- winpc(datx)
    #print( paste("decile",i,"mean:",means[i],"vol range:",lowval,"-",hival) )
  }
  barplot(means,xlab=xlabel,ylab="SPY mean daily return",main="Mean daily SPY returns per volatility bucket",sub="low vol on left, high vol on right")
  if(numrows == 4){
    barplot(wins,xlab=xlabel,ylab="SPY mean daily return",main="Daily win % for SPY returns per vol bucket",sub="low vol on left, high vol on right")
    abline(h=c(0.54),col="red")
  }
}
