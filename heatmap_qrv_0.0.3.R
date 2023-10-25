### heatmap_qrv v0.0.3 by babbage9010 and friends
### released under MIT License
# adds nATR to the vol measures you can generate heatmaps for
# takes a long time: reduce the resolution to generate test maps more quickly

# Step 1: Load necessary libraries and data
library(quantmod)
library(PerformanceAnalytics)

date_start <- as.Date("2006-07-01")
date_end <- as.Date("2019-12-31")
symbol_signal1 <- "SPY"  # S&P 500 symbol (use SPY or ^GSPC)
symbol_trade1  <- "SPY"  # ETF to trade

data_spy <- getSymbols(symbol_signal1, src = "yahoo", from = date_start, to = date_end, auto.assign = FALSE)
prices_signal1 <- Cl(data_spy) #SPY ETF, Close(Cl) for the signal (realized vol)
prices_trade1 <- Op(data_spy) #SPY data, Open(Op) for our trading


#make a heatmap, days (lookback) vs thresh (volatility threshold)
# start with a small vector of days, thresh to test it out
# the biggest ones here (29x31) took 7 minutes to run on a M1 MacBook Air
days <- c(3,5,10)
#days <- c(3,5,10,20,40,100)
#days <- c(4,6,8,10,12,14,16,18,20,24,28,32,36,40)
#days <- c(2,3,4,5,6,7,8,9,10,12,14,16,18,20,22,24,26,28,30,34,38,42,46,50,60,70,80,90,100)
thresh <- c(0.05,0.10,0.20,0.50,0.80)
#thresh <- c(0.04,0.05,0.06,0.07,0.08,0.09,0.10,0.11,0.12,0.13,0.14,0.15,0.16,0.17,0.18,0.19,0.20,0.21,0.22,0.23,0.24,0.25,0.30,0.35,0.40,0.45,0.50,0.55,0.60,0.65,0.70)
days <- seq(2,100,by=1)
#use this thresh for nATR
thresh <- seq(0.000,0.05,by=0.0005)
#use this thresh for other vol measures
#thresh <- seq(0.01,1.08,by=0.005)

#uncomment the next line if you want timestamps to see how long it takes.
timestamp()
ar_days <- list()
sh_days <- list()
ex_days <- list()
md_days <- list()
for(d in 1:length(days)){
  ar_thresh <- list()
  sh_thresh <- list()
  ex_thresh <- list()
  md_thresh <- list()
  for(t in 1:length(thresh)){
    r <- ROC(prices_signal1, n = 1, type = "discrete")
    #v <- runSD(r, n = days[d]) * sqrt(252)
    #v <- volatility(data_spy, n=days[d], calc="close")
    #v <- volatility(data_spy, n=days[d], calc="gk.yz")
    #v <- volatility(data_spy, n=days[d], calc="parkinson")
    #v <- volatility(data_spy, n=days[d], calc="rogers.satchell")
    v <- ATR(data_spy, n=days[d], maType="ZLEMA")[ , "atr"] / prices_signal1
    s <- ifelse(v < thresh[t], 1, 0)
    s <- na.omit(s)
    sr <- r * stats::lag(s, 2)
    ar <- Return.annualized(sr)
    ar_thresh[t] <- ar
    sh <- SharpeRatio.annualized(sr)
    sh_thresh[t] <- sh
    ex <- sum(sr != 0, na.rm=TRUE) / length(sr)
    #^^^ why not just sr!=0? see response by Jealie to first answer here: 
    #https://stackoverflow.com/questions/22286957/count-the-number-of-non-zero-elements-of-each-column
    ex_thresh[t] <- ex
    md <- maxDrawdown(sr)
    md_thresh[t] <- md
  }
  ar_days[[d]] <- ar_thresh
  sh_days[[d]] <- sh_thresh
  ex_days[[d]] <- ex_thresh
  md_days[[d]] <- md_thresh
  timestamp()
  print( paste("Days to go:",length(days)-d) )
}

#generate the heatmaps from the matrices
arm_days <- do.call(rbind,ar_days)
arm <- matrix(as.numeric(arm_days),ncol=ncol(arm_days))
rownames(arm) <- days
colnames(arm) <- thresh

shm_days <- do.call(rbind,sh_days)
shm <- matrix(as.numeric(shm_days),ncol=ncol(shm_days))
rownames(shm) <- days
colnames(shm) <- thresh

exm_days <- do.call(rbind,ex_days)
exm <- matrix(as.numeric(exm_days),ncol=ncol(exm_days))
rownames(exm) <- days
colnames(exm) <- thresh

mdm_days <- do.call(rbind,md_days)
mdm <- matrix(as.numeric(mdm_days),ncol=ncol(mdm_days))
rownames(mdm) <- days
colnames(mdm) <- thresh

heatmap(exm,Rowv=NA,Colv=NA)
heatmap(arm,Rowv=NA,Colv=NA)
heatmap(shm,Rowv=NA,Colv=NA)
heatmap(mdm,Rowv=NA,Colv=NA)

#this one too
timestamp()
