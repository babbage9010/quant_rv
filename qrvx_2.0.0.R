### qrvx v2.0.0 by babbage9010 and friends
### released under MIT License
# qrvx_2.0.0.R based on quant_rv_2.0.0.R
# 1: aims for decent performance strategy with low correlation with SPY
# 2: combo w/SPY in 60/40-style: SPY-like returns, better risk-adjusted stats
# 3: published with data through Feb 26, 2024

### Step 1: Load necessary libraries and data
library(quantmod)
library(PerformanceAnalytics)

date_start <- as.Date("1992-03-01")
date_end <- as.Date("2033-12-31")
symbol_benchmark1  <- "SPY"  # benchmark for comparison
symbol_signal1 <- "SPY"  # S&P 500 symbol (use SPY or ^GSPC)
symbol_trade1  <- "SPY"  # ETF to trade
symbol_trade2  <- "SPY"  # -1x ETF to trade. in real life use SH

### reloadall == TRUE is to rebuild the VOL and SIG matrices 
###  when changing lookback & threshold vectors
### FWIW, I just leave it FALSE and empty my environment if I want 
###  to rebuild the matrices
reloadall <- FALSE | !exists("data_benchmark1")  
if(reloadall){
  data_benchmark1 <- getSymbols(symbol_benchmark1, src = "yahoo", from = date_start, to = date_end, auto.assign = FALSE)
  data_signal1 <- getSymbols(symbol_signal1, src = "yahoo", from = date_start, to = date_end, auto.assign = FALSE)
  data_trade1 <- getSymbols(symbol_trade1, src = "yahoo", from = date_start, to = date_end, auto.assign = FALSE)
  data_trade2 <- getSymbols(symbol_trade2, src = "yahoo", from = date_start, to = date_end, auto.assign = FALSE)
  prices_benchmark1 <- Ad(data_benchmark1) #Adjusted(Ad) for the #1 benchmark
  prices_benchmark2 <- Op(data_benchmark1) #Open(Op) for the #2 benchmark
  prices_signal1 <- Ad(data_signal1) #Adjusted(Ad) for the signal (realized vol)
  prices_trade1 <- Op(data_trade1) #Open(Op) for our trading
  prices_trade2 <- Op(data_trade2) #Open(Op) for our trading
  prices_signal1Cl <- Cl(data_signal1) #Close(Cl) for the ATR normalization
}

### Step 2: Calculate ROC series
roc_signal1 <- ROC(prices_signal1, n = 1, type = "discrete")
roc_benchmark1 <- ROC(prices_benchmark1, n = 1, type = "discrete")
roc_benchmark2 <- ROC(prices_benchmark2, n = 1, type = "discrete")
roc_trade1 <- ROC(prices_trade1, n = 1, type = "discrete")

#HACK! SH not available in olden times, so use -1x SPY
roc_trade2 <- -1 * ROC(prices_trade1, n = 1, type = "discrete")


# Step 3: Function for building the volatility signals
# we're using five measures of volatility with four lookback periods

### first, calculate the volatility parameter space as a big XTS
calc_vols <- function(volmeas, lookbacks){
  ### calculates volatilities for all the vol measures + lookbacks
  numvolmeas <- length(volmeas) #number of vol measures (5)
  numlbs <- length(lookbacks)
  xts_vols <- as.xts(data_signal1[,"SPY.Adjusted"])
  numvols <- numvolmeas*numlbs
  volnames <- c(1:numvols)
  nas <- xts(matrix(NA,nrow(xts_vols),length(volnames)),
             index(xts_vols), dimnames=list(NULL,volnames))
  xts_vols <- merge(xts_vols, nas)
  print(paste("vrows",nrow(xts_vols),"vcols",ncol(xts_vols)))
  vidx <- 0
  for(vv in volmeas){
    #print(paste("vv",vv,"vidx",vidx))
    if(vv != "natr"){
      for(nn in 1:numlbs){
        #print(paste("nn",nn,"v+n",vidx+nn,"lb",lookbacks[nn]))
        xts_vols[,1+vidx+nn] <- volatility(data_signal1, n = lookbacks[nn], calc = vv)
      }
    } else {
      for(nn in 1:numlbs){
        #print(paste("nn",nn,"v+n",vidx+nn,"lb",lookbacks[nn]))
        xts_vols[,1+vidx+nn] <- ATR(data_signal1, n=lookbacks[nn], maType="ZLEMA")[ , "atr"] / prices_signal1Cl
      }
    }
    vidx <- vidx + numlbs
  }
  return(xts_vols)
}

### second, calc the vol signals with a sequence of thresholds, store as XTS
calc_sigs <- function(volmeas, lookbacks, thevols, vthresh, lthresh){
  ### calculates all the signals: loop volmeasures, then lookbacks, then thresholds
  xts_sigs <- as.xts(data_signal1[,"SPY.Adjusted"])
  numlbs <- length(lookbacks)
  numthresholds <- length(vthresh)
  numvols <- ncol(thevols) - 1
  numsigs <- numvols * numthresholds
  print(paste("nl",numlbs,"nt",numthresholds,"nv",numvols,"ns",numsigs))
  siggnames <- c(1:numsigs)
  nas <- xts(matrix(NA,nrow(xts_sigs),length(siggnames)),
             index(xts_sigs), dimnames=list(NULL,siggnames))
  xts_sigs <- merge(xts_sigs, nas)
  print(paste("vrows",nrow(xts_sigs),"vcols",ncol(xts_sigs)))
  
  ### make a matrix of sig references (which vol,lb,th for each sig)
  vidx <- 0
  sidx <- 0
  for(vv in volmeas){
    print(paste("vv",vv,"vidx",vidx))
    if(vv != "natr"){
      for(nn in 1:numlbs){
        #print(paste("nn",nn))
        for(tt in 1:numthresholds){
          #calc sig here
          #print(paste("tt",tt,"s+n+t",sidx+nn+tt,"whichvol",1+vidx+nn))
          xts_sigs[,sidx+nn+tt] <- ifelse( thevols[,1+vidx+nn] < vthresh[tt], 1, 0)
        }
        sidx <- sidx + numthresholds - 1 
      }
      sidx <- sidx + numlbs #4? is it numthresholds-1? or?
    } else { # only natr
      for(nn in 1:numlbs){
        #print(paste("nn",nn))
        for(tt in 1:numthresholds){
          #calc sig here
          #print(paste("tt",tt,"s+n+t",sidx+nn+tt,"whichvol",1+vidx+nn))
          xts_sigs[,sidx+nn+tt] <- ifelse( thevols[,1+vidx+nn] < lthresh[tt], 1, 0)
        }
        sidx <- sidx + numthresholds - 1 
      }
    }
    vidx <- vidx + numlbs
  }
  return(xts_sigs)
}

### third, calculate the signal totals for each trading day
calc_sigtotal <- function(thesigs,sbst=c(0)){
  ### calculate the allvol or selvol, total of positive signals
  ### thesigs includes a reference column of SPY values
  ###   get rid of it for this
  ### sbst is a list, an optional subset of column numbers to sum sig totals across
  ###  default is to use ALL signals available (allvol)
  numsigcols <- ncol(thesigs)
  allthesigs <- thesigs[,2:numsigcols]
  if(sbst[1] == 0){
    therealsigs <- allthesigs
  } else {
    therealsigs <- allthesigs[,sbst]
  }
  #siggs is our signal totals to be returned as a 1 col xts object
  siggs <- as.xts(data_signal1[,"SPY.Adjusted"]) #match it to SPY for index
  sums <- xts(rowSums(therealsigs, na.rm=TRUE), index(siggs))
  siggs[,1] <- sums  #replace prices in siggs with signal sums
  return( siggs )
}

### finally, set up the vthresh and lthresh sequences 
###   then call the function(s) above

### vthresh and lthresh are the threshold values for signal generation
###   length(vthresh) must == length(lthresh) to work right
### vthresh = thresholds for the four volatility measures
#vthresh3 <- seq(0.13, 0.22, by=0.01) #lower res sampling
#vthresh3 <- seq(0.13, 0.22, by=0.005) #medium res sampling
vthresh3 <- seq(0.13, 0.22, by=0.0025) #high res sampling

### lthresh = thresholds for the NATR vol-like measure
#lthresh3 <- seq(0.006, 0.015, by=0.00025) #lower res sampling
#lthresh3 <- seq(0.006, 0.015, by=0.00025) #medium res sampling
lthresh3 <- seq(0.006, 0.015, by=0.00025) #high res sampling


### lookback period in days
lookbacks <- seq(4, 25, by=1)

### parameter names for the volatility measures in calc_vols
volmeasures <- c("close","rogers.satchell","parkinson","gk.yz","natr") #vol measures

### calculate volatility measures and signal candidates
if(reloadall){
  x_allvols <- calc_vols(volmeasures, lookbacks)
  x_allsigs <- calc_sigs(volmeasures, lookbacks, x_allvols, vthresh3, lthresh3)
}

### add up the signals
### allvol is used in the strategy as the signal measure of low vol
allvol <- calc_sigtotal(x_allsigs) 

### sdp is the date range to use for stats and plotting
sdp <- "2006-07-01/" # sdp = start date for plotting

### Calculate Benchmark 1&2 returns
returns_benchmark1 <- stats::lag(roc_benchmark1, 0) 
returns_benchmark1 <- na.omit(returns_benchmark1)
label_benchmark1 <- "Benchmark SPY total return"
returns_benchmark2 <- stats::lag(roc_benchmark2, 0)
returns_benchmark2 <- na.omit(returns_benchmark2)
label_benchmark2 <- "Benchmark 2: SPY Open-Open, no divvies"



### STRATEGIES ###

### Strategy 1 High volatility, go short
label_1 <- "S_1: High vol; SHORT"
thr_hi_vol <- 100 
signal_1 <- ifelse(allvol < thr_hi_vol, 1, 0) 
signal_1[is.na(signal_1)] <- 0
#use roc_trade2 (-1x SPY) not roc_trade1
returns_1 <- roc_trade2 * stats::lag(signal_1, 2)
returns_1 <- na.omit(returns_1)

#plot and stats
comparison_1 <- cbind(returns_1, returns_benchmark1)
colnames(comparison_1) <- c(label_1, label_benchmark1)
stats_rv_1 <- rbind(table.AnnualizedReturns(comparison_1[sdp]), maxDrawdown(comparison_1[sdp]))
charts.PerformanceSummary(comparison_1[sdp], main = "qrvx Short component vs SPY total return")

### Strategy 2 Low volatility, go long
label_2 <- "S_2: Low vol; LONG"
thr_lo_vol <- 3600 
signal_2 <- ifelse(allvol >= thr_lo_vol, 1, 0) 
signal_2[is.na(signal_2)] <- 0
returns_2 <- roc_trade1 * stats::lag(signal_2, 2)
returns_2 <- na.omit(returns_2)

#plot and stats
comparison_2 <- cbind(returns_2, returns_benchmark1, returns_1)
colnames(comparison_2) <- c(label_2, label_benchmark1, label_1)
stats_rv_2 <- rbind(table.AnnualizedReturns(comparison_2[sdp]), maxDrawdown(comparison_2[sdp]))
charts.PerformanceSummary(comparison_2[sdp], main = "qrvx_2+1 Long, Short components vs SPY")

### Strategy 3: qrvx_2+1 
label_3 <- "S_3: qrvx_2+1 L/S"
returns_3 <- returns_1 + returns_2 #corr = -0.110
returns_3 <- na.omit(returns_3)

### Strategy 4: combo SPY/qrvx
label_4 <- "S_4: 35/65 combo SPY/qrvx_1+2"
fraction <- 0.35 ### fraction allocated to SPY
returns_4 <- fraction * returns_benchmark1 + (1 - fraction) * returns_3 
returns_4 <- na.omit(returns_4)

#plot and stats
comparison_3 <- cbind(returns_3, returns_benchmark1, returns_4, returns_1, returns_2)
colnames(comparison_3) <- c(label_3, label_benchmark1, label_4, label_1, label_2)
stats_rv_3 <- rbind(table.AnnualizedReturns(comparison_3[sdp]), maxDrawdown(comparison_3[sdp]))
charts.PerformanceSummary(comparison_3[sdp], main = "qrvx_2+1 and combo vs SPY")

### Strategy 5 Med-High volatility (100 <= allvol < 700)
label_5 <- "S_5: Med-High vol; LONG"
thr_med_vol <- 700
signal_5 <- ifelse( allvol >= thr_hi_vol & allvol < thr_med_vol, 1, 0) 
signal_5[is.na(signal_5)] <- 0
returns_5 <- roc_trade1 * stats::lag(signal_5, 2)
returns_5 <- na.omit(returns_5)

### Strategy 6 qrvx_51 L/S
label_6 <- "S_6: qrvx_5+1 L/S"
returns_6 <- returns_5 + returns_1 # corr = -0.117
returns_6 <- na.omit(returns_6)

label_7 <- "S_7: 35/65 combo SPY/qrvx_5+1"
fraction <- 0.35
returns_7 <- fraction * returns_benchmark1 + (1 - fraction) * returns_6
returns_7 <- na.omit(returns_7)

#plot and stats
comparison_4 <- cbind(returns_6, returns_benchmark1, returns_7, returns_1, returns_5)
colnames(comparison_4) <- c(label_6, label_benchmark1, label_7, label_1, label_5)
stats_rv_4 <- rbind(table.AnnualizedReturns(comparison_4[sdp]), maxDrawdown(comparison_4[sdp]))
charts.PerformanceSummary(comparison_4[sdp], main = "qrvx_5+1 and combo vs SPY total return")


### Strategy 8 qrvx_251 L/S
label_8 <- "S_8: qrvx_2+5+1 L/L/S"
returns_8 <- returns_2 + returns_5 + returns_1 # corr = -0.030 at pub
returns_8 <- na.omit(returns_8)

label_9 <- "S_9: 35/65 combo SPY/qrvx_2+5+1"
fraction <- 0.35
returns_9 <- fraction * returns_benchmark1 + (1 - fraction) * returns_8
returns_9 <- na.omit(returns_9)

#plot and stats
comparison_5 <- cbind(returns_8, returns_benchmark1, returns_9, returns_1, returns_2, returns_5)
colnames(comparison_5) <- c(label_8, label_benchmark1, label_9, label_1, label_2, label_5)
stats_rv_5 <- rbind(table.AnnualizedReturns(comparison_5[sdp]), maxDrawdown(comparison_5[sdp]))
charts.PerformanceSummary(comparison_5[sdp], main = "qrvx_2+5+1 and combo vs SPY total return")






### add an "exposure" metric (informative, not strictly correct)
exposure <- function(vec){ sum(vec != 0) / length(vec) * 100 }
### and a couple more metrics
winPercent <- function(vec){ 
  s <- sum(vec > 0)
  s / (s + sum(vec < 0)) * 100
}
avgWin <- function(vec){ 
  aw <- mean( na.omit(ifelse(vec>0,vec,NA))) 
  return( aw * 100 )
}
avgLoss <- function(vec){ 
  al <- mean( na.omit(ifelse(vec<0,vec,NA))) 
  return( al * 100 )
}
extraStats <- function(vec){
  ex <- exposure(vec)
  aw <- avgWin(vec)
  al <- avgLoss(vec)
  wp <- winPercent(vec)
  wl <- -(aw/al)
  return( paste("exp_%:", round(ex,2),  " win_%:", round(wp, 2), " avgWin:", round(aw,3), " avgLoss:", round(al,3), "w/l:", round(wl, 3)) )
}

print( paste("B1 -", extraStats(returns_benchmark1[sdp]) )) 
print( paste("S_1 -", extraStats(returns_1[sdp]) )) 
print( paste("S_2 -", extraStats(returns_2[sdp]) )) 
print( paste("S_3 -", extraStats(returns_3[sdp]) )) 
print( paste("S_4 -", extraStats(returns_4[sdp]) )) 
print( paste("S_5 -", extraStats(returns_5[sdp]) )) 
print( paste("S_6 -", extraStats(returns_6[sdp]) )) 
print( paste("S_7 -", extraStats(returns_7[sdp]) )) 
print( paste("S_8 -", extraStats(returns_8[sdp]) )) 
print( paste("S_9 -", extraStats(returns_9[sdp]) )) 

