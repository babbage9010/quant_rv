### quant_rv v2.0.0 by babbage9010 and friends
### released under MIT License
# quant_rv 2.0.0 new code base
# 1: builds matrices (XTS) of volatilities and signals once that
#      store in environment for faster model idea testing
# 2: can use all the signals (2000+) or a subset (randomized or manual)

### Step 1: Load necessary libraries and data
library(quantmod)
library(PerformanceAnalytics)
library(dplyr)

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
### either use ALL the signals (don't send a subset list)
### allvol is used in the strategy as the signal measure of low vol
allvol <- calc_sigtotal(x_allsigs) 

### or use a select (or random) subset of the signals
### example: selectsigs <- c(99,299,499,699,899,999)
### where each element is a column number in the signals matrix

### or use a random selection of signals
num_random_sigs <- 20 #best to use a multiple of 5 (5 vol measures)
numvol_all <- ncol(x_allsigs)-1
#selectsigs <- floor(runif(num_random_sigs,min=1,max=numvol_all))
#comment out the selectsigs line you DON'T want to use

### or this routine makes sure to select equally among the five vol measures
num_rnd_sigs_per_vm <- floor(num_random_sigs/5) #number of random sigs per vol measure
numpervm <- numvol_all/5
sigs_cc <- floor(runif(num_rnd_sigs_per_vm,min=1,max=numpervm)) #close-close
sigs_rs <- floor(runif(num_rnd_sigs_per_vm,min=numpervm+1,max=2*numpervm)) #rogers-satchell
sigs_pk <- floor(runif(num_rnd_sigs_per_vm,min=2*numpervm+1,max=3*numpervm)) #parkinson
sigs_yz <- floor(runif(num_rnd_sigs_per_vm,min=3*numpervm+1,max=4*numpervm)) #yang-zhang
sigs_tr <- floor(runif(num_rnd_sigs_per_vm,min=4*numpervm+1,max=numvol_all)) #natr
selectsigs <- cbind(sigs_cc, sigs_rs, sigs_pk, sigs_yz, sigs_tr)
#comment out the selectsigs line you DON'T want to use

### selvol == select vol signals, as opposed to all vol signals
selvol <- calc_sigtotal(x_allsigs,selectsigs)

### print out the current selectsigs to the console
###   can be used to replicate select runs 
###   just copy/paste from console and then
###   set `selectsigs <- c(your sigs)` just above `selvol` in code
#print(paste("c(",toString(selectsigs),")"),sep="")

### sdp is the date range to use for stats and plotting
sdp <- "2006-07-31/2019-12-31" # sdp = start date for plotting


### Strategy logic

# thr: how many positive low vol signals does it take to go long?
# This strat uses selvol (selectsigs random subset) 
thr1 <- 1 
# thr == 1 works fine if only 20 signals (selectsigs) are used
signal_1 <- ifelse(selvol >= thr1, 1, 0) #only 1 signal needed
signal_1[is.na(signal_1)] <- 0
label_strategy1 <- "Strategy 1: MV5 original (20 sigs, thr == 1)"

# try setting thr2 higher when using more signals
# e.g. try ~90-110 for all 4070 signals, ~30-60 for 2000 sigs, ~10-15 for 500, etc 
thr2 <- 100 #floor(runif(1,min=90,max=110))  #random signal thr from range
signal_2 <- ifelse(allvol >= thr2, 1, 0) #allvol uses all the vol signals
#signal_2 <- ifelse(selvol >= thr2, 1, 0) #selvol uses subset of vol sigs
signal_2[is.na(signal_2)] <- 0
label_strategy2 <- "Strategy 2: MV5_big"

#signal_3 inverse MV5_2k
signal_3 <- ifelse(allvol < thr2, 1, 0)
signal_3[is.na(signal_3)] <- 0
label_strategy3 <- "Strategy 3: Short MV5_big (inverse)"
label_strategy4 <- "Strategy 4: L/S MV5_big (S2+S3)"



# Step 4: Backtest the strategies
returns_strategy1 <- roc_trade1 * stats::lag(signal_1, 2)
returns_strategy1 <- na.omit(returns_strategy1)
returns_strategy2 <- roc_trade1 * stats::lag(signal_2, 2)
returns_strategy2 <- na.omit(returns_strategy2)

# MV5 inverse uses -1x SPY (here as roc_trade2)
returns_strategy3 <- roc_trade2 * stats::lag(signal_3, 2)
returns_strategy3 <- na.omit(returns_strategy3)

# MV5 L/S combines Strategy_1 + _2
returns_strategy4 <- returns_strategy2 + returns_strategy3

# Calculate Benchmark 1&2 returns
returns_benchmark1 <- stats::lag(roc_benchmark1, 0) 
returns_benchmark1 <- na.omit(returns_benchmark1)
label_benchmark1 <- "Benchmark SPY total return"
returns_benchmark2 <- stats::lag(roc_benchmark2, 0)
returns_benchmark2 <- na.omit(returns_benchmark2)
label_benchmark2 <- "Benchmark 2: SPY Open-Open, no divvies"


# Step 5: Evaluate performance and risk metrics

#1 = MV5 original vs SPY total return and SPY open-open no dividends
comparison1 <- cbind(returns_benchmark1, returns_benchmark2, returns_strategy1)
colnames(comparison1) <- c(label_benchmark1, label_benchmark2, label_strategy1)

#2 = MV5 original vs MV5_big vs SPY total return
comparison2 <- cbind(returns_benchmark1, returns_strategy1, returns_strategy2)
colnames(comparison2) <- c(label_benchmark1, label_strategy1, label_strategy2)

#3 = MV5 original vs MV5_big vs MV5_big_inverse vs MV5_big_LS_combined
comparison3 <- cbind(returns_benchmark1, returns_strategy1, returns_strategy2, returns_strategy3, returns_strategy4)
colnames(comparison3) <- c(label_benchmark1, label_strategy1, label_strategy2, label_strategy3, label_strategy4)

### use comp to choose which comparison to display
comp <- comparison3
stats_rv <- rbind(table.AnnualizedReturns(comp[sdp]), maxDrawdown(comp[sdp]))
charts.PerformanceSummary(comp[sdp], main = "quant_rv strategies vs SPY total return Benchmark")

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
print( paste("B2 -", extraStats(returns_benchmark2[sdp]) )) 
print( paste("S1 -", extraStats(returns_strategy1[sdp]) )) 
print( paste("S2 -", extraStats(returns_strategy2[sdp]) )) 
print( paste("S3 -", extraStats(returns_strategy3[sdp]) )) 
print( paste("S4 -", extraStats(returns_strategy4[sdp]) )) 
