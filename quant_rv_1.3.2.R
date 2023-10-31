### quant_rv v1.3.2 by babbage9010 and friends
### released under MIT License
# changelog
# 1: add second benchmark, Op-Op no dividends
# 2: fix (remove) erroneous 2 day lag in the benchmark returns
# 3: remove useless negative thresholds in vol signals
# 4: add normalized ATR functionality, add to strategy signals
# 5: revision: see #lookback periods randomized; using four lookback ranges
#   offering complete coverage from 4-25 days, instead of three formerly
# 6: minor changes to symbol naming/loading code

# Step 1: Load necessary libraries and data
library(quantmod)
library(PerformanceAnalytics)

date_start <- as.Date("2006-07-01")
date_end <- as.Date("2019-12-31")
symbol_benchmark1  <- "SPY"  # benchmark for comparison
symbol_signal1 <- "SPY"  # S&P 500 symbol (use SPY or ^GSPC)
symbol_trade1  <- "SPY"  # ETF to trade

data_benchmark1 <- getSymbols(symbol_benchmark1, src = "yahoo", from = date_start, to = date_end, auto.assign = FALSE)
#data_signal1 <- getSymbols(symbol_signal1, src = "yahoo", from = date_start, to = date_end, auto.assign = FALSE)
#data_trade1 <- getSymbols(symbol_trade1, src = "yahoo", from = date_start, to = date_end, auto.assign = FALSE)
data_signal1 <- data_benchmark1 #do this if only using "SPY", e.g., to avoid extra downloading
data_trade1 <- data_benchmark1
data_benchmark2 <- data_benchmark1
prices_benchmark1 <- Ad(data_benchmark1) #Adjusted(Ad) for the #1 benchmark
prices_benchmark2 <- Op(data_benchmark2) #Open(Op) for the #2 benchmark
prices_signal1 <- Ad(data_signal1) #Adjusted(Ad) for the signal (realized vol)
prices_trade1 <- Op(data_trade1) #Open(Op) for our trading
prices_signal1Cl <- Cl(data_signal1) #Close(Cl) for the ATR normalization

# Step 2: Calculate ROC series
roc_signal1 <-   ROC(prices_signal1, n = 1, type = "discrete")
roc_benchmark1 <- ROC(prices_benchmark1, n = 1, type = "discrete")
roc_benchmark2 <- ROC(prices_benchmark2, n = 1, type = "discrete")
roc_trade1 <-    ROC(prices_trade1, n = 1, type = "discrete")

# Step 3: Develop the trading strategies
# we're using five measures of volatility with four lookback periods

#lookback periods randomized
lookback_long <- floor(runif(5, min = 20, max = 25)) 
lookback_medlong <- floor(runif(5, min = 14, max = 19)) 
lookback_medshort <- floor(runif(5, min = 9, max = 13)) 
lookback_short <- floor(runif(5, min = 4, max = 8)) 

#calculate all five volatility measures across 4 lookback ranges (12)
#cc: Close-to-Close volatility
vol_cc_L <- volatility(data_signal1, n = lookback_long[1], calc = "close")
vol_cc_ML <- volatility(data_signal1, n = lookback_medlong[1], calc = "close")
vol_cc_MS <- volatility(data_signal1, n = lookback_medshort[1], calc = "close")
vol_cc_S <- volatility(data_signal1, n = lookback_short[1], calc = "close")
#rs: Rogers-Satchell volatility
vol_rs_L <- volatility(data_signal1, n = lookback_long[2], calc = "rogers.satchell")
vol_rs_ML <- volatility(data_signal1, n = lookback_medlong[2], calc = "rogers.satchell")
vol_rs_MS <- volatility(data_signal1, n = lookback_medshort[2], calc = "rogers.satchell")
vol_rs_S <- volatility(data_signal1, n = lookback_short[2], calc = "rogers.satchell")
#p: Parkinson volatility
vol_p_L <- volatility(data_signal1, n = lookback_long[3], calc = "parkinson")
vol_p_ML <- volatility(data_signal1, n = lookback_medlong[3], calc = "parkinson")
vol_p_MS <- volatility(data_signal1, n = lookback_medshort[3], calc = "parkinson")
vol_p_S <- volatility(data_signal1, n = lookback_short[3], calc = "parkinson")
#gkyz: Garman-Klass Yang-Zhang volatility
vol_gkyz_L <- volatility(data_signal1, n = lookback_long[4], calc = "gk.yz")
vol_gkyz_ML <- volatility(data_signal1, n = lookback_medlong[4], calc = "gk.yz")
vol_gkyz_MS <- volatility(data_signal1, n = lookback_medshort[4], calc = "gk.yz")
vol_gkyz_S <- volatility(data_signal1, n = lookback_short[4], calc = "gk.yz")

#natr: normalized Average True Range volatility
natr_L <- ATR(data_signal1, n=lookback_long[5], maType="ZLEMA")[ , "atr"] / prices_signal1Cl
natr_ML <- ATR(data_signal1, n=lookback_medlong[5], maType="ZLEMA")[ , "atr"] / prices_signal1Cl
natr_MS <- ATR(data_signal1, n=lookback_medshort[5], maType="ZLEMA")[ , "atr"] / prices_signal1Cl
natr_S <- ATR(data_signal1, n=lookback_short[5], maType="ZLEMA")[ , "atr"] / prices_signal1Cl

#strategy volatility thresholds, randomized
vthresh <- runif(16, min = 0.12, max = 0.17) #low threshold for volatility measures
lthresh <- runif(4, min = 0.010, max = 0.015) #low threshold for nATR

#calculate the Vol signals
sig_cc_L <- ifelse(vol_cc_L < vthresh[1], 1, 0)
sig_cc_ML <- ifelse(vol_cc_ML < vthresh[2], 1, 0)
sig_cc_MS <- ifelse(vol_cc_MS < vthresh[3], 1, 0)
sig_cc_S <- ifelse(vol_cc_S < vthresh[4], 1, 0)
sig_rs_L <- ifelse(vol_rs_L < vthresh[5], 1, 0)
sig_rs_ML <- ifelse(vol_rs_ML < vthresh[6], 1, 0)
sig_rs_MS <- ifelse(vol_rs_MS < vthresh[7], 1, 0)
sig_rs_S <- ifelse(vol_rs_S < vthresh[8], 1, 0)
sig_p_L <- ifelse(vol_p_L < vthresh[9], 1, 0)
sig_p_ML <- ifelse(vol_p_ML < vthresh[10], 1, 0)
sig_p_MS <- ifelse(vol_p_MS < vthresh[11], 1, 0)
sig_p_S <- ifelse(vol_p_S < vthresh[12], 1, 0)
sig_gkyz_L <- ifelse(vol_gkyz_L < vthresh[13], 1, 0)
sig_gkyz_ML <- ifelse(vol_gkyz_ML < vthresh[14], 1, 0)
sig_gkyz_MS <- ifelse(vol_gkyz_MS < vthresh[15], 1, 0)
sig_gkyz_S <- ifelse(vol_gkyz_S < vthresh[16], 1, 0)

sig_natr_L <- ifelse(natr_L < lthresh[1], 1, 0)
sig_natr_ML <- ifelse(natr_ML < lthresh[2], 1, 0)
sig_natr_MS <- ifelse(natr_MS < lthresh[3], 1, 0)
sig_natr_S <- ifelse(natr_S < lthresh[4], 1, 0)

#add up the signals
totalvol <- (
  + sig_cc_L
  + sig_cc_ML
  + sig_cc_MS
  + sig_cc_S
  + sig_rs_L 
  + sig_rs_ML 
  + sig_rs_MS 
  + sig_rs_S  
  + sig_p_L 
  + sig_p_ML 
  + sig_p_MS 
  + sig_p_S 
  + sig_gkyz_L 
  + sig_gkyz_ML 
  + sig_gkyz_MS 
  + sig_gkyz_S 
)

totalnatr <- (
  + sig_natr_L
  + sig_natr_ML
  + sig_natr_MS
  + sig_natr_S
)

#combine the signals
#look for any positive signal (ie, 1. or increase this threshold count up to 20)
signal_1 <- ifelse(totalvol + totalnatr >= 1, 1, 0)
signal_1[is.na(signal_1)] <- 0
label_strategy1 <- "Strategy 1: Multivol 5"

signal_2 <- ifelse(totalnatr >= 1, 1, 0)
signal_2[is.na(signal_2)] <- 0
label_strategy2 <- "Strategy 2: nATR only"

signal_3 <- ifelse(totalvol >= 1, 1, 0)
signal_3[is.na(signal_3)] <- 0
label_strategy3 <- "Strategy 3: Multivol 4 (no nATR)"

# Step 4: Backtest the strategies
returns_strategy1 <- roc_trade1 * stats::lag(signal_1, 2)
returns_strategy1 <- na.omit(returns_strategy1)
returns_strategy2 <- roc_trade1 * stats::lag(signal_2, 2)
returns_strategy2 <- na.omit(returns_strategy2)
returns_strategy3 <- roc_trade1 * stats::lag(signal_3, 2)
returns_strategy3 <- na.omit(returns_strategy3)

# Calculate Benchmark 1&2 returns
returns_benchmark1 <- stats::lag(roc_benchmark1, 0) 
returns_benchmark1 <- na.omit(returns_benchmark1)
label_benchmark1 <- "Benchmark SPY total return"
returns_benchmark2 <- stats::lag(roc_benchmark2, 0)
returns_benchmark2 <- na.omit(returns_benchmark2)
label_benchmark2 <- "Benchmark 2: SPY Open-Open, no divvies"

# Step 5: Evaluate performance and risk metrics
# add an "exposure" metric (informative, not evaluative)
exposure <- function(vec){ sum(vec != 0) / length(vec) }

comparison <- cbind(returns_benchmark1, returns_benchmark2, returns_strategy1, returns_strategy2, returns_strategy3)
colnames(comparison) <- c(label_benchmark1, label_benchmark2, label_strategy1, label_strategy2, label_strategy3)
stats_rv <- rbind(table.AnnualizedReturns(comparison), maxDrawdown(comparison))
charts.PerformanceSummary(comparison, main = "Realized Vol Strategies vs S&P 500 Benchmark")

#print exposure to console in R Studio
exposure_s1 <- exposure(returns_strategy1)
exposure_s2 <- exposure(returns_strategy2)
exposure_s3 <- exposure(returns_strategy3)
print( paste("Exposure for Strategy 1:", exposure_s1) ) 
print( paste("Exposure for Strategy 2:", exposure_s2) ) 
print( paste("Exposure for Strategy 3:", exposure_s3) ) 

#Step 6: generate/plot 20 Multivol 5 equity curves at once
comparison3 <- as.xts(returns_benchmark1)
for(s in 1:20){
  #repeat signal generation within the loop...
  #lookback periods randomized
  lookback_long <- floor(runif(5, min = 20, max = 25)) 
  lookback_medlong <- floor(runif(5, min = 14, max = 19)) 
  lookback_medshort <- floor(runif(5, min = 9, max = 13)) 
  lookback_short <- floor(runif(5, min = 4, max = 8)) 
  
  #calculate all five volatility measures across 4 lookback ranges (12)
  #cc: Close-to-Close volatility
  vol_cc_L <- volatility(data_signal1, n = lookback_long[1], calc = "close")
  vol_cc_ML <- volatility(data_signal1, n = lookback_medlong[1], calc = "close")
  vol_cc_MS <- volatility(data_signal1, n = lookback_medshort[1], calc = "close")
  vol_cc_S <- volatility(data_signal1, n = lookback_short[1], calc = "close")
  #rs: Rogers-Satchell volatility
  vol_rs_L <- volatility(data_signal1, n = lookback_long[2], calc = "rogers.satchell")
  vol_rs_ML <- volatility(data_signal1, n = lookback_medlong[2], calc = "rogers.satchell")
  vol_rs_MS <- volatility(data_signal1, n = lookback_medshort[2], calc = "rogers.satchell")
  vol_rs_S <- volatility(data_signal1, n = lookback_short[2], calc = "rogers.satchell")
  #p: Parkinson volatility
  vol_p_L <- volatility(data_signal1, n = lookback_long[3], calc = "parkinson")
  vol_p_ML <- volatility(data_signal1, n = lookback_medlong[3], calc = "parkinson")
  vol_p_MS <- volatility(data_signal1, n = lookback_medshort[3], calc = "parkinson")
  vol_p_S <- volatility(data_signal1, n = lookback_short[3], calc = "parkinson")
  #gkyz: Garman-Klass Yang-Zhang volatility
  vol_gkyz_L <- volatility(data_signal1, n = lookback_long[4], calc = "gk.yz")
  vol_gkyz_ML <- volatility(data_signal1, n = lookback_medlong[4], calc = "gk.yz")
  vol_gkyz_MS <- volatility(data_signal1, n = lookback_medshort[4], calc = "gk.yz")
  vol_gkyz_S <- volatility(data_signal1, n = lookback_short[4], calc = "gk.yz")
  
  #natr: normalized Average True Range volatility
  natr_L <- ATR(data_signal1, n=lookback_long[5], maType="ZLEMA")[ , "atr"] / prices_signal1Cl
  natr_ML <- ATR(data_signal1, n=lookback_medlong[5], maType="ZLEMA")[ , "atr"] / prices_signal1Cl
  natr_MS <- ATR(data_signal1, n=lookback_medshort[5], maType="ZLEMA")[ , "atr"] / prices_signal1Cl
  natr_S <- ATR(data_signal1, n=lookback_short[5], maType="ZLEMA")[ , "atr"] / prices_signal1Cl
  
  #strategy volatility thresholds, randomized
  vthresh <- runif(16, min = 0.12, max = 0.17) #low threshold for volatility measures
  lthresh <- runif(4, min = 0.010, max = 0.015) #low threshold for nATR

  #calculate the Vol signals
  sig_cc_L    <- ifelse(vol_cc_L < vthresh[1], 1, 0)
  sig_cc_ML   <- ifelse(vol_cc_ML < vthresh[2], 1, 0)
  sig_cc_MS   <- ifelse(vol_cc_MS < vthresh[3], 1, 0)
  sig_cc_S    <- ifelse(vol_cc_S < vthresh[4], 1, 0)
  sig_rs_L    <- ifelse(vol_rs_L < vthresh[5], 1, 0)
  sig_rs_ML   <- ifelse(vol_rs_ML < vthresh[6], 1, 0)
  sig_rs_MS   <- ifelse(vol_rs_MS < vthresh[7], 1, 0)
  sig_rs_S    <- ifelse(vol_rs_S < vthresh[8], 1, 0)
  sig_p_L     <- ifelse(vol_p_L < vthresh[9], 1, 0)
  sig_p_ML    <- ifelse(vol_p_ML < vthresh[10], 1, 0)
  sig_p_MS    <- ifelse(vol_p_MS < vthresh[11], 1, 0)
  sig_p_S     <- ifelse(vol_p_S < vthresh[12], 1, 0)
  sig_gkyz_L  <- ifelse(vol_gkyz_L < vthresh[13], 1, 0)
  sig_gkyz_ML <- ifelse(vol_gkyz_ML < vthresh[14], 1, 0)
  sig_gkyz_MS <- ifelse(vol_gkyz_MS < vthresh[15], 1, 0)
  sig_gkyz_S  <- ifelse(vol_gkyz_S < vthresh[16], 1, 0)
  
  sig_natr_L  <- ifelse(natr_L < lthresh[1], 1, 0)
  sig_natr_ML <- ifelse(natr_ML < lthresh[2], 1, 0)
  sig_natr_MS <- ifelse(natr_MS < lthresh[3], 1, 0)
  sig_natr_S  <- ifelse(natr_S < lthresh[4], 1, 0)
  
  #add up the signals
  totalvol <- (
    + sig_cc_L
    + sig_cc_ML
    + sig_cc_MS
    + sig_cc_S
    + sig_rs_L 
    + sig_rs_ML 
    + sig_rs_MS 
    + sig_rs_S  
    + sig_p_L 
    + sig_p_ML 
    + sig_p_MS 
    + sig_p_S 
    + sig_gkyz_L 
    + sig_gkyz_ML 
    + sig_gkyz_MS 
    + sig_gkyz_S 
  )
  
  totalnatr <- (
    + sig_natr_L
    + sig_natr_ML
    + sig_natr_MS
    + sig_natr_S
  )
  
  #look for any positive signal (or increase this threshold up to 20)
  #signal_2 <- ifelse(totalnatr >= 1, 1, 0) #only nATR
  #signal_2 <- ifelse(totalvol >= 1, 1, 0) #Multivol without nATR
  signal_2 <- ifelse(totalvol + totalnatr >= 1, 1, 0) #Multivol with nATR
  signal_2[is.na(signal_2)] <- 0
  
  # Step 4: Backtest the strategies
  returns_strategy2 <- roc_trade1 * stats::lag(signal_2, 2)
  returns_strategy2 <- na.omit(returns_strategy2)
  
  rtns <- returns_strategy2
  comparison3 <- cbind(comparison3, rtns) 
  print( exposure(rtns)) 
  
}
charts.PerformanceSummary(comparison3, main = "Random RV Strategies vs S&P 500 Benchmark")
#comment out the "stats_rv5" line below to improve speed of script
# at the expense of not having comprehensive stats to view
stats_rv5 <- rbind(table.AnnualizedReturns(comparison3), maxDrawdown(comparison3))
