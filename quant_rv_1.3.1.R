### quant_rv v1.3.1 by babbage9010 and friends
# change 1: change colors: Strategy 1 is red, Benchmark is black
### released under MIT License

# Step 1: Load necessary libraries and data
library(quantmod)
library(PerformanceAnalytics)

date_start <- as.Date("2006-07-01")
date_end <- as.Date("2019-12-31")
symbol_bench1  <- "SPY"  # benchmark for comparison
symbol_signal1 <- "SPY"  # S&P 500 symbol (use SPY or ^GSPC)
symbol_trade1  <- "SPY"  # ETF to trade

data_spy <- getSymbols(symbol_bench1, src = "yahoo", from = date_start, to = date_end, auto.assign = FALSE)
prices_benchmark <- Ad(data_spy) #SPY ETF, Adjusted(Ad) for the benchmark
prices_signal1 <- Ad(data_spy) #SPY ETF, Adjusted(Ad) for the signal (realized vol)
prices_trade1 <- Op(data_spy) #SPY data, Open(Op) for our trading

# Step 2: Calculate ROC series
roc_signal1 <-   ROC(prices_signal1, n = 1, type = "discrete")
roc_benchmark <- ROC(prices_benchmark, n = 1, type = "discrete")
roc_trade1 <-    ROC(prices_trade1, n = 1, type = "discrete")

# Step 3: Develop the trading strategies
# Strategy 1: A benchmark strategy
lookback_period1 <- 20
threshold1 <- 0.15
label_strategy1 <- "Strategy 1: rv20d15"
volatility1 <- runSD(roc_signal1, n = lookback_period1) * sqrt(252)
signal_1 <- ifelse(volatility1 < threshold1, 1, 0)
signal_1[is.na(signal_1)] <- 0

# Strategy 2: The one plotted first, with Daily Returns shown
# we're using four measures of volatility with three lookback periods
label_strategy2 <- "Strategy 2: randomized multivol"

#lookback periods randomized
lookback_long <- floor(runif(4, min = 20, max = 25)) #20-25
lookback_med <- floor(runif(4, min = 12, max = 16)) #12-16
lookback_short <- floor(runif(4, min = 4, max = 8))   #4-8

#strategy volatility threshold randomized
vthresh <- runif(21, min = 0.12, max = 0.17)

#calculate all the volatility measures (12)
vol_cc_L <- volatility(data_spy, n = lookback_long[1], calc = "close")
vol_cc_M <- volatility(data_spy, n = lookback_med[1], calc = "close")
vol_cc_S <- volatility(data_spy, n = lookback_short[1], calc = "close")
vol_rs_L <- volatility(data_spy, n = lookback_long[2], calc = "rogers.satchell")
vol_rs_M <- volatility(data_spy, n = lookback_med[2], calc = "rogers.satchell")
vol_rs_S <- volatility(data_spy, n = lookback_short[2], calc = "rogers.satchell")
vol_p_L <- volatility(data_spy, n = lookback_long[3], calc = "parkinson")
vol_p_M <- volatility(data_spy, n = lookback_med[3], calc = "parkinson")
vol_p_S <- volatility(data_spy, n = lookback_short[3], calc = "parkinson")
vol_gkyz_L <- volatility(data_spy, n = lookback_long[4], calc = "gk.yz")
vol_gkyz_M <- volatility(data_spy, n = lookback_med[4], calc = "gk.yz")
vol_gkyz_S <- volatility(data_spy, n = lookback_short[4], calc = "gk.yz")

#calculate the signals
# note that volatility for Rogers-Satchell, Parkinson and GK-YZ
# all generate positive and negative values, so we signal a low vol zone for those
sig_cc_L <- ifelse(vol_cc_L < vthresh[1], 1, 0)
sig_cc_M <- ifelse(vol_cc_M < vthresh[2], 1, 0)
sig_cc_S <- ifelse(vol_cc_S < vthresh[3], 1, 0)
sig_rs_L <- ifelse(vol_rs_L > -vthresh[4] & vol_rs_L < vthresh[5], 1, 0)
sig_rs_M <- ifelse(vol_rs_M > -vthresh[6] & vol_rs_M < vthresh[7], 1, 0)
sig_rs_S <- ifelse(vol_rs_S > -vthresh[8] & vol_rs_S < vthresh[9], 1, 0)
sig_p_L <- ifelse(vol_p_L > -vthresh[10] & vol_p_L < vthresh[11], 1, 0)
sig_p_M <- ifelse(vol_p_M > -vthresh[12] & vol_p_M < vthresh[13], 1, 0)
sig_p_S <- ifelse(vol_p_S > -vthresh[14] & vol_p_S < vthresh[15], 1, 0)
sig_gkyz_L <- ifelse(vol_gkyz_L > -vthresh[16] & vol_gkyz_L < vthresh[17], 1, 0)
sig_gkyz_M <- ifelse(vol_gkyz_M > -vthresh[18] & vol_gkyz_M < vthresh[19], 1, 0)
sig_gkyz_S <- ifelse(vol_gkyz_S > -vthresh[20] & vol_gkyz_S < vthresh[21], 1, 0)

#add up the signals
totalvol <- (
  sig_cc_L
  + sig_cc_M
  + sig_cc_S
  + sig_rs_L 
  + sig_rs_M 
  + sig_rs_S  
  + sig_p_L 
  + sig_p_M 
  + sig_p_S 
  + sig_gkyz_L 
  + sig_gkyz_M 
  + sig_gkyz_S 
)

#look for any positive signal (or increase this threshold up to 12)
signal_2 <- ifelse(totalvol >= 1, 1, 0)
signal_2[is.na(signal_2)] <- 0

# Step 4: Backtest the strategies
returns_strategy1 <- roc_trade1 * stats::lag(signal_1, 2)
returns_strategy1 <- na.omit(returns_strategy1)
returns_strategy2 <- roc_trade1 * stats::lag(signal_2, 2)
returns_strategy2 <- na.omit(returns_strategy2)

# Calculate benchmark returns
returns_benchmark <- roc_benchmark 
returns_benchmark <- stats::lag(returns_benchmark, 2)
returns_benchmark <- na.omit(returns_benchmark)

# Step 5: Evaluate performance and risk metrics
# add an "exposure" metric (informative, not evaluative)
exposure <- function(vec){ sum(vec != 0) / length(vec) }
comparison <- cbind(returns_benchmark, returns_strategy1,returns_strategy2)
colnames(comparison) <- c("Benchmark SPY total return", label_strategy1, label_strategy2)
stats_rv <- rbind(table.AnnualizedReturns(comparison), maxDrawdown(comparison))
charts.PerformanceSummary(comparison, main = "Realized Vol Strategies vs S&P 500 Benchmark")
exposure_s2 <- exposure(returns_strategy2)
exposure_s1 <- exposure(returns_strategy1)
print( paste("Exposure for Strategy 2:", exposure_s2) ) 
print( paste("Exposure for Strategy 1:", exposure_s1) ) 

#Step 6: random generation/plotting of strategies
#srs <- as.xts(returns_benchmark)
comparison3 <- as.xts(returns_benchmark)
for(s in 1:20){

  # Strategy 2: The one plotted first, with Daily Returns shown
  # we're using four measures of volatility with three lookback periods
  label_strategy2 <- "Strategy 2: randomized multivol"
  
  #lookback periods randomized
  lookback_long <- floor(runif(4, min = 20, max = 25)) #20-25
  lookback_med <- floor(runif(4, min = 12, max = 16)) #12-16
  lookback_short <- floor(runif(4, min = 4, max = 8))   #4-8
  
  #strategy volatility threshold randomized
  vthresh <- runif(21, min = 0.12, max = 0.17)
  
  #calculate all the volatility measures (12)
  vol_cc_L <- volatility(data_spy, n = lookback_long[1], calc = "close")
  vol_cc_M <- volatility(data_spy, n = lookback_med[1], calc = "close")
  vol_cc_S <- volatility(data_spy, n = lookback_short[1], calc = "close")
  vol_rs_L <- volatility(data_spy, n = lookback_long[2], calc = "rogers.satchell")
  vol_rs_M <- volatility(data_spy, n = lookback_med[2], calc = "rogers.satchell")
  vol_rs_S <- volatility(data_spy, n = lookback_short[2], calc = "rogers.satchell")
  vol_p_L <- volatility(data_spy, n = lookback_long[3], calc = "parkinson")
  vol_p_M <- volatility(data_spy, n = lookback_med[3], calc = "parkinson")
  vol_p_S <- volatility(data_spy, n = lookback_short[3], calc = "parkinson")
  vol_gkyz_L <- volatility(data_spy, n = lookback_long[4], calc = "gk.yz")
  vol_gkyz_M <- volatility(data_spy, n = lookback_med[4], calc = "gk.yz")
  vol_gkyz_S <- volatility(data_spy, n = lookback_short[4], calc = "gk.yz")
  
  #calculate the signals
  # note that volatility for Rogers-Satchell, Parkinson and GK-YZ
  # all generate positive and negative values, so we signal a low vol zone for those
  sig_cc_L <- ifelse(vol_cc_L < vthresh[1], 1, 0)
  sig_cc_M <- ifelse(vol_cc_M < vthresh[2], 1, 0)
  sig_cc_S <- ifelse(vol_cc_S < vthresh[3], 1, 0)
  sig_rs_L <- ifelse(vol_rs_L > -vthresh[4] & vol_rs_L < vthresh[5], 1, 0)
  sig_rs_M <- ifelse(vol_rs_M > -vthresh[6] & vol_rs_M < vthresh[7], 1, 0)
  sig_rs_S <- ifelse(vol_rs_S > -vthresh[8] & vol_rs_S < vthresh[9], 1, 0)
  sig_p_L <- ifelse(vol_p_L > -vthresh[10] & vol_p_L < vthresh[11], 1, 0)
  sig_p_M <- ifelse(vol_p_M > -vthresh[12] & vol_p_M < vthresh[13], 1, 0)
  sig_p_S <- ifelse(vol_p_S > -vthresh[14] & vol_p_S < vthresh[15], 1, 0)
  sig_gkyz_L <- ifelse(vol_gkyz_L > -vthresh[16] & vol_gkyz_L < vthresh[17], 1, 0)
  sig_gkyz_M <- ifelse(vol_gkyz_M > -vthresh[18] & vol_gkyz_M < vthresh[19], 1, 0)
  sig_gkyz_S <- ifelse(vol_gkyz_S > -vthresh[20] & vol_gkyz_S < vthresh[21], 1, 0)
  
  #add up the signals
  totalvol <- (
    sig_cc_L
    + sig_cc_M
    + sig_cc_S
    + sig_rs_L 
    + sig_rs_M 
    + sig_rs_S  
    + sig_p_L 
    + sig_p_M 
    + sig_p_S 
    + sig_gkyz_L 
    + sig_gkyz_M 
    + sig_gkyz_S 
  )
  
  #look for any positive signal (or increase this threshold up to 12)
  signal_2 <- ifelse(totalvol >= 1, 1, 0)
  signal_2[is.na(signal_2)] <- 0
  
  # Step 4: Backtest the strategies
  returns_strategy2 <- roc_trade1 * stats::lag(signal_2, 2)
  returns_strategy2 <- na.omit(returns_strategy2)
  
  rtns <- returns_strategy2
  comparison3 <- cbind(comparison3, rtns) 
  print( exposure(rtns)) 
  
}
charts.PerformanceSummary(comparison3, main = "Random RV Strategies vs S&P 500 Benchmark")
stats_rv5 <- rbind(table.AnnualizedReturns(comparison3), maxDrawdown(comparison3))
