### quant_rv v1.2.0 by babbage9010 and friends
# new setup to compare two strategies and parameters
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
prices_signal1 <- Cl(data_spy) #SPY ETF, Close(Cl) for the signal (realized vol)
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

# Strategy 2: The one plotted first, with Daily Returns
lookback_period2 <- 22
threshold2 <- 0.17
label_strategy2 <- "Strategy 2: rv22d17"
volatility2 <- runSD(roc_signal1, n = lookback_period2) * sqrt(252)
signal_2 <- ifelse(volatility2 < threshold2, 1, 0)
signal_2[is.na(signal_2)] <- 0

# Step 4: Backtest the strategies
returns_strategy1 <- roc_trade1 * Lag(signal_1, 2)
returns_strategy1 <- na.omit(returns_strategy1)
returns_strategy2 <- roc_trade1 * Lag(signal_2, 2)
returns_strategy2 <- na.omit(returns_strategy2)

# Calculate benchmark returns
returns_benchmark <- roc_benchmark 
returns_benchmark <- Lag(returns_benchmark, 2)
returns_benchmark <- na.omit(returns_benchmark)

# Step 5: Evaluate performance and risk metrics
# add an "exposure" metric (informative, not evaluative)
exposure <- function(vec){ sum(vec != 0) / length(vec) }
comparison <- cbind(returns_strategy2, returns_benchmark, returns_strategy1)
colnames(comparison) <- c(label_strategy2, "Benchmark SPY total return", label_strategy1)
stats_rv <- rbind(table.AnnualizedReturns(comparison), maxDrawdown(comparison))
charts.PerformanceSummary(comparison, main = "Realized Vol Strategies vs S&P 500 Benchmark")
exposure_s2 <- exposure(returns_strategy2)
exposure_s1 <- exposure(returns_strategy1)
print( paste("Exposure for Strategy 2:", exposure_s2) ) 
print( paste("Exposure for Strategy 1:", exposure_s1) ) 

