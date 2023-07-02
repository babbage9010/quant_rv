### quant_rv v1.0.1 by babbage9010 and friends
# cleanup and stats release
### released under MIT License

# Step 1: Load necessary libraries and data
library(quantmod)
library(PerformanceAnalytics)

#bab: sensible var names, type_description
date_start <- as.Date("2000-01-01")
date_end <- as.Date("2021-12-31")
symbol <- "SPY"  # SPY ETF symbol 

#bab: reorient this so the variable name is in front
data_spy <- getSymbols(symbol, src = "yahoo", from = date_start, to = date_end, auto.assign = FALSE)
#bab: rename these, separate trade from signal from benchmark
prices_benchmark <- Ad(data_spy) #SPY ETF, Adjusted(Ad) for the benchmark
prices_signal <- Cl(data_spy) #SPY ETF, Close(Cl) for the signal (realized vol)
prices_trade <- Op(data_spy) #SPY data, Open(Op) for our trading

# Step 2: Calculate volatility as a risk indicator
lookback_period <- 20
#bab: rename, separate ROC components
roc_signal <-    ROC(prices_signal, n = 1, type = "discrete")
roc_benchmark <- ROC(prices_benchmark, n = 1, type = "discrete")
roc_trade <-     ROC(prices_trade, n = 1, type = "discrete")
#bab: vol formula now easier to read - SD of the daily ROC, annualized
volatility <- runSD(roc_signal, n = lookback_period) * sqrt(252)

# Step 3: Develop the trading strategy
threshold <- 0.15
signal <- ifelse(volatility < threshold, 1, 0)
signal[is.na(signal)] <- 0

# Step 4: Backtest the strategy
returns_strategy <- roc_trade * Lag(signal, 2)
returns_strategy <- na.omit(returns_strategy)

# Calculate benchmark returns
returns_benchmark <- roc_benchmark 
returns_benchmark <- Lag(returns_benchmark, 2)
returns_benchmark <- na.omit(returns_benchmark)

# Step 5: Evaluate performance and risk metrics
#switch the order to switch colors
comparison <- cbind(returns_strategy, returns_benchmark)
colnames(comparison) <- c("Strategy", "Benchmark")
#bab: new line for basic statistics in a table
stats_rv <- rbind(table.AnnualizedReturns(comparison), maxDrawdown(comparison), AverageRecovery(comparison))

charts.PerformanceSummary(comparison, main = "Long/Flat Strategy vs S&P 500 Benchmark")
