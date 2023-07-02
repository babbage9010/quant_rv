### quant_rv v1.0.0 by babbage9010 and friends
### released under MIT License

# Step 1: Load necessary libraries and data
library(quantmod)
library(PerformanceAnalytics)

start_date <- as.Date("2000-01-01")
end_date <- as.Date("2021-12-31")
symbol <- "SPY"  # SPY ETF symbol 

#bab: changed the name to spy_data
getSymbols(symbol, src = "yahoo", from = start_date, to = end_date, auto.assign = FALSE) -> spy_data 
benchmark_data <- Cl(spy_data) #bab: use SPY data, Close(Cl) for the benchmark
price_data <- Op(spy_data) #bab: use new SPY data, Open for our trading

# Step 2: Calculate volatility as a risk indicator
lookback_period <- 20
volatility <- runSD(ROC(price_data, n = 1, type = "discrete"), n = lookback_period) * sqrt(252)

# Step 3: Develop the trading strategy
threshold <- 0.15
signal <- ifelse(volatility < threshold, 1, 0)
#bab: comment out this lag line, remove it later
#signal <- lag(signal, 1)  # To avoid look-ahead bias
signal[is.na(signal)] <- 0

# Step 4: Backtest the strategy
#bab: change the lag!
returns <- ROC(price_data, n = 1, type = "discrete") * Lag(signal, 2)
strategy_returns <- na.omit(returns)

# Calculate benchmark returns
benchmark_returns <- ROC(benchmark_data, n = 1, type = "discrete") 
#bab: add a new line to lag the benchmark too! original code missed this
benchmark_returns <- Lag(benchmark_returns, 2)
benchmark_returns <- na.omit(benchmark_returns)

# Step 5: Evaluate performance and risk metrics
#switch the order to switch colors
comparison <- cbind(strategy_returns, benchmark_returns)
colnames(comparison) <- c("Strategy", "Benchmark")

charts.PerformanceSummary(comparison, main = "Long/Flat Strategy vs S&P 500 Benchmark")
