### quant_rv v1.1.0 by babbage9010 and friends
# explore use of QQQ and of leverage
### released under MIT License

# Step 1: Load necessary libraries and data
library(quantmod)
library(PerformanceAnalytics)

date_start <- as.Date("2006-07-01")
date_end <- as.Date("2019-12-31")
symbol_bench1  <- "SPY"  # benchmark for comparison
symbol_signal1 <- "SPY"  # S&P 500 symbol (use SPY or ^GSPC)
symbol_trade1  <- "SPY"  # ETF to trade
symbol_trade2  <- "QQQ"  # ETF to trade

data_spy <- getSymbols(symbol_bench1, src = "yahoo", from = date_start, to = date_end, auto.assign = FALSE)
data_qqq <- getSymbols(symbol_trade2, src = "yahoo", from = date_start, to = date_end, auto.assign = FALSE)
prices_benchmark <- Ad(data_spy) #SPY ETF, Adjusted(Ad) for the benchmark
prices_signal <- Cl(data_spy) #SPY ETF, Close(Cl) for the signal (realized vol)
prices_trade1 <- Op(data_spy) #SPY data, Open(Op) for our trading
prices_trade2 <- Op(data_qqq) #QQQ data, Open(Op) for our trading

# Step 2: Calculate volatility as a risk indicator
lookback_period <- 20
roc_signal <-    ROC(prices_signal, n = 1, type = "discrete")
roc_benchmark <- ROC(prices_benchmark, n = 1, type = "discrete")
roc_trade1 <-    ROC(prices_trade1, n = 1, type = "discrete")
roc_trade2 <-    ROC(prices_trade2, n = 1, type = "discrete")
volatility <- runSD(roc_signal, n = lookback_period) * sqrt(252)

# Step 3: Develop the trading strategy
threshold <- 0.15
signal <- ifelse(volatility < threshold, 1, 0)
signal[is.na(signal)] <- 0

# Step 4: Backtest the strategies
leverage_strategy1 <- 1.0 #use this to simulate a leveraged ETF component
leverage_strategy2 <- 1.0
returns_strategy1 <- leverage_strategy1 * roc_trade1 * Lag(signal, 2)
returns_strategy1 <- na.omit(returns_strategy1)
returns_strategy2 <- leverage_strategy2 * roc_trade2 * Lag(signal, 2)
returns_strategy2 <- na.omit(returns_strategy2)

# Calculate benchmark returns
returns_benchmark <- roc_benchmark 
returns_benchmark <- Lag(returns_benchmark, 2)
returns_benchmark <- na.omit(returns_benchmark)

# Step 5: Evaluate performance and risk metrics
#switch the order to switch colors
comparison <- cbind(returns_strategy1, returns_benchmark, returns_strategy2)
colnames(comparison) <- c("Strategy1", "Benchmark", "Strategy2")
stats_rv <- rbind(table.AnnualizedReturns(comparison), maxDrawdown(comparison), AverageRecovery(comparison))

charts.PerformanceSummary(comparison, main = "Long/Flat Strategy vs S&P 500 Benchmark")
