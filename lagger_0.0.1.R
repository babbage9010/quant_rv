### lagger v0.0.1 by babbage9010 and friends
### released under MIT License
# 
# Simple backtesting demo to understand how lag() works
#

# Load the necessary libraries
library(quantmod)
library(PerformanceAnalytics)

# Generate date sequence from Jan 1, 2023 to Feb 28, 2023
dates <- seq(from = as.Date("2023-01-01"), to = as.Date("2023-02-13"), by = "days")

# Create a matrix with labeled columns
data_matrix <- matrix(
  c(1.1, 1.2, 1.0, 1.1, 1.1, 0),
  nrow = length(dates),
  ncol = 6,
  byrow = TRUE,
  dimnames = list(dates, c("Open", "High", "Low", "Close", "Adjusted Close", "Volume"))
)

# Create the XTS object
my_xts <- xts(data_matrix, order.by = dates)

# Apply the specified pattern to the relevant columns
for (i in seq_along(my_xts[,1])) {
  if (i %% 10 == 0 && i > 3) {
    # Every 10th day: Increase Open, High, Low, Close, Adjusted Close by 0.02
    my_xts[i, c("Open", "High", "Low", "Close", "Adjusted Close")] <- my_xts[i, c("Open", "High", "Low", "Close", "Adjusted Close")] + 0.02
  } else if ((i - 1) %% 10 == 0 && i > 3) {
    # The day after the 10th day: Increase Open, High, Low, Close, Adjusted Close by 0.01
    my_xts[i, c("Open", "High", "Low", "Close", "Adjusted Close")] <- my_xts[i, c("Open", "High", "Low", "Close", "Adjusted Close")] + 0.03
  } 
}


#strategy
prices_close <- Ad(my_xts)
prices_open <- Op(my_xts)
roc_close <- ROC(prices_close, n = 1, type = "discrete")
roc_open <- ROC(prices_open, n = 1, type = "discrete")

returns_benchmark <- roc_close 
returns_benchmark <- na.omit(returns_benchmark)

comparison1 <- cbind(returns_benchmark)
colnames(comparison1) <- c("Benchmark: Up up DOWN")
charts.PerformanceSummary(comparison1, main = "Testing Lag Strategies")

#strategies testing lag
roc_thresh <- 0.015

roc_trade <- roc_open #substitute roc_close to look at the close!
signal <- ifelse(roc_close >= roc_thresh, 1, 0) 

#lag(0)
returns_strategy0 <- roc_trade * stats::lag(signal, 0)
returns_strategy0 <- na.omit(returns_strategy0)

#lag(1)
returns_strategy1 <- roc_trade * stats::lag(signal, 1)
returns_strategy1 <- na.omit(returns_strategy1)

#lag(2)
returns_strategy2 <- roc_trade * stats::lag(signal, 2)
returns_strategy2 <- na.omit(returns_strategy2)

comparison2 <- cbind(returns_benchmark, returns_strategy0, returns_strategy1, returns_strategy2)
colnames(comparison2) <- c("Benchmark: Up up DOWN", "Lag(0) Time Travel", "Lag(1) Magical Thinking", "Lag(2) Next Day")
stats_rv <- rbind(table.AnnualizedReturns(comparison2), maxDrawdown(comparison2))
charts.PerformanceSummary(comparison2, main = "Testing Lag Strategies")



