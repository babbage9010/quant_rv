### Demo-Death-Cross.R by babbage9010 and friends
### released under MIT License
# Simple "Golden Death" strategy
#  Signal is true at daily close if SMA(SPY,50) > SMA(SPY,200)
#  Strategy goes long at the open on the next day, or sells at the open
#    if signal returns 0
# variable "sdp" is used to display the proper plot of strategy vs benchmark
# without comparing the 200 day lag while SMA(SPY,200) is being calculated
# Some commented out code was used to make the other plots in the blog post
# originally published Nov 23 2023

# Step 1: Load necessary libraries and data
library(quantmod)
library(PerformanceAnalytics)

#dates and symbols for gathering data
#setting to start of 2021
#date_start <- as.Date("2021-01-01") 
#setting back 200 trading days
#date_start <- as.Date("2020-03-18") 
#setting to start of SPY trading
date_start <- as.Date("1993-01-29")
date_end <- as.Date("2034-12-31") #a date in the future
symbol_benchmark1  <- "SPY"  # benchmark for comparison
symbol_signal1 <- "SPY"  # S&P 500 symbol (use SPY or ^GSPC)
symbol_trade1  <- "SPY"  # ETF to trade

#get data from yahoo
data_benchmark1 <- getSymbols(symbol_benchmark1, src = "yahoo", from = date_start, to = date_end, auto.assign = FALSE)
data_signal1 <- getSymbols(symbol_signal1, src = "yahoo", from = date_start, to = date_end, auto.assign = FALSE)
data_trade1 <- getSymbols(symbol_trade1, src = "yahoo", from = date_start, to = date_end, auto.assign = FALSE)

#use these prices
prices_benchmark1 <- Ad(data_benchmark1) #Adjusted(Ad) for the #1 benchmark
prices_signal1 <- Ad(data_signal1) #Adjusted(Ad) for the signal 
prices_trade1 <- Op(data_trade1) #Open(Op) for our trading

#calculate 1 day returns (rate of change) 
roc_benchmark1 <- ROC(prices_benchmark1, n = 1, type = "discrete")
roc_signal1 <- ROC(prices_signal1, n = 1, type = "discrete")
roc_trade1 <- ROC(prices_trade1, n = 1, type = "discrete")

#the strategy
spy50 <- SMA(prices_signal1, 50)
spy200 <- SMA(prices_signal1, 200)
signal_1 <- ifelse(spy50 >= spy200, 1, 0)
signal_1[is.na(signal_1)] <- 0

#calculate returns
returns_benchmark1 <- stats::lag(roc_benchmark1, 0) 
returns_benchmark1 <- na.omit(returns_benchmark1)
label_benchmark1 <- "Benchmark SPY total return"

returns_strategy1 <- roc_trade1 * stats::lag(signal_1, 2) 
returns_strategy1 <- na.omit(returns_strategy1)
label_strategy1 <- "Golden Death"

#combine returns into one XTS object, add column names
comparison <- cbind(returns_strategy1, returns_benchmark1)
colnames(comparison) <- c(label_strategy1, label_benchmark1)

#default chart and stats: uses full data downloaded
#charts.PerformanceSummary(comparison, main = "Golden Death Strategy vs S&P 500 Benchmark - default")
#stats_default <- rbind(table.AnnualizedReturns(comparison), maxDrawdown(comparison))

#trimmed plot and stats
# sdp = start date for plotting
sdp <- "2021-01-01/" #start date for our plot in this blog post
#other sample sdp examples
#sdp <- "/"  # same as default, use all the data downloaded
#sdp <- "/1995-12-31" # all data to end of 1995
#sdp <- "1993-11-13/1995-12-31" # start after 200 days, to end of 1995
charts.PerformanceSummary(comparison[sdp], main = "Golden Death Strategy vs S&P 500 Benchmark - trimmed")
stats_gd <- rbind(table.AnnualizedReturns(comparison[sdp]), maxDrawdown(comparison[sdp]))
