### vol_compare v0.0.2 by babbage9010 and friends
# initial release
### released under MIT License

library(quantmod)

#get the data
date_start <- as.Date("2006-07-01")
date_end <- as.Date("2019-12-31")
symbol_signal1 <- "SPY"  # S&P 500 symbol (use SPY or ^GSPC)
data_spy <- getSymbols(symbol_signal1, src = "yahoo", from = date_start, to = date_end, auto.assign = FALSE)

#here we calculate vol
volatility_20d_gkyz <- volatility(data_spy, n=20, calc = "gk.yz")
volatility_20d_c2c <- volatility(data_spy, n=20, calc = "close")
volatility_20d_park <- volatility(data_spy, n=20, calc = "parkinson")
volatility_20d_rosa <- volatility(data_spy, n=20, calc = "rogers.satchell")
volatility_20d_yazh <- volatility(data_spy, n=20, calc = "yang.zhang")

#for plotting, just the data
vol_20day_GKYZ = coredata(volatility_20d_gkyz) 
vol_20day_C2C = coredata(volatility_20d_c2c)

#plot it
pl1 <- plot.new()
pl1 <- plot(x=vol_20day_C2C,y=vol_20day_GKYZ, main = "C2C vol vs GKYZ vol")
pl1 <- abline(reg = lm(vol_20day_GKYZ ~ vol_20day_C2C), col = "red", lwd = 2)

#let's also look at daily returns (vol direction)
roc_volatility_gkyz <- ROC(volatility_20d_gkyz, n = 1, type = "continuous")
roc_volatility_c2c <- ROC(volatility_20d_c2c, n = 1, type = "continuous")
roc_volatility_park <- ROC(volatility_20d_park, n = 1, type = "continuous")
roc_volatility_rosa <- ROC(volatility_20d_rosa, n = 1, type = "continuous")
roc_volatility_yazh <- ROC(volatility_20d_yazh, n = 1, type = "continuous")

#for plotting, just the data
roc_vol_20day_GKYZ = coredata(roc_volatility_gkyz) 
roc_vol_20day_C2C = coredata(roc_volatility_c2c)
roc_vol_20day_PARK = coredata(roc_volatility_park)
roc_vol_20day_ROSA = coredata(roc_volatility_rosa)
roc_vol_20day_YAZH = coredata(roc_volatility_yazh)

#plot it
pl2 <- plot.new()
pl2 <- plot(x=roc_vol_20day_C2C,y=roc_vol_20day_GKYZ, main = "Daily Returns: C2C vol vs GKYZ vol")
pl2 <- abline(reg = lm(roc_vol_20day_GKYZ ~ roc_vol_20day_C2C), col = "red", lwd = 2)
#pl2 <- plot(x=roc_vol_20day_PARK,y=roc_vol_20day_GKYZ, main = "Daily Returns: PARK vol vs GKYZ vol")
#pl2 <- abline(reg = lm(roc_vol_20day_GKYZ ~ roc_vol_20day_PARK), col = "red", lwd = 2)

comp <- c()
comp <- cbind(comp, roc_vol_20day_C2C, roc_vol_20day_PARK, roc_vol_20day_ROSA, roc_vol_20day_GKYZ, roc_vol_20day_YAZH )
colnames(comp)<- c("C2C", "Parkinson", "Rogers-Satchell", "Garman-Klass,Yang-Zhang", "Yang-Zhang")
#pairs(comp, lower.panel = NULL)
#pairs(comp)
comp <- na.omit(comp)
cor <- cor(comp, method="pearson")

#ok, now remove YAZH because it's too nearly identical to GKYZ, at least with default params
comp2 <- c()
comp2 <- cbind(comp2, roc_vol_20day_C2C, roc_vol_20day_PARK, roc_vol_20day_ROSA, roc_vol_20day_GKYZ )
colnames(comp2)<- c("C2C", "Parkinson", "Rogers-Satchell", "Garman-Klass,Yang-Zhang")
#pairs(comp2, lower.panel = NULL)
pairs(comp2)
comp2 <- na.omit(comp2)
cor2 <- cor(comp2, method="pearson")
print(cor2)
