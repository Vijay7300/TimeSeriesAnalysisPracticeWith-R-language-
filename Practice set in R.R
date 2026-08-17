library(fpp2)        # contains h02 and helpers
library(forecast)    # ARIMA, auto.arima, BoxCox, etc.
library(tseries)     # adf.test
library(ggplot2)
library(lmtest)      # for some tests
# load data
data("h02", package="fpp2")
h02
autoplot(h02) + ggtitle("h02 time series")


# (a) STL decomposition & describe trend/seasonality

library(fpp2)
autoplot(h02)
fit_stl <- stl(h02,s.window='periodic')
autoplot(fit_stl)+ggtitle('stl decomposition on h02')

# (b) 12-month moving average to inspect trend

ma12 <- ma(h02,order=12)
autoplot(h02)+autolayer(ma12,series='ma(12)')+ggtitle('h02 with 12-month ma')

# (c) Do data need transforming? (Box–Cox)
# suggested lambda from Guerrero method
lambda <- BoxCox.lambda(h02, method="guerrero")
lambda
# plot transformed series
autoplot(BoxCox(h02, lambda)) + ggtitle(paste("BoxCox transformed (lambda=", round(lambda,3),")"))


# (d) Are the data stationary? If not, find differencing to yield stationarity
# Tests for number of differences
nd <- ndiffs(h02, alpha=0.05, test="adf")      # non-seasonal differences
nsd <- nsdiffs(h02, m=frequency(h02))          # seasonal differences

nd; nsd

# apply differences suggested
h02_diff <- diff(h02, differences=nd)
if(nsd>0) h02_diff <- diff(h02_diff, lag=frequency(h02), differences=nsd)

# plot ACF/PACF of differenced series
ggtsdisplay(h02_diff, main="Differenced h02")
adf.test(na.omit(h02_diff))  # stationarity test


















