'''  Q1  Consider the dataset elecequip from fpp2 package. Consider the dataset till
dec 2007 as training data and remaining dataset as test data. Fit an appropri-
ate SARIMA model on train data and also obtain the model using auto.arima on
train data. Based on RMSE, determine which model is best for forecasting on test
data.'''

library(fpp2)
data('elecequip')
print(data)

train <- window(elecequip,end=c(2007,12))
test <- window(elecequip,start=c(2008,1))
print(train)
print(test)

fit_manual <- Arima(train,order = c(0,1,1),seasonal = c(0,1,1))
summary(fit_manual)

fit_auto <- auto.arima(train)
summary(fit_auto)

fc_maual <- forecast(fit_manual,h=length(test))
fc_auto <- forecast(fit_auto,h=length(test))

rmse_manual <- accuracy(fc_maual,test)[2, 'RMSE']
rmse_auto <- accuracy(fc_auto,test)[2, 'RMSE']


''' Q10  Consider the dataset debitcards from fpp2 package. Consider the dataset till
July 2006 as train dataset and remaining as test dataset. Based on RMSE, out of
auto.arima and ETS model which model is best for forecasting.'''

library(fpp2)
data('debitcards')

train <-window(debitcards,end=c(2006,7))
test <- window(debitcards,start=c(2007,8))
print(train)

fit_ets <- ets(train)
summary(fit_ets)

fit_arima <- auto.arima(train)
summary(fit_arima)


fc_ets <- forecast(fit_ets,h=length(test))
fc_arima <- forecast(fit_arima,h=length(test))

rmse_ets <- accuracy(fc_ets,test)[2, 'RMSE']
rmse_arima <- accuracy(fc_arima,test)[2, 'RMSE']


''' Q9 Consider the dataset debitcards from fpp2 package. Using Cross-validation tech-
nique, out of auto.arima and ETS model which model is best for 2-step ahead
forecasting.'''


library(fpp2)
library(forecast)
library(tseries)

data('debitcards')


fc_arima <- function(y,h){forecast(auto.arima(y),h=h)}
fc_ets <- function(y,h){forecast(ets(y),h=h)}

e_arima <- tsCV(debitcards,fc_arima,h=2)
e_ets <- tsCV(debitcards,fc_ets,h=2)


rmse_arima <- sqrt(mean(e_arima^2 , na.rm = TRUE))
rmse_ets <- sqrt(mean(e_ets^2 , na.rm = TRUE))
print(rmse_arima)
print(rmse_ets)


''' Q8 Consider the dataset debitcards from fpp2 package. Determine which ETS model
is fitted well. Now, do 10 step ahead forecarsting and plot it.'''


library(fpp2)
library(forecast)
library(tseries)

data('debitcards')

fit_ets <- ets(debitcards)
summary(fit_ets)

fc_ets <- forecast(fit_ets,h=10)
print(fc_ets)

autoplot(fc_ets) +
  ggtitle("10-Step Ahead Forecast using ETS Model") +
  xlab("Year") + ylab("Number of Debit Card Transactions")



fit_ets_all <- ets(debitcards, model = "ZZZ")  # automatic selection
fit_ets_add <- ets(debitcards, model = "AAA")  # additive model
fit_ets_mul <- ets(debitcards, model = "MMM")  # fully multiplicative

fit_ets_all$aic
fit_ets_add$aic
fit_ets_mul$aic


''' Q2 Consider the dataset auscafe from fpp2 package. Fit the model using auto.arima
with bias-adjustment and also using auto.arima having simple back transformation.
For the obtained models, forecast the values and plot it.'''

library(fpp2)

data("auscafe")
autoplot(auscafe) + ggtitle("Australian Café Expenditure")

y <- log(auscafe)
fit_bias <- auto.arima(y, biasadj = TRUE)
fit_simple <- auto.arima(y)

fc_bias <- forecast(fit_bias)
fc_simple <- forecast(fit_simple)

# for back-transform
fc_bias$mean <- exp(fc_bias$mean)
fc_simple$mean <- exp(fc_simple$mean)


autoplot(auscafe) +
  autolayer(fc_bias$mean, series = "Bias-adjusted Forecast", color = "blue") +
  autolayer(fc_simple$mean, series = "Simple Back-transform Forecast", color = "red") +
  ggtitle("Forecast Comparison: Bias-adjusted vs Simple ARIMA") +
  xlab("Year") +
  ylab("Café Expenditure") +
  guides(colour = guide_legend(title = "Forecast Type"))


'''
3. Consider the dataset qcement from fpp2 package. Fit an appropriate SARIMA
model and obtain the model using auto.arima. For both models compute Cross-
Validation error for h = 4. Out of these two, determine which model is best for forecasting.'''


# Forecasting qcement: Manual SARIMA vs auto.arima

library(fpp2)

# Load data
ts <- qcement

fit_manual <- Arima(ts, order = c(1,1,1), seasonal = c(0,1,1))
summary(fit_manual)

fit_auto <- auto.arima(ts)
summary(fit_auto)


# CV for Manual Model
manual_cv <- tsCV(
  ts,
  forecastfunction = function(y) forecast(
    Arima(y, order = c(1,1,1), seasonal = c(0,1,1))
  ),
  h = 4
)

# CV for auto.arima Model
auto_cv <- tsCV(
  ts,
  forecastfunction = function(y) forecast(auto.arima(y)),
  h = 4
)

manual_rmse <- sqrt(mean(manual_cv^2, na.rm = TRUE))
auto_rmse <- sqrt(mean(auto_cv^2, na.rm = TRUE))

manual_rmse
auto_rmse

cat("RMSE (Manual SARIMA):", manual_rmse, "\n")
cat("RMSE (auto.arima):", auto_rmse, "\n")

if (auto_rmse < manual_rmse) {
  cat("Conclusion: auto.arima model performs better for forecasting.\n")
} else {
  cat("Conclusion: Manual SARIMA model performs better for forecasting.\n")
}


''' 4. Consider the dataset maxtemp from fpp2 package. Consider the dataset upto
year 2004 as train data and remaining as test data. Now, do forecasting using
average method, naive method, drift method and determine the accuracy of each
method on test dataset.'''


library(fpp2)

# Load data
data("maxtemp")
ts <- maxtemp


train <- window(ts, end = c(2004))
test  <- window(ts, start = c(2005))

length(train)
length(test)


f_mean  <- meanf(train, h = length(test))
f_naive <- naive(train, h = length(test))
f_drift <- rwf(train, h = length(test), drift = TRUE)


acc_mean  <- accuracy(f_mean, test)
acc_naive <- accuracy(f_naive, test)
acc_drift <- accuracy(f_drift, test)

acc_mean
acc_naive
acc_drift



''' 5. Consider the dataset marathon from fpp2 package. Using Simple exponential
smoothing forecasting method, Holt method and damped trend method(φ = 0.89),
do the 7-step ahead forecasting and plot it.''' 

library(fpp2)

# Load dataset
data("marathon")
ts <- marathon

fit_ses <- ses(ts, h = 7)
summary(fit_ses)


fit_holt <- holt(ts, h = 7)
summary(fit_holt)

fit_damped <- holt(ts, damped = TRUE, phi = 0.89, h = 7)
summary(fit_damped)

autoplot(ts) +
  autolayer(fit_ses, series = "SES") +
  autolayer(fit_holt, series = "Holt") +
  autolayer(fit_damped, series = "Damped Trend") +
  ggtitle("7-Step Ahead Forecasting for marathon Dataset") +
  xlab("Time") + ylab("Marathon Data") +
  guides(colour = guide_legend(title = "Methods"))



''' 6. Consider the dataset guinerice from fpp2 package. Consider the dataset till 2002
as train dataset and remaining as test dataset. Now, compare Simple exponential
smoothing forecasting method, Holt method and damped trend method(φ = 0.88).'''

library(fpp2)

# Load dataset
data("guinearice")
ts <- guinearice


train <- window(ts, end = c(2002))
test  <- window(ts, start = c(2003))

length(train)
length(test)

# Simple Exponential Smoothing
fit_ses <- ses(train, h = length(test))

# Holt's Method
fit_holt <- holt(train, h = length(test))

# Damped Trend Method (phi = 0.88)
fit_damped <- holt(train, damped = TRUE, phi = 0.88, h = length(test))


acc_ses    <- accuracy(fit_ses, test)
acc_holt   <- accuracy(fit_holt, test)
acc_damped <- accuracy(fit_damped, test)

acc_ses
acc_holt
acc_damped


''' 7. Consider the dataset h02 from fpp2 package. Consider the dataset till dec 2003
as train data and remaining dataset as test data. Now, based on RMSE, compare
Additive Holt-Winter, Multiplicative Holt-Winter, Damped additive Holt-Winter
and Damped multiplicative Holt-Winter methods.'''



library(fpp2)

# Load dataset
data("h02")
ts <- h02

train <- window(ts, end = c(2003, 12))
test  <- window(ts, start = c(2004, 1))

# Additive Holt-Winter
fit_add <- hw(train, seasonal = "additive", h = length(test))

# Multiplicative Holt-Winter
fit_mul <- hw(train, seasonal = "multiplicative", h = length(test))

# Damped Additive Holt-Winter
fit_add_damp <- hw(train, seasonal = "additive", damped = TRUE, h = length(test))

# Damped Multiplicative Holt-Winter
fit_mul_damp <- hw(train, seasonal = "multiplicative", damped = TRUE, h = length(test))

acc_add      <- accuracy(fit_add, test)
acc_mul      <- accuracy(fit_mul, test)
acc_add_damp <- accuracy(fit_add_damp, test)
acc_mul_damp <- accuracy(fit_mul_damp, test)

# Display results
acc_add
acc_mul
acc_add_damp
acc_mul_damp
































