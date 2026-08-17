'''  Q1  Consider the dataset elecequip from fpp2 package. Consider the dataset till
dec 2007 as training data and remaining dataset as test data. Fit an appropri-
ate SARIMA model on train data and also obtain the model using auto.arima on
train data. Based on RMSE, determine which model is best for forecasting on test
data.'''

library(fpp2)
data('elecequip')
autoplot(elecequip)


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

autoplot(fc_manual) + autolayer(test, series="Test Data", PI=FALSE)
autoplot(fc_auto) + autolayer(test, series="Test Data", PI=FALSE)


''' Q10  Consider the dataset debitcards from fpp2 package. Consider the dataset till
July 2006 as train dataset and remaining as test dataset. Based on RMSE, out of
auto.arima and ETS model which model is best for forecasting.'''

library(fpp2)
data('debitcards')
autoplot(debitcards)

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

autoplot(fc_ets) + autolayer(test, series="Test Data", PI=FALSE)
autoplot(fc_arima) + autolayer(test, series="Test Data", PI=FALSE)



''' Q9 Consider the dataset debitcards from fpp2 package. Using Cross-validation tech-
nique, out of auto.arima and ETS model which model is best for 2-step ahead
forecasting.'''


library(fpp2)
library(forecast)
library(tseries)

data('debitcards')
autoplot(debitcards)

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





