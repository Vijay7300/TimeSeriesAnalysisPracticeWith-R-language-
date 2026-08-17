''' Select a time series from “Quandl” package.
Then copy its short URL and import the data using
 y <−Quandl(“?????”, api key= “?????”, type= “ts”) 
(Replace each ????? with the appropriate values.)'''


# 1. Load packages
install.packages(c("Quandl","forecast"))
library(Quandl)
library(forecast)
y <- Quandl("FRED/GDP", api_key = "YOUR_API_KEY_HERE", type = "ts")


#  1. Plot graphs of the data.
plot(y,main='US GDP (FRED/GDP)',ylab='billions of $',col='blue')



# 2. Create appropriate train and test sets.( Train-test split (e.g., last 20 quarters = test))
n <- length(y)
train <- window(y,end=time(y)[n-20])
test <- window(y,start=time(y)[n-19])
n
train
test


# 3. Try to identify an appropriate ARIMA model based on the train set.
fit_arima <- arima(train)
summary(fit_arima)


# 4. Do residual diagnostic checking of your ARIMA model. Are the residuals white noise?
acf(residuals(fit_arima),type='correlation',plot = T)
Box.test(residuals(fit_arima),type = 'Ljung-Box') # not white noise
# → If p-value > 0.05 → residuals ≈ white noise


#  5. Use your chosen ARIMA model to forecast for the length of test set
library(forecast)
fc_arima <- forecast(fit_arima, h=length(test))
autoplot(fc_arima)+autolayer(test,series='Test')


#  6. Nowtry to identify an appropriate ETS model based on the same train set.
fit_ets <- ets(train)
summary(fit_ets)
 

#  7. Do residual diagnostic checking of your ETS model. Are the residuals white noise?
par(mfrow=c(1,2))
acf(residuals(fit_ets),type = 'correlation',plot = T)
Box.test(residuals(fit_ets),type = 'Ljung-Box')
# it is a white noise because p=0.2>0.05


#  8. Use your chosen ETS model to forecast for the length of test set.
library(forecast)
fc_ets <- forecast(fit_ets,h=length(test))
autoplot(fc_ets)+autolayer(test,series = 'Test')


#  9. Which of the two models do you prefer and why? ( Compare accuracy on test set)

acc_arima <- accuracy(fc_arima,test)
acc_ets <- accuracy(fc_ets,test)
print(acc_arima)
print(acc_ets)  # Lower RMSE and MAE → better model on test data (ets model is better)


''' 10. Do you change your conclusion if the cross validation error
is used as a measure of accuracy  '''


#  Cross-validation errors
e_arima <- tsCV(train, forecastfunction = function(x,h) forecast(auto.arima(x), h=h), h=1)
e_ets   <- tsCV(train, forecastfunction = function(x,h) forecast(ets(x), h=h), h=1)

cv_arima <- sqrt(mean(e_arima^2, na.rm=TRUE))
cv_ets   <- sqrt(mean(e_ets^2, na.rm=TRUE))

print(paste("CV RMSE ARIMA:", round(cv_arima,2)))
print(paste("CV RMSE ETS:", round(cv_ets,2)))
# Lower RMSE  → better model on test data (arima model is better on CV)

























































