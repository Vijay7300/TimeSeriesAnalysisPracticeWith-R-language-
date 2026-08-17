''' 
Question 1:
Dataset: arrivals data from the fpp2 package
Subject: Quarterly international arrivals (in thousands) to Australia
Focus: New Zealand arrivals (1981 Q1 – 2012 Q3) '''

# (a) Data Division: Training and Test Sets

library(fpp2)

data('arrivals')
nz <- arrivals[,'NZ']
autoplot(nz)+ggtitle("Quarterly Arrivals from New Zealand to Australia")

train <- window(nz,end=c(2007,2))
test <- window(nz,start=c(2007,3))

print(train)
print(test)

autoplot(train)+ggtitle('training data')
autoplot(test)+ggtitle('testing data')


# (b) Model Fitting: ARIMA and ETS

fit_arima <- auto.arima(train)
summary(fit_arima)

fit_ets <- ets(train)
summary(fit_ets)

# (c) Model Validation and Forecasting

fcast_arima <- forecast(fit_arima,h=length(test))
fcast_ets <- forecast(fit_ets,h=length(test))

autoplot(fcast_arima)+autolayer(test,series = 'Test Data')
autoplot(fcast_ets)+autolayer(test,series = 'Test Data')

acc_arima <- accuracy(fcast_arima, test)
acc_ets   <- accuracy(fcast_ets, test)
acc_arima
acc_ets






###############  Q2. advert data — monthly sales and advertising

#  (a) Fit a standard regression model and predicted equation
library(fpp2)
library(tseries)
data('advert')

sales <- advert[, 'sales']
adv <- advert[,'advert']

fit_lm <- tslm(sales~adv)
summary(fit_lm)


#(b) Are there significant autocorrelations in residuals?

# exact residuals 
resid_lm <-residuals(fit_lm)

# check residual autocorrelation up to lag L
# # fitdf = number of regressors
Box.test(resid_lm,lag = 12,type = 'Ljung-Box',fitdf = 2)


# (c) Fit appropriate regression model with ARIMA errors
#     and write down the model equation

fit_manual <- Arima(sales,order = c(1,0,1),seasonal = c(0,1,1),xreg = adv)
summary(fit_manual)


# (d) Check the residuals of the fitted model and conclusion

resid2 <- residuals(fit_manual)
tsdisplay(resid2)
Box.test(resid2,lag = 12,type = 'Ljung-Box',fitdf = 3)


# (e) Forecast next six months when advertising = 10 units per month
newx <- matrix(rep(10,6))
fc <- forecast(fit_manual,xreg = newx,h=6)
plot(fc,main='6-step forecast with xreg =10')
print(fc)






# Q3 (a) Construct percentage growth rate series and plot
data <- read.table("C:/COLLEGE DATA/College profile/Time & Series Class notes/data.txt",
                   header = TRUE, sep = "")   # 'sep=""' handles space-separated data

head(data)
colnames(data)

# colnames(data) -> "year"  "mon"   "day"   "ipncongd"  "ipbuseq"  "ipmat" 

x1 <- ts(data$ipncongd, start = c(1963, 12), frequency = 12)  # nondurable consumer goods
x2 <- ts(data$ipmat,    start = c(1963, 12), frequency = 12)  # materials

# Construct percentage growth-rate series: 100 * diff(log(x))
z1 <- 100 * diff(log(x1))
z2 <- 100 * diff(log(x2))

z <- cbind(z1, z2)
colnames(z) <- c("ConsumerGoods", "Materials")


plot.ts(z, main = "Percentage Growth Rates of Two Components",)


# (b) Determine possible VARMA models using two-way table (α = 0.04)

install.packages("vars")


library(vars)
library(tseries)

#  Build simple two-way significance table 
N <- nrow(z)
crit <- 1.96 / sqrt(N)  # 4% level ≈ 1.96
cc <- ccf(z1, z2, plot = FALSE)
sig <- abs(cc$acf) > crit
two_way <- data.frame(Lag = cc$lag, Corr = cc$acf, Significant = sig)
head(two_way, 10)  # inspect first 10 lags

#  Automatic VAR order suggestion
VARselect(z, lag.max = 10, type = "const")
#Choose lag order p with lowest AIC/HQ/SC/FPE (often p=1)




# (c) Fit and simplify best VAR/VARMA model using |t| > 1.96

install.packages(c("tseries", "MTS", "zoo", "forecast"))

library(tseries)
library(MTS)
library(vars)
library(forecast)

# 1. Fit the suggested VAR(p) model (say p=1)
fit_var <- VAR(z, p = 3, type = "const")

# Check if residuals are NULL - this will likely be TRUE
is.null(fit_var$residuals)

# CORRECTED: Use residuals() function instead of $residuals
resid_var <- residuals(fit_var)
mq(resid_var, lag = 12)  # Portmanteau test for whiteness
# p-value > 0.05 → residuals are white (model adequate)

summary(fit_var)

# the fitted model is a VAR(3) model (Vector Autoregression with 3 lag) with a constant term.


# (d) Perform model checking of the fitted model. Is the model adequate? Why?

library(tseries)
library(MTS)

residuals_var <- residuals(fit_var)
ts.plot(residuals_var, main = "Residuals from VAR Model")
# Multivariate Q-test for autocorrelation
mq(residuals_var, lag = 12)
apply(residuals_var, 2, function(x) jarque.bera.test(x))



# (e) Do the 10-step ahead prediction using the best fitted model

# Step 1: Forecast 10 steps ahead
forecast_10 <- predict(fit_var, n.ahead = 10)

# Step 2: View forecast results for each variable
forecast_10$fcst   # contains forecasts + standard errors + 95% CIs

























