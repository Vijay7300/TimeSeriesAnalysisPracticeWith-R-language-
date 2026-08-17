########. Autralian Beer Data ###############
library(fpp2)
ab=window(ausbeer, start=c(1995,1))
autoplot(ab)
#####################. Pigs data ##########
pg=window(pigs, start=c(1992,1))
autoplot(pg)
################# Applying Benchmark Methods ################

library(fpp2)
autoplot(ausbeer) ## Plot of the data
ausbeer
ggseasonplot(ausbeer)  ## Seasonal Plot of the data
train <- window(ausbeer,start=c(1975,1), end=c(2007,3))
train ## Training set
test<-window (ausbeer,start=c(2007,4))
test ## Test Set
m1=meanf(train, h=length(test)) ##Forecasting using average method
m2=rwf(train, h=length(test)) ##Forecasting using Naive method
m3=snaive(train, h=length(test)) ##Forecasting using seasonal naive method
m4=rwf(train, h=length(test), drift=T) ##Forecasting using drift method
###### Plot of forecasts ################### 
window(ausbeer, start=c(1975,1)) %>% autoplot() +
  autolayer(m1,series="Mean", PI=F) +
  autolayer(m2,series="Naïve", PI=F) +
  autolayer(m3,series="Seasonal naïve", PI=F)+
  autolayer(m4,series="Drift", PI=F) 
############################## Plot with prediction interval ######
window(ausbeer, start=c(1975,1)) %>% autoplot()+
  autolayer(m3,series="Seasonal naïve", PI=T)
############### Accuracy Measure ##################
accuracy(m1, test) ## Accuracy of Model 1
accuracy(m2,test) ## Accuracy of Model 2
accuracy(m3,test) ## Accuracy of Model 3
accuracy(m4,test) ## Accuracy of Model 4





########################################################################
######### Another Data set  #######################################
#######################################################################
library(fpp2)
autoplot(goog200) ##Plot of the data
goog200 ## The data
Train=window(goog200, end=177)
Train ## Training Set 
Test=window(goog200, start=178)
Test ## Test Set
M1=meanf(Train, h=23) ## Forecasting using average method
M2=rwf(Train, h=23)  ## Forecasting using Naive method
M3=rwf(Train, drift=TRUE, h=23) ## Forecasting using Drift Method
###### Plot of forecasts ################### 
autoplot(goog200) +
  autolayer(M1, series="Mean", PI=FALSE) +
  autolayer(M2, series="Naïve", PI=FALSE) +
  autolayer(M3,series="Drift", PI=FALSE) 
####### Accuracy of different Methods. ###########
accuracy(M1, Test)  ## Accuracy of Model 1
accuracy(M2,Test)   ## Accuracy of Model 2
accuracy(M3,Test)   ## Accuracy of Model 3




########################################################################
#########. Simple Exponential Smoothing ################################
########################################################################
library(fpp2)
autoplot(oil)
oildata <- window(oil, start=1992)
fc <- ses(oildata, h=5)# Estimate parameters
summary(fc)
accuracy(fc)# Accuracy of one-step-ahead training errors
autoplot(fc)+autolayer(fitted(fc), series="Fitted") 




########################################################################
#########. Holt's Linear Trennd Method ################################
########################################################################
air <- window(ausair, start=1990)
f1 <- holt(air, h=5)
summary(f1)
accuracy(f1)
autoplot(air) + autolayer(f1, series="Holt's method") 





##############. Damped Trend Method #################
f1 <- holt(air, h=15)
f2 <- holt(air, damped=TRUE, phi = 0.9, h=15)
autoplot(air) +
  autolayer(f1, series="Holt's method", PI=FALSE) +
  autolayer(f2, series="Damped Holt's method", PI=FALSE)





######. Comparisons between SES, Hot and Damped Methods ############
library(fpp2)
autoplot(livestock)
Train <- window(livestock, start=1970, end=2000)
Test<-window(livestock, start=2001)
autoplot(Train)
fit1 <- ses(Train)
summary(fit1)
fit2 <- holt(Train)
summary(fit2)
fit3 <- holt(Train, damped = TRUE)
summary(fit3)
accuracy(fit1, Test) 
accuracy(fit2,Test) 
accuracy(fit3, Test)
autoplot(livestock) +
  autolayer(fit1, series="SES method", PI=FALSE)+
  autolayer(fit2, series="Holt's method", PI=FALSE) +
  autolayer(fit3, series="Damped Holt's method", PI=FALSE)





###############. Holt-Winters’  seasonal Method ########################
library(fpp2)
autoplot(austourists)
aust <- window(austourists,start=2005) 
autoplot(aust)
f1 <- hw(aust,seasonal="additive")
f2 <- hw(aust,seasonal="multiplicative")
autoplot(aust) +
  autolayer(f1, series="HW additive forecasts", PI=FALSE) +
  autolayer(f2, series="HW multiplicative forecasts",PI=FALSE) 






###############. Holt-Winters’  damped Method ########################
library(fpp2)
autoplot(hyndsight)
hyn=subset(hyndsight,end=length(hyndsight)-35)
fc <- hw(hyn,damped = TRUE, seasonal="multiplicative", h=35)
autoplot(hyndsight) +
  autolayer(fc, series="HW multi damped", PI=FALSE)





######################################################################
##################.   ETS Models. ####################################
######################################################################
library(fpp2)
aust <- window(austourists, start=2005)
autoplot(aust)
fit <- ets(aust)
summary(fit)
autoplot(fit)
cbind('Residuals' = residuals(fit),
      'Forecast errors' = residuals(fit,type='response')) %>%
  autoplot(facet=TRUE) + xlab("Year") + ylab("")
####. Forecasting
autoplot(aust)+autolayer(forecast(fit, h=8)) 
########################. Another Example ###############
h02 %>% ets() %>% forecast() %>% autoplot()
#############
train <- window(h02, end=c(2004,12)) 
test <- window(h02, start=2005)
fit1 <- ets(train)
summary(fit1)
fit2 <- ets(test, model = fit1)##Refitting on new data
summary(fit2)
accuracy(fit2)
accuracy(forecast(fit1,10), test)





###########. ARIMA VS ETS #################
library(fpp2)
air
autoplot(air)
#####. Time Series Cross Validation ##########
f_ets <- function(x, h) {
  forecast(ets(x), h = h)}
f_arima <- function(x, h) {
  forecast(auto.arima(x), h=h)}
# Compute CV errors for ETS as e1
e1 <- tsCV(air, f_ets, h=1)
# Compute CV errors for ARIMA as e2
e2 <- tsCV(air, f_arima, h=1)
# Find MSE of each model class
mean(e1^2, na.rm=TRUE)
#> [1] 7.864
mean(e2^2, na.rm=TRUE)
##### Forecasting using ETS #########
air %>% ets() %>% forecast() %>% autoplot()





#####################################################################
#########. Comparing auto.arima() and ets() on seasonal data #########
######################################################################
# Consider the qcement data beginning in 1988
library(fpp2)
qcement
autoplot(qcement)
ggseasonplot(qcement)
# Use 20 years of the data as the training set
train <- window(qcement, start=c(1988,1), end=c(2007,4))
test<-window(qcement, start=c(2008,1))
fit.arima <- auto.arima(train)
fit.arima
checkresiduals(fit.arima)
fit.ets <- ets(train)
fit.ets
checkresiduals(fit.ets)
# Generate forecasts and compare accuracy over the test set
a1 <- fit.arima %>% forecast(h = length(test)) %>% accuracy(test)
a1[,c("RMSE","MAE","MAPE","MASE")]
a2 <- fit.ets %>% forecast(h = length(test)) %>% accuracy(test)
a2[,c("RMSE","MAE","MAPE","MASE")]
########. Forecastin ####################
qcement %>% ets() %>% forecast(h=12) %>% autoplot()





############################################################################
#############################. Dynamic Regression ##########################
#############################################################################
library(fpp2)
uschange
autoplot(uschange[,1:2], facets=TRUE)
LM1=Arima(uschange[,"Consumption"], xreg=uschange[,"Income"], order=c(0,0,0))
LM1 ##Fitting of linear model with uncorrelated errors
checkresiduals(LM1)
Acf(LM1$residuals)
Pacf(LM1$residuals)
###### Fitting Manually ###############
M2<-Arima(uschange[,"Consumption"], xreg=uschange[,"Income"], order=c(3,0,3))
M2
checkresiduals(M2)
################# Fitting automatically #################
fit <- auto.arima(uschange[,"Consumption"], xreg=uschange[,"Income"], stepwise=F)
fit
checkresiduals(fit)
fcast <- forecast(fit, xreg=rep(mean(uschange[,2]),8))
autoplot(fcast)





######################################################
##### Considering all predictors #########
autoplot(uschange[,1:5], facets=TRUE)
Ind <- cbind(Income=uschange[,"Income"],
              Production = uschange[,"Production"],
              Savings=uschange[,"Savings"],
              UE=uschange[,"Unemployment"])
##########
LM2=Arima(uschange[,"Consumption"], xreg=Ind, order=c(0,0,0))
LM2 ##Fitting of linear model with uncorrelated errors
checkresiduals(LM2)
ggseasonplot(LM2$residuals)
Acf(LM2$residuals)
Pacf(LM2$residuals)
##############
M1<-auto.arima(uschange[,"Consumption"], xreg=Ind)
M1
checkresiduals(M1)
fc <- forecast(M1, xreg = cbind(Income=c(1.2,2.3,1.5,-1.3), 
                                Production=c(-2,3.1,1.5,-2.4),
                               Savings=c(4.8,7.9,6.7,4.7), 
                               UE=c(-0.1,-0.2,0.5,0.4)))
autoplot(fc)





###########################################################
######. Another Example ####################
elecdaily
xreg <- cbind(MaxTemp = elecdaily[, "Temperature"],
              MaxTempSq = elecdaily[, "Temperature"]^2,
              Workday = elecdaily[, "WorkDay"])
ft <- auto.arima(elecdaily[, "Demand"], xreg = xreg)
ft
checkresiduals(ft)
fcast <- forecast(ft,
                  xreg = cbind(MaxTemp=rep(26,14), MaxTempSq=rep(26^2,14),
                               Workday=c(0,1,0,0,1,1,1,1,1,0,0,1,1,1)))
autoplot(fcast)









