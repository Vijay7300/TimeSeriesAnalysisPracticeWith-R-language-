################. Mean of ARMA(1,1)###############
n<-300;p<-1;d<-0;q<-1
arma11<-arima.sim(list(order=c(p,d,q), ma=0.8, ar=.3), n) 
ts.plot(arma11)
mean(arma11)
lines(0.0*(1:n), col=2)##original mean
lines((-0.02/n)*(1:n), col=3)###Sample mean



#############################. Fitting AR(p) model using Yule-Walker Method #############
library(itsmr)
plotc(lake)
acf(lake)
pacf(lake)
yw(lake,2)


#################.  Hannan–Rissanen estimator. ##############
library(itsmr)
hannan(lake, p=1,q=1)


####################. Maximum likelihood method. 
library(itsmr)
library(forecast)
fit=Arima(USAccDeaths, order = c(1,0,1),method = "ML")
fit
plot(forecast(fit,h=30))




#########################.  Model selection based on AIC
library(forecast)

fit1=Arima(USAccDeaths, order = c(1,0,1),method = "ML")
fit1
fit2=Arima(USAccDeaths, order = c(1,0,0),method = "ML")
fit2
fit3=Arima(USAccDeaths, order = c(0,0,1),method = "ML")
fit3



#########################.  Model Diagonostic check
library(forecast)
fit=Arima(USAccDeaths, order = c(1,0,1),method = "ML")
plot(residuals(fit))
acf(residuals(fit),type = "correlation",plot = T)
pacf(residuals(fit))




##########. Alternative comment #########
library(itsmr)
test(residuals(fit))



############. For Gaussian noise #######
library(tseries)
jarque.bera.test(residuals(fit))