
############################### TS-2 ###################


#. Statinary and non-stationary time series. 
set.seed(223)
n<-100
T1 <- rnorm(n,0,1)###IID Noise
T2<-cumsum(T1) ##Random walk
z<-T1+0.5 
T3<-cumsum(z)##Sum of iid general normal variables with mean 0.5
plot.ts(T1, ylim=c(-15,45), ylab="Stationary and Non Stationary") 
lines(T2, col=5)##Plotting T2
lines(T3,lty=6, col=4)##Plotting T2
lines(0.5*(1:n), col=3, lty="dashed")##Plotting mean of T3
lines(0.0*(1:n), col=2,lwd=2)## Plotting mean of noise


# ACF plot
set.seed(223)
n<-100
T1 <- rnorm(n,0,1)###IID Noise
T2<-cumsum(T1) ##Random walk
z<-T1+0.5 
T3<-cumsum(z)##Sum of iid general normal variables with mean 0.5

par(mfrow=c(1,3))
acf(T1,type = 'correlation',plot = T)
acf(T2,type = 'correlation',plot = T)
acf(T3,type = 'correlation',plot = T)


# ACF plot for red wine slaes data
library(itsmr)
plotc(wine)
acf(wine,type = 'correlation',plot = T)


# Moving average process
set.seed(124)
n<-500
##Simulated data from the model: X_t=Z_t+0.7Z_{t-1} i.e. MA(1)
ma1<-arima.sim(list(order=c(0,0,1),ma='0.7'),n)
plot.ts(ma1)

##Simulated data from the model: X_t=Z_t-0.3Z_{t-1}+0.8Z_{t-2} i.e. MA(2)
ma2<-arima.sim(list(order=c(0,0,2),ma=c(-0.3,0.8)),n)
plot.ts(ma2)


# ACF plot for moving average series

set.seed(124)
n<-500
par(mfcol=c(2,1))
##Simulated data from the model: X_t=Z_t+0.7Z_{t-1} i.e. MA(1)
ma1<-arima.sim(list(order=c(0,0,1),ma='0.7'),n)
acf(ma1,type = 'correlation',plot = T)

##Simulated data from the model: X_t=Z_t-0.3Z_{t-1}+0.8Z_{t-2} i.e. MA(2)
ma2<-arima.sim(list(order=c(0,0,2),ma=c(-0.3,0.8)),n)
acf(ma2,type = 'correlation',plot=T)


# ACF plot for autoregressive process
set.seed(523)
n <- 500
AR1<- arima.sim(list(order(1,0,0), AR=0.6),n)
AR2<- arima.sim(list(order(2,0,0),AR=c(0.6,08)),n)
plot.ts(AR1)
plot.ts(AR2)

#. ACF plot for autoregressive series
set.seed(523)
n <- 500
AR1<- arima.sim(list(order(1,0,0),AR=0.6),n)
AR2<- arima.sim(list(order(2,0,0),AR=c(0.6,08)),n)
acf(AR1,type='correlation',plot=T)
acf(AR2,type='correlation',plot=T)



# pacf for MA series
set.seed(124)
n<-500
par(mfcol=c(2,1))
##Simulated data from the model: X_t=Z_t+0.7Z_{t-1} i.e. MA(1)
ma1<-arima.sim(list(order=c(0,0,1),ma='0.7'),n)
pacf(ma1,type = 'correlation',plot = T)

##Simulated data from the model: X_t=Z_t-0.3Z_{t-1}+0.8Z_{t-2} i.e. MA(2)
ma2<-arima.sim(list(order=c(0,0,2),ma=c(-0.3,0.8)),n)
pacf(ma2,type = 'correlation',plot=T)

# pacf for AR series

set.seed(523)
n <- 500
AR1<- arima.sim(list(order(1,0,0),AR=0.6),n)
AR2<- arima.sim(list(order(2,0,0),AR=c(0.6,08)),n)
pacf(AR1,type='correlation',plot=T)
pacf(AR2,type='correlation',plot=T)


''' ma1 (top):

Strong spike at lag 1.
PACF beyond lag 1 ≈ 0 (no significant correlation , looks like white noise).
Looks like an MA(1).

ma2 (bottom):
Significant spikes at lag 1 and lag 2.
PACF after lag 2 ≈ 0.(looks like white noise)
Looks like an MA(2).

Theory check:
ACF of MA(q) cuts off after lag q.
PACF of MA(q) tails off gradually.  '''



#. AR and MA(\infnity) model
set.seed(13)
n<-100
d<-0
arsim<-numeric(0)
masim<-numeric(0)
for (i in 1:5000){
  ar1<-arima.sim(list(order=c(1,0,0),ar=0.7),n)
  mainf<-arima.sim(list(order=c(0,0,500),ma=(0.7)^(seq(1:500))),n)
  arsim[i]<-ar1[n]
  masim[i]<-mainf[n]
}
plot(density(arsim),main = 'density',n)
lines(density(masim),col='red')



#. ARMA(1,1) model 
set.seed(123)
par(mfrow=c(1,3))
n<-100;p<-1;d<-0;q<-1
arma11<-arima.sim(list(order=c(p,d,q),ma=0.4,ar=0.5),n)
plot.ts(arma11)
acf(arma11,type = 'correlation',plot = T)
pacf(arma11,type='correlation',plot = T)


#. ARMA(2,1) model 
set.seed(123)
par(mfrow=c(1,3))
n<-100;p<-2;d<-0;q<-1
arma21<-arima.sim(list(order=c(p,d,q), ma=0.4, ar=c(0.6,0.3)),n)
plot.ts(arma21)
acf(arma21,type = 'correlation',plot = T)
pacf(arma21,type='correlation',plot = T)



#. ARMA(2,2) model 
set.seed(123)
par(mfrow=c(1,3))
n<-100;p<-2;d<-0;q<-2
arma22<-arima.sim(list(order=c(p,d,q),ma=c(0.2,0.6),ar=c(0.6,0.2)),n)
plot.ts(arma22)
acf(arma22,type = 'correlation',plot = T)
pacf(arma22,type='correlation',plot = T)


# white noise

#. ARMA(0,0) model 
set.seed(123)
par(mfrow=c(1,3))
n<-100;p<-0;d<-0;q<-0
arma22<-arima.sim(list(order=c(p,d,q)),n)
plot.ts(arma22)
acf(arma22,type = 'correlation',plot = T)
pacf(arma22,type='correlation',plot = T)



############################### TS-3 ###################
#. Mean of ARMA(1,1)

set.seed(123)
n<-300
par(mfrow=c(1,2))
arma11<-arima.sim(list(order=c(1,0,1),ma=0.8,ar=0.4),n)
plot.ts(arma11)
mean(arma11)
lines(0.0*(1:n),col=2)       #original mean
lines((-0.02/n)*(1:n),col=3)  # sample mean


#. Fitting AR(p) model using Yule-Walker Method 
library(itsmr)
plotc(lake)
acf(lake)
pacf(lake)
yw(lake,2)


#.  Hannan–Rissanen estimator.
library(itsmr)
hannan(lake,1,1)


#. Maximum likelihood method. 
library(itsmr)
library(forecast)
fit<-Arima(USAccDeaths,order=c(1,0,1),method = 'ML')
fit
plot(forecast(fit,h=30))


#.  Model selection based on AIC
library(forecast)

fit1=Arima(USAccDeaths, order = c(1,0,1),method = "ML")
fit1
fit2=Arima(USAccDeaths, order = c(1,0,0),method = "ML")
fit2
fit3=Arima(USAccDeaths, order = c(0,0,1),method = "ML")
fit3



#.  Model Diagonostic  check
library(forecast)
par(mfrow=c(1,3))
fit<-Arima(USAccDeaths,order=c(1,0,1),method = 'ML')
plot(residuals(fit))
acf(residuals(fit),type='correlation',plot = T)
pacf(residuals(fit))


#. Alternative comment
library(itsmr)
test(residuals(fit))

#. For Gaussian noise 
library(tseries)
jarque.bera.test(residuals(fit))
























