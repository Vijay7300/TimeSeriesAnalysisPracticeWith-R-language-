################. Statinary and non-stationary time series. ############
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



################## ACF plot################
set.seed(1231) 
n<-100
T1 <- rnorm(n,0,1)###IID Noise
T2<-cumsum(T1) ##Random walk
z<-T1+0.5 
T3<-cumsum(z)##Sum of iid general normal variables with mean 0.5
################# ACF plot of T1 #######
acf(T1,type = "correlation",plot = T)
############### ACF plot of T1 ##########
acf(T2,type = "correlation",plot = T)
###############. ACF plot of T1. ########
acf(T3,type = "correlation",plot = T)



###############################. ACF plot of red wine sales data ###########
library(itsmr) # Load ITSM-R
plotc(wine)
acf(wine,type = "correlation",plot = T)



###################. Moving average ################
set.seed(124) 
n<-500
##Simulated data from the model: X_t=Z_t+0.7Z_{t-1} i.e. MA(1)
ma1<-arima.sim(list(order=c(0,0,1), ma=0.7), n) 
ts.plot(ma1) #arima->order = c(p, d, q)

##Simulated data from the model: X_t=Z_t-0.3Z_{t-1}+0.8Z_{t-2} i.e. MA(2)
ma2<-arima.sim(list(order=c(0,0,2), ma=c(-0.3,0.8)), n)
ts.plot(ma2) # #arima-> ma = c(theta1,theta2)



##################. ACF plot for moving average series ##########
par(mfcol = c(2,1))
ma1<-arima.sim(list(order=c(0,0,1), ma=0.7), n=500) 
acf(ma1,type = "correlation",plot = T)
ma2<-arima.sim(list(order=c(0,0,2), ma=c(-0.3,0.8)), n=500)
acf(ma2,type = "correlation",plot = T)



##################. ACF plot for autoregressive process. ################
set.seed(523)
AR1<-arima.sim(list(order=c(1,0,0), ar=0.9), n=500) 
ts.plot(AR1)
AR2<-arima.sim(list(order=c(2,0,0), ar=c(-0.2,0.6)), n=500) 
ts.plot(AR2)


##################. ACF plot for autoregressive series ##########
AR1<-arima.sim(list(order=c(1,0,0), ar=0.9), n=500) 
acf(AR1,type = "correlation",plot = T)
AR2<-arima.sim(list(order=c(2,0,0), ar=c(-0.2,0.6)), n=500)
acf(AR2,type = "correlation",plot = T)




##################. PACF plot for moving average series ##########
ma1<-arima.sim(list(order=c(0,0,1), ma=0.7), n=500) 
pacf(ma1)
ma2<-arima.sim(list(order=c(0,0,2), ma=c(-0.3,0.8)), n=500)
pacf(ma2)


##################. PACF plot for autoregressive series ##########
AR1<-arima.sim(list(order=c(1,0,0), ar=0.9), n=500) 
pacf(AR1)
AR2<-arima.sim(list(order=c(2,0,0), ar=c(-0.2,0.6)), n=500)
pacf(AR2)




######################. AR and MA(\infnity) model ##########

set.seed(13) 
t <- 100
d <- 0
arsim <- numeric(0)   # pre-allocate
masim <- numeric(0)

for (i in 1:5000) {
  # AR(1)
  ar1 <- arima.sim(list(order=c(1,0,0), ar=0.7), t) 
  
  # MA(500) with decaying coefficients
  mainf <- arima.sim(list(order=c(0,0,500), ma=(0.7)^(seq(1:500))), t) 
  
  # store the last observation
  arsim[i] <- ar1[t] 
  masim[i] <- mainf[t]
}
plot(density(arsim), main="density")
lines(density(masim),col="red")
#plot(ecdf(arsim), main="ecdf")
#lines(ecdf(masim),col="red")



#################################. ARMA(1,1) model ########################
set.seed(23)
n<-300;p<-1;d<-0;q<-1
arma11<-arima.sim(list(order=c(p,d,q), ma=0.8, ar=.3), n) 
ts.plot(arma11)
acf(arma11,type = "correlation",plot = T)
pacf(arma11)



#####################ARMA(2,1) #################
n<-200;p<-2;d<-0;q<-1
arma21<-arima.sim(list(order=c(p,d,q), ma=0.5, ar=c(0.6,0.2)), n) 
ts.plot(arma21)
acf(arma11,type = "correlation",plot = T)
pacf(arma11)



######################. ARMA(2,2) #######################
n<-300;p<-2;d<-0;q<-2
arma22<-arima.sim(list(order=c(p,d,q), ma=c(0.5,0.7), ar=c(0.6,0.2)), n) 
ts.plot(arma22)
acf(arma22,type = "correlation",plot = T)
pacf(arma22)






