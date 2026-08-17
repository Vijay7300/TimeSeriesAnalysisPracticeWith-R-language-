'''  1. Simulate the dataset of 300 data points from Autoregressive Moving Average series
 of order (2,2) with ϕ1 = 0.1, ϕ2 = 0.2, θ1 = 0.3 and θ2 = 0.3. Plot the dataset.
 Also, plot ACF and PACF.'''

set.seed(123)
n<-300
p<-2;d<-0;q<-2
arma <- arima.sim(list(order=c(p,d,p),ar=c(0.1,0.2),ma=c(0.3,0.3)),n)
plot.ts(arma)
acf(arma,type='correlation',plot=T)
pacf(arma)



''' 2. Consider the dataset lake from itsmr package. Determine the suitable model—AR(p)
or MA(q)—and its order.'''

library(itsmr)
data(lake)
plot.ts(lake)
acf(lake,main='ACF of lake data')
pacf(lake,main='PACF of lake data')
arma(lake,p=1,q=0)
arma(lake,p=2,q=0)
arma(lake,p=0,q=1)
arma(lake,p=0,q=2)



''' 
3. Consider the dataset strikes from itsmr package. Using Maximum likelihood es-
timator method, answer the following:

(a) Based on AIC measure, out of ARMA(1,1), ARMA(2,1) and ARMA(1,2) which
is best fitted model.
(b) Based on AICc measure, out of ARMA(2,1), ARMA(2,2) and ARMA(1,2) which
is best fitted model.
(c) Based on BIC measure, out of ARMA(2,2), ARMA(3,1) and ARMA(1,3) which
is best fitted model.'''

library(itsmr)
data(strikes)
plot.ts(strikes)

# (a) based on AIC,AICc,BIC is lowest value is better fit model
m11 <- arima(strikes,order=c(1,0,1),method='ML')
m21 <- arima(strikes,order=c(2,0,1),method='ML')
m12 <- arima(strikes,order=c(1,0,2),method='ML')
AIC(m11) # m11 is better
AIC(m21)
AIC(m12)

# (b) based on AIC,AICc,BIC is lowest value is better
m21 <- arima(strikes,order=c(2,0,1),method='ML')
m22 <- arima(strikes,order=c(2,0,2),method='ML')
m12 <- arima(strikes,order=c(1,0,2),method='ML')
AIC(m21) 
AIC(m22) # m22 is better fit model
AIC(m12)

# (c) based on AIC,AICc,BIC is lowest value is better
m22 <- arima(strikes,order=c(2,0,2),method='ML')
m31 <- arima(strikes,order=c(3,0,1),method='ML')
m13 <- arima(strikes,order=c(1,0,3),method='ML')
AIC(m22) # m22 is better fit model
AIC(m31)
AIC(m13)



''' 4. For the best fitted model in each part of question (3), 
using the 5% level of significance, answer the following:

(a) Determine whether the residuals are iid noise or not.
(b) Determine whether the residuals are iid Gaussian noise or not.

If p > 0.05 → residuals are iid (no autocorrelation).
If p ≤ 0.05 → residuals are not iid.'''

# (a) Determine whether the residuals are iid noise or not.

Box.test(residuals(m11),type = 'Ljung-Box') # for a-part in q3
Box.test(residuals(m22),type = 'Ljung-Box') # for b-part
Box.test(residuals(m22),type = 'Ljung-Box') # for c-part

#(b) Determine whether the residuals are iid Gaussian noise or not.

tseries::jarque.bera.test(residuals(m11))
tseries::jarque.bera.test(residuals(m22))



''' 5. Consider the time series having 100 observations: X1, X2, X3, .., X100 gave the
following sample ACF: ˆρ(1) = 0.43. Assume that the data is generated from MA(1)
process. Determine the approximate 99% confidence interval for ρ(h) for h ≥ 1.'''


# given values
n <- 100
rho <- 0.43

# 99% confidence interval(CI) uses z=2.576
z <- qnorm(0.995)
std_error <- 1/sqrt(n)

# ci for lag 1 autocorrelation
ci_rho <- c(rho - z*std_error , rho + z*std_error)

# ci for lag >=2 (  true for MA(1))

ci_rho1 <- c(- z*std_error , z*std_error)

ci_rho
ci_rho1
z



''' 7. Consider the dataset oil from fpp2 package. Using ADF, PP and KPSS test, for
α = 0.05, determine whether the series is stationary or not.
If ADF p-value < 0.05 → stationary.
If PP p-value < 0.05 → stationary.
If KPSS p-value < 0.05 → not stationary.  '''


library(fpp2)
library(tseries)

data(oil)
plot(oil,main='oil production data')
adf.test(oil) # not st.
pp.test(oil) # not st.
kpss.test(oil) # not st.




''' 10. Simulate the dataset of 200 data points from SARIMA(1, 0, 1)(1, 0, 1)12 with Φ1 =
0.2, φ1 = 0.3, Θ1 = 0.4 and θ1 = 0.5. Write down the model equation and plot the
dataset. Also, plot ACF and PACF.'''

library(forecast)
set.seed(123)
n<-200
# parameters
phi<- 0.2 ;theeta <- 0.4 
phi1 <- 0.3;theeta1 <- 0.5 # for seasonal

  sarima_model <- arima.sim(model=list(order=c(1,0,1),seasonal = list(order=c(1,0,1),
      period=12,ar=phi1,ma=theeta1),ar=phi,ma=theeta),n)
plot.ts(sarima_model, main="Simulated SARIMA(1,0,1)(1,0,1)[12] Series")

acf(sarima_model)
pacf(sarima_model)



''' 12. Consider the dataset elec from fma package. Using Box-cox transformation, deter-
mine the value of lambda. Based on AICC and BIC, out of ARIMA(2,1,1)(1,1,1),
ARIMA(2,1,2)(2,1,2) and ARIMA(0,1,1)(1,2,1) which is best fitted model. 
If λ≈1 → no transformation needed.
If λ=0 → log-transform is recommended.'''

library(fma)
data("elec")
autoplot(elec) # plot to see trend/seasonality

lambda1 <- BoxCox.lambda(elec)
lambda1

fit1 <- Arima(elec,order = c(2,1,1),seasonal = c(1,1,1),lambda=lambda1)
fit2 <- Arima(elec,order = c(2,1,2),seasonal = c(2,1,2),lambda=lambda1)
fit3 <- Arima(elec,order = c(0,1,1),seasonal = c(1,2,1),lambda=lambda1)

# Compare
cbind(
  Model = c("ARIMA(2,1,1)(1,1,1)",
            "ARIMA(2,1,2)(2,1,2)",
            "ARIMA(0,1,1)(1,2,1)"),
  AICc = c(fit1$aicc, fit2$aicc, fit3$aicc),
  BIC  = c(fit1$bic, fit2$bic, fit3$bic)
)
# so model fit1 is better



''' 14. Consider the dataset speech from astsa package. Using ADF, PP and KPSS test,
for α = 0.05, determine whether the series is stationary or not.'''
install.packages('astsa')

library(astsa)
data(speech)
plot.ts(speech)

library(tseries)
adf <- adf.test(speech)
adf
pp <- pp.test(speech)
pp
kpss <- kpss.test(speech)
kpss



''' 15. Consider the dataset woolyrnq from fma package. Fit an appropriate SARIMA
model.'''

install.packages('fma')

library(fma)
data('woolyrnq')
autoplot(woolyrnq)

fit <- sarima(woolyrnq, p=0, d=1, q=1, P=0, D=1, Q=1, S=4)







































