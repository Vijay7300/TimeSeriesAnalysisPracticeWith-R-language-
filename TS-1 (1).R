
library(fpp2)


#################### White Noise ##########
set.seed(30)       # Fixes randomness so results are reproducible
y <- ts(rnorm(50)) # Creates a time series of 50 random normal values
autoplot(y) 

WN <- arima.sim(model = list(order = c(0, 0, 0)), n = 200)
plot.ts(WN,col=4, main="White Noise Series")



####################. Random walk. ########
set.seed(123)
TT <- 100
## initialize {x_t} and {w_t}
xx <- ww <- rnorm(n = TT, mean = 0, sd = 1)  # Random noise (mean=0, sd=1)
## compute values through TT
for (t in 2:TT) {
  xx[t] <- xx[t - 1] + ww[t]
}
par(mfrow = c(1, 2))  # parameter c(no.of rows of plots, no.of columns of plots)
plot.ts(xx, ylab = expression(italic(x[t])))



####################. TRend ##############
set.seed(123)
n <- 200
y <- rnorm(n,0,1)                 # Random noise (mean=0, sd=1)
xy<- y   # xy = pure noise.
xt<- 0.1*(1:n)+y                # Adds a linear trend (0.1*t) to noise
plot.ts(xy, ylim=c(-5,35), ylab="Trend")
lines(xt)                        # xt = noise + upward linear trend.
lines(0.1*(1:n), col=2, lty="dashed")



######################## Seasonality. $$$$$
set.seed(21)
n<-100;
yt <- rnorm(n,0,1)
xtr<-0.2*(1:n)
xt<-0.2*(1:n) + 2*sin(2.5*(1:n)*pi)+yt         # xt = linear trend + seasonality + noise
plot.ts(xt, ylim=c(-5,33), ylab="Seasonality")
lines(xtr, col=2)                             # Adds the trend line (red).
lines(2*sin(2.5*(1:n)*pi), col=3)          # Adds the seasonal component (green).



############Additive decomposition of anibiaotic data. #####
library(fpp2)
autoplot(a10)    # Data=Trend + Seasonal + Random
autoplot(a10, series="Data") + autolayer(ma(a10,5), series="5-MA") 
plot(decompose(a10,type = "additive"))



###################### Multiplicative Model #########
library(fpp2)    # Data=Trend × Seasonal × Random
autoplot(euretail) # euretail is a quarterly time series of retail trade volumes in the Euro area.
plot(decompose(euretail,type = "multiplicative"))



############# Estimating trend: smoothing average ########
library(fpp2)
ma(elecsales,5)   # elecsales = monthly electricity sales in South Australia
autoplot(elecsales, series="Data") + autolayer(ma(elecsales,5), series="5-MA") 
# original electricity sales data + 5-month moving average



#############. Estimating Trend: Exponential smoothing ###########
library(fpp2)
autoplot(elecsales, series="Data") + autolayer(ses(elecsales,alpha=0.2), series="SES") 
 ses(elecsales, alpha = .2)
# Simple Exponential Smoothing (SES) with smoothing parameter 𝛼=0.2
# SES is like a weighted moving average where recent values get more weight.
# Lower α (close to 0) → more smoothing, reacts slowly to changes.
# Higher α (close to 1) → less smoothing, reacts quickly.
 
 
 
###############. Testing of Estimated Noise #################
 library(itsmr)
 plotc(wine)  # dataset: Australian wine sales (monthly).
 M = c("log","season",12,"trend",1)
 e = Resid(wine,M) # You first model the wine series (log + remove seasonality + detrend).
 library(feasts)
 box_pierce(e)
 
""" 
Extract residuals e.

Then test residuals with Box–Pierce test.

If p-value > 0.05 → residuals look like white noise (your model is a good fit) or you fail to reject H₀.

If p-value < 0.05 → residuals still have autocorrelation or you reject H₀
(model needs improvement or  model hasn’t captured all patterns).
 
"""
 
 ########
 library(itsmr)
 M = c("log","season",12,"trend",1)
 e = Resid(wine,M)
 test(e)
 
 
 
##################
 library(itsmr)
 M = c("log","season",12,"trend",1)
 e = Resid(wine,M)
 library(tseries)
 jarque.bera.test(e)
 
 
 '''  NOte:- 
 box_pierce(e)      → tested independence (white noise).
jarque.bera.test(e) → tests normality of residuals.

High p-value (> 0.05): Fail to reject H₀ → residuals look approximately normal.
Low p-value (≤ 0.05): Reject H₀ → residuals are not normal (skewed or heavy tails).

Null hypothesis (H₀): Residuals follow a normal distribution.
Alternative (H₁): Residuals are not normally distributed. '''
 
 
 
 
 
 
 
 