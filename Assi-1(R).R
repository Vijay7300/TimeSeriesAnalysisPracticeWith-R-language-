#Q-10.  Consider the datasets Sunspots and strikes from itsmr library.
# Plot the ACF and PACF for both datasets.

library(itsmr)

data("sunspots")
data('strikes')


ts_sn <- sunspots

par(mfrow=c(2,2))

acf(ts_sn,main='Sunspots-acf')
pacf(ts_sn,main='Sunspots-pacf')

acf(ts_str,main='stirkes-acf')
pacf(ts_str,main='strikes-pacf')



'''Q-11.  Consider the dataset austourists from fpp2 library. Plot the dataset. Estimate
its trend using smoothing with moving average filter with q = 3 and exponential
smoothing for alpha=0.4 and plot it.'''


library(fpp2)

# 2) Moving average q = 3 (centred)
ma_trend <- ma(austourists, order = 3, centre = TRUE)

# 3) Simple exponential smoothing (alpha = 0.4)
ses_fit <- ses(austourists, alpha = 0.4)

# 4) Overlay both smoothers on the data
autoplot(austourists, series = "Data") +
  autolayer(ma_trend,            series = "MA(3)") +
  autolayer(ses_fit$fitted,      series = "SES(0.4)") +
  ggtitle("Austourists: Moving Average (q=3) vs SES (alpha=0.4)") +
  ylab("Number of Tourists") + xlab("Year") +
  scale_colour_manual(values = c("Data" = "black",
                                 "MA(3)" = "red",
                                 "SES(0.4)" = "blue")) +
  guides(colour = guide_legend(title = NULL))





''' Q-12. Consider the dataset austourists from fpp2 library. Compute the
residuals after estimating the trend with quadratic polynomial and seasonality
of 12.Using 5% level of significance, conclude whether the residuals represent 
iid noise, and determine whether they follow a Gaussian distribution '''


# Load libraries
library(fpp2)      # for austourists dataset
library(tseries)   # for Jarque-Bera test


# 1. Data preparation

y <- austourists
t <- 1:length(y)                # time index
season <- factor(cycle(y))      # seasonal factor with period 12


# 2. Fit quadratic trend + seasonality (period = 12)

fit <- lm(y ~ t + I(t^2) + season)
summary(fit)

# Residuals
resid_fit <- residuals(fit)


# 3. Test for independence (Ljung-Box)

lb_test <- Box.test(resid_fit, lag=12, type="Ljung-Box")
print(lb_test)


# 4. Test for Gaussianity

# Jarque-Bera test
jb_test <- jarque.bera.test(resid_fit)
print(jb_test)

# Shapiro-Wilk test
sw_test <- shapiro.test(resid_fit)
print(sw_test)


# 5. Diagnostic plots

par(mfrow=c(2,2))

# Time plot of residuals
plot(resid_fit, type="l", main="Residuals", ylab="Residuals", xlab="Time")
acf(resid_fit, main="ACF of Residuals")
hist(resid_fit, breaks=18, main="Histogram of Residuals")

# Normal Q-Q plot
qqnorm(resid_fit); qqline(resid_fit, col="red")

par(mfrow=c(1,1))  # reset layout








