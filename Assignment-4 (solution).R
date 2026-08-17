'''1. Consider the dataset m-msft8608.txt which contains the monthly simple returns
of Microsoft stock. Plot the log return series and check whether there is an
ARCH effect or not.'''


# Load data
msft <- read.table("m-msft8608.txt", header = TRUE)
head(msft)

# Extract simple returns and compute log returns
r <- msft$rtn
logret <- log(1 + r)

# Plot log return series
plot(logret, type = "l",
     main = "Microsoft Log Return Series",
     ylab = "Log Return", xlab = "Time")

# ARCH-LM Test
install.packages("FinTS")

library(FinTS)
ArchTest(logret, lags = 12)




''' 2. Consider the dataset m-c8608.txt which contains the monthly simple returns of
Citi-group stock. Based on AIC, which is the best fitted model among ARCH(1),
ARCH(2) and ARCH(3) with Gaussian innovation? Write down the model equation
for best fitted model.'''


library(tseries)

# Load data
citi <- read.table("m-c8608.txt", header = TRUE)
head(citi)
r <- citi$rtn                   # simple returns
logret <- log(1 + r)

# Fit ARCH models with Gaussian innovations
m1 <- garch(logret, order = c(0, 1))   # ARCH(1)
m2 <- garch(logret, order = c(0, 2))   # ARCH(2)
m3 <- garch(logret, order = c(0, 3))   # ARCH(3)

# Compare AIC values
AIC(m1, m2, m3)



''' 3. Consider the dataset m-gm3dx7508.txt which contains the monthly simple re-
turns of IBM stock, VW, EW, and S&P. Out of these simple returns consider
only monthly simple returns of IBM stock given in column gm. Based on BIC,
which is the best fitted model among ARCH(2), ARCH(3) and ARCH(4) with
student-t innovation? Write down the model equation for best fitted model.'''

install.packages("rugarch")

library(rugarch)

# Load data
gm <- read.table("m-gm3dx7508.txt", header = TRUE)
r <- gm$gm                      # IBM simple returns



# --- ARCH(2) ---
spec2 <- ugarchspec(
  variance.model = list(model="sGARCH", garchOrder=c(2,0)),
  mean.model     = list(armaOrder=c(0,0)),
  distribution.model = "std"
)
fit2 <- ugarchfit(spec2, r)
fit2
# --- ARCH(3) ---
spec3 <- ugarchspec(
  variance.model = list(model="sGARCH", garchOrder=c(3,0)),
  mean.model     = list(armaOrder=c(0,0)),
  distribution.model = "std"
)
fit3 <- ugarchfit(spec3, r)

# --- ARCH(4) ---
spec4 <- ugarchspec(
  variance.model = list(model="sGARCH", garchOrder=c(4,0)),
  mean.model     = list(armaOrder=c(0,0)),
  distribution.model = "std"
)
fit4 <- ugarchfit(spec4, r)

# --- Compare BIC (Correct Method) ---
bic2 <- infocriteria(fit2)[2]
bic3 <- infocriteria(fit3)[2]
bic4 <- infocriteria(fit4)[2]

c(ARCH2 = bic2, ARCH3 = bic3, ARCH4 = bic4)




''' 4. Consider the dataset m-gmsp5008.txt which contains the monthly simple returns
of GM stock & SP500. Out of these simple returns consider only monthly simple
returns of SP500 given in column sp. Fit ARCH(1) model with skew student-t
innovation and check whether square of standardized residuals are iid noise or not.'''

library(rugarch)
library(FinTS)     # for ArchTest

# Read data
data <- read.table("m-gmsp5008.txt", header = TRUE)
sp <- data$sp

# ARCH(1) with skewed t
spec <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1,0)),
  mean.model = list(armaOrder = c(0,0), include.mean = TRUE),
  distribution.model = "sstd"
)

fit <- ugarchfit(spec, sp)


# ARCH-LM test on residuals
ArchTest(residuals(fit), lags = 12)




''' 5. Consider the dataset m-3m4608.txt which contains the monthly simple returns of
3M stock. Fit GARCH(1,1) model with student-t innovation and check whether
square of standardized residuals are iid noise or not.'''

library(rugarch)
library(FinTS)


data <- read.table('m-3m4608.txt',header = TRUE)

r<-data$rtn

spec11 <- ugarchspec(variance.model=list(model='sGARCH',garchOrder=c(1,1)),
                      mean.model=list(armaOrder=c(0,0)),
                      distribution.model='std')
fit <- ugarchfit(spec11,r)

ArchTest(residuals(fit),lags = 12)



''' 6. Consider the dataset m-intc7308.txt which contains the monthly simple returns
of Intel stock. Fit an appropriate GARCH model.'''

library(rugarch)
library(FinTS)


data <- read.table('m-intc7308.txt',header = TRUE)

r<-data$rtn

# ARCH(1,1)
spec1 <- ugarchspec(variance.model=list(model='sGARCH',garchOrder=c(1,1)),
                     mean.model=list(armaOrder=c(0,0)),
                     distribution.model='std')
# 2. GJR-GARCH(1,1)
spec2 <- ugarchspec(
  variance.model = list(model="gjrGARCH", garchOrder=c(1,1)),
  mean.model = list(armaOrder=c(0,0), include.mean=TRUE),
  distribution.model="std"
)

# 3. EGARCH(1,1)
spec3 <- ugarchspec(
  variance.model = list(model="eGARCH", garchOrder=c(1,1)),
  mean.model = list(armaOrder=c(0,0), include.mean=TRUE),
  distribution.model="std"
)

fit1 <- ugarchfit(spec1, r)
fit2 <- ugarchfit(spec2, r)
fit3 <- ugarchfit(spec3, r)

# Compare by AIC/BIC
info <- rbind(
  GARCH11 = infocriteria(fit1),
  GJRGARCH = infocriteria(fit2),
  EGARCH = infocriteria(fit3)
)
info



''' 7. Consider the dataset m-ge2608.txt which contains the monthly returns of GE
stock. Fit GARCH(2,1) model with Gaussian, student-t and skew student-t inno-
vations. Plot the volatility of all three models and find correlation between them.
Also, plot ACF and PACF of square of standardized residuals of each model.'''

library(rugarch)
library(FinTS)


data <- read.table('m-ge2608.txt',header = TRUE)
r <- data$rtn

# GARCH(2,1)
spec1 <- ugarchspec(variance.model = list(model='sGARCH',garchOrder=c(2,1)),
                    mean.model = list(armaOrder=c(0,0)),
                    distribution.model = 'norm')
fit1 <- ugarchfit(spec1,r)

spec2 <- ugarchspec(variance.model = list(model='sGARCH',garchOrder=c(2,1)),
                    mean.model = list(armaOrder=c(0,0)),
                    distribution.model = 'std')
fit2 <- ugarchfit(spec2,r)

spec3 <- ugarchspec(variance.model = list(model='sGARCH',garchOrder=c(2,1)),
                    mean.model = list(armaOrder=c(0,0)),
                    distribution.model = 'sstd')
fit3 <- ugarchfit(spec3,r)


#  Extract conditional volatility
vol.norm <- sigma(fit1)
vol.t    <- sigma(fit2)
vol.sstd <- sigma(fit3)


#  Plot volatilities
plot(vol.norm, type="l", col="black", main="Volatility Comparison",ylab="Volatility")
lines(vol.t, col="blue")
lines(vol.sstd, col="red")


#Correlation between volatilities
cor.matrix <- cor(cbind(vol.norm, vol.t, vol.sstd))
print(cor.matrix)


# ACF & PACF of squared standardized residuals
z.norm  <- residuals(fit1, standardize = TRUE)
z.t     <- residuals(fit2, standardize = TRUE)
z.sstd  <- residuals(fit3, standardize = TRUE)

# Gaussian
acf(z.norm^2, main="ACF of z^2 (Gaussian)")
pacf(z.norm^2, main="PACF of z^2 (Gaussian)")

# Student-t
acf(z.t^2, main="ACF of z^2 (Student-t)")
pacf(z.t^2, main="PACF of z^2 (Student-t)")

# Skew Student-t
acf(z.sstd^2, main="ACF of z^2 (Skew Student-t)")
pacf(z.sstd^2, main="PACF of z^2 (Skew Student-t)")





























































