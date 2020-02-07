
rm(list = ls())

set.seed(1)

n <- 100 # Length of simulated time series
sig.sq <- 1 
t <- 1:n

# Mean nonstationarity
beta <- 3/n
y <- beta*t + rnorm(n, mean = 0, sd = sqrt(sig.sq))

par(mfrow = c(2, 2))
par(mar = rep(4, 4))
plot(y, type = "l",
     xlab = "Time")
abline(h = 0, lty = 2)

acf(y, main = "")

plot(lm(y~t)$residuals, type = "l",
     xlab = "Time", ylab = "r")
abline(h = 0, lty = 2)

acf(lm(y~t)$residuals, main = "")

# Variance nonstationarity
beta <- 3/n
y <- rnorm(n, mean = 0, 
           sd = sqrt(sig.sq*exp(beta*t)))

par(mfrow = c(2, 2))
par(mar = rep(4, 4))
plot(y, type = "l",
     xlab = "Time")
abline(h = 0, lty = 2)

acf(y, main = "")

r <- lm(y~t)$residuals
linmod.r <- lm(I(log(r^2))~t)
hat.sig.sq <- exp(linmod.r$coefficients[1])
hat.beta <- linmod.r$coefficients[2]

plot(r/sqrt(hat.sig.sq*exp(hat.beta*t)), type = "l",
     xlab = "Time", ylab = "r")
abline(h = 0, lty = 2)

acf(r/sqrt(hat.sig.sq*exp(hat.beta*t)), main = "")

