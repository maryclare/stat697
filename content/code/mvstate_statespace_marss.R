# Let's analyze the exports and strawberry data simultaneously
rm(list = ls())

library(MARSS)

load("~/Dropbox/Teaching/TimeSeries2020/stat697/content/data/expo.RData")
load("~/Dropbox/Teaching/TimeSeries2020/stat697/content/data/straw.RData")

comb <- merge(expo, straw, 
              by = c("fdate"),
              all = TRUE)
Y <- as.matrix(comb[!is.na(comb$price) & !is.na(comb$exports), c("exports", "price")])
Y <- apply(Y, 2, function(y) {(y - mean(y))/sd(y)})

# Has 48 time points - lets plan to leave
# out the last 12 when we're fitting our models so we can visually 
# assess performance
sub.keep <- 1:36
sub.pred <- 37:48

# Set the last 12 values to NA if you want to predict them
Y.marss <- Y
Y.marss[sub.pred, ] <- NA

plot(Y[, 1], Y[, 2],
     xlab = "Exports", ylab = "Stawberry Prices")
plot(Y[, 2], Y[, 1],
     ylab = "Exports", xlab = "Stawberry Prices")

# Simplest example first - probably won't give good forecasts but worth trying!
# It only lets us have a single latent time series, and we assume the elements of phi
# and a are all 1. We also assume that the errors in the observation equation are uncorrelated.
# Fitting the model
# y_t = x_t + u_t, u_t is normal, dimension 2, mean 0, independent elements, with variances 
# x_t = x_{t-1} + v_t, x_1 = mu
model <-  list(
  B=matrix(1), # The autoregressive parameter(s) of the latent process
  U=matrix(0, 1, 1), # We're ignoring this - specific quantity described in MARSS software we won't use
  Q=matrix("q"), # Noise variance of the latent process
  Z=matrix(1,2,1), # Observation equation, our a vector in our notes, 
                   # determines how latent time series process gets taken up into our observed process
  A=matrix(0, 2, 1), # We're ignoring this - specific quantity described in MARSS software we won't use
  R="diagonal and unequal", # Variance-covariance matrix of the observed process is diagonal with unequal variances
  x0=matrix("mu"),
  tinitx=1)

fit <- MARSS(t(Y.marss), model=model, method = "kem")
fit <- MARSS(t(Y.marss), model=model, method = "BFGS", inits = fit)

par(mfrow = c(1, 2))
plot(Y[, 1], col = "gray", type = "l")
lines(sub.pred, fit$ytT[1, sub.pred])
lines(sub.pred, fit$ytT[1, sub.pred] + qnorm(0.975)*fit$ytT.se[1, sub.pred], lty = 2)
lines(sub.pred, fit$ytT[1, sub.pred] + qnorm(0.025)*fit$ytT.se[1, sub.pred], lty = 2)

plot(Y[, 2], col = "gray", type = "l")
lines(sub.pred, fit$ytT[2, sub.pred])
lines(sub.pred, fit$ytT[2, sub.pred] + qnorm(0.975)*fit$ytT.se[2, sub.pred], lty = 2)
lines(sub.pred, fit$ytT[2, sub.pred] + qnorm(0.025)*fit$ytT.se[2, sub.pred], lty = 2)

# Lets more parameters vary, but keep a single underlying latent process
model <-  list(
  B=matrix("gamma", 1, 1), # The autoregressive parameter(s) of the latent process
  U=matrix(0, 1, 1), # We're ignoring this - specific quantity described in MARSS software we won't use
  Q=matrix("q"), # Noise variance of the latent process
  Z=matrix(c("a1", "a2"),2,1), # Observation equation that relates observed process to latent process
  A=matrix(0, 2, 1), # We're ignoring this - specific quantity described in MARSS software we won't use
  R="unconstrained", # Variance-covariance matrix of the observed process
  x0=matrix("mu"),
  tinitx=1)

fit <- MARSS(t(Y.marss), model=model, method = "kem")
fit <- MARSS(t(Y.marss), model=model, method = "BFGS", inits = fit)

par(mfrow = c(1, 2))
plot(Y[, 1], col = "gray", type = "l")
lines(sub.pred, fit$ytT[1, sub.pred])
lines(sub.pred, fit$ytT[1, sub.pred] + qnorm(0.975)*fit$ytT.se[1, sub.pred], lty = 2)
lines(sub.pred, fit$ytT[1, sub.pred] + qnorm(0.025)*fit$ytT.se[1, sub.pred], lty = 2)

plot(Y[, 2], col = "gray", type = "l")
lines(sub.pred, fit$ytT[2, sub.pred])
lines(sub.pred, fit$ytT[2, sub.pred] + qnorm(0.975)*fit$ytT.se[2, sub.pred], lty = 2)
lines(sub.pred, fit$ytT[2, sub.pred] + qnorm(0.025)*fit$ytT.se[2, sub.pred], lty = 2)

# Now let's try a MUCH more complicated model

model <-  list(
  B=matrix(c("gamma11", "gamma21", "gamma12", "gamma22"), nrow = 2, ncol = 2),
  U=matrix(0, 2, 1),
  Q="unconstrained",
  Z=matrix(c("a11", "a21", "a12", "a22"),2, 2), # Observation equation
  A=matrix(0, 2, 1),
  R="unconstrained",
  x0=matrix("mu", 2, 1),
  tinitx=1)

# This takes a long time!! You have been warned.
fit <- MARSS(t(Y.marss), model=model, method = "kem")
fit <- MARSS(t(Y.marss), model=model, method = "BFGS", inits = fit)

par(mfrow = c(1, 2))
plot(Y[, 1], col = "gray", type = "l")
lines(sub.pred, fit$ytT[1, sub.pred])
lines(sub.pred, fit$ytT[1, sub.pred] + qnorm(0.975)*fit$ytT.se[1, sub.pred], lty = 2)
lines(sub.pred, fit$ytT[1, sub.pred] + qnorm(0.025)*fit$ytT.se[1, sub.pred], lty = 2)

plot(Y[, 2], col = "gray", type = "l")
lines(sub.pred, fit$ytT[2, sub.pred])
lines(sub.pred, fit$ytT[2, sub.pred] + qnorm(0.975)*fit$ytT.se[2, sub.pred], lty = 2)
lines(sub.pred, fit$ytT[2, sub.pred] + qnorm(0.025)*fit$ytT.se[2, sub.pred], lty = 2)

par(mfrow = c(1, 2))
plot(fit$states[1, ], type = "l")
plot(fit$states[2, ], type = "l")
