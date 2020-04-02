# Removes everything in the workspace
rm(list = ls())

library(MARSS)

# Exports from Massachusetts
load("~/Dropbox/Teaching/TimeSeries2020/stat697/content/data/expo.RData")
plot(expo$fdate, expo$exports, type = "l",
     xlab = "Date", ylab = "Exports")
# This looks approximately stationary after 2010, 
# so let's just look at that
expo <- expo[expo$year >= 2010, ]
plot(expo$fdate, expo$exports, type = "l",
     xlab = "Date", ylab = "Exports")
# Our time series will be the # of exports
y <- expo$exports
length(y) 

model <- list(
  B=matrix("phi"), U=matrix(0), Q=matrix("sig.sq.w"),
  Z=matrix("a"), A=matrix(0), R=matrix("sig.sq.v"),
  x0=matrix("mu"), tinitx=0 )

fit <- MARSS(c(y), model=model, method = "kem")
# Get Kalman filter, predictor and smoother
kf <- MARSSkfss(fit)

par(mfrow = c(3, 1))
par(mar = rep(1, 4))
# Kalman Predictions
plot(expo$fdate, expo$exports, type = "l",
     xlab = "Date", ylab = "Exports", col = "darkgray")
lines(expo$fdate,
      c(kf[["xtt1"]]), col = "red", lwd = 2)
lines(expo$fdate,
      c(kf[["xtt1"]]) + qnorm(0.025)*c(sqrt(kf[["Vtt1"]])), col = "red", lwd = 2, lty = 3)
lines(expo$fdate,
      c(kf[["xtt1"]]) + qnorm(0.975)*c(sqrt(kf[["Vtt1"]])), col = "red", lwd = 2, lty = 3)

# Filter
plot(expo$fdate, expo$exports, type = "l",
     xlab = "Date", ylab = "Exports", col = "darkgray")
lines(expo$fdate,
      c(kf[["xtt"]]), col = "blue", lwd = 2)
lines(expo$fdate,
      c(kf[["xtt"]]) + qnorm(0.025)*c(sqrt(kf[["Vtt1"]])), col = "blue", lwd = 2, lty = 3)
lines(expo$fdate,
      c(kf[["xtt"]]) + qnorm(0.975)*c(sqrt(kf[["Vtt1"]])), col = "blue", lwd = 2, lty = 3)

# Smoother
plot(expo$fdate, expo$exports, type = "l",
     xlab = "Date", ylab = "Exports", col = "darkgray")
lines(expo$fdate,
      c(fit$states), col = "darkgreen", lwd = 2)
lines(expo$fdate,
      c(fit$states) + qnorm(0.025)*c(fit$states.se), col = "darkgreen", lwd = 2, lty = 3)
lines(expo$fdate,
      c(fit$states) + qnorm(0.975)*c(fit$states.se), col = "darkgreen", lwd = 2, lty = 3)

