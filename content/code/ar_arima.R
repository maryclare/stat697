# Removes everything in the workspace
rm(list = ls())

# We're going to work with a new dataset!
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
# Has 120 time points - lets plan to leave
# out the last 20 when we're fitting our models so we can visually 
# assess performance
sub <- 1:100

# Make a matrix of lags - let's consider up to 20
ps <- 0:20
aics <- rep(NA, length(ps))
pred.vals <- matrix(NA, nrow = length(ps), ncol = 20)
pred.ses <- matrix(NA, nrow = length(ps), ncol = 20)
coefs <- matrix(NA, nrow = length(ps), ncol = length(ps))

for (p in ps) {
  cat("p=", p, "\n")
  fitar <- arima(y[sub], order = c(p, 0, 0))
  aics[which(p == ps)] <- log(mean(sum(fitar$residuals^2))) + 
    (100 + 2*(p + 1))/100
  if (p > 0) {
    coefs[which(p == ps), 1:p] <- fitar$coef[1:p]
  }
  predicts <-  predict(fitar, n.ahead = 20, se.fit = TRUE)
  pred.vals[which(p == ps), ] <- predicts$pred
  pred.ses[which(p == ps), ] <- predicts$se
}
plot(ps, aics, pch = 16, xlab = "p", ylab = "AIC")
abline(v = 13, lty = 2)

p <- 6
plot(expo$fdate, y, type = "l")
points(expo$fdate[101:120], pred.vals[which(ps == p), ], type = "l", col = "blue")
polygon(c(expo$fdate[101:120], rev(expo$fdate[101:120])),
        c(pred.vals[which(ps == p), ] + qnorm(0.975)*pred.ses[which(ps == p), ],
          rev(pred.vals[which(ps == p), ] + qnorm(0.025)*pred.ses[which(ps == p), ])),
        col = rgb(0, 0, 1, 0.5), border = FALSE)

p <- 4
acf(y[sub], lag.max = 50)
acf <- ARMAacf(ar = coefs[which(p == ps), 1:p], lag.max = 50)
lines(0:50, acf, col = "red")
