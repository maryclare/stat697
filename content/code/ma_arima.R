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
qs <- 0:20
aics <- rep(NA, length(qs))
pred.vals <- matrix(NA, nrow = length(qs), 
                    ncol = 20)
pred.ses <- matrix(NA, nrow = length(qs), 
                   ncol = 20)
coefs <- matrix(NA, nrow = length(qs), 
                ncol = length(qs))

for (q in qs) {
  cat("q=", q, "\n")
  fitma <- arima(y[sub], order = c(0, 0, q))
  aics[which(q == qs)] <- log(mean(sum(fitma$residuals^2))) + 
    (100 + 2*(q + 1))/100
  if (q > 0) {
    coefs[which(q == qs), 1:q] <- fitma$coef[1:q]
  }
  predicts <-  predict(fitma, n.ahead = 20, se.fit = TRUE)
  pred.vals[which(q == qs), ] <- predicts$pred
  pred.ses[which(q == qs), ] <- predicts$se
}
plot(qs, aics, pch = 16, xlab = "p", ylab = "AIC")
abline(v = 18, lty = 2)

q <- 1
plot(expo$fdate, y, type = "l")
points(expo$fdate[101:120], pred.vals[which(qs == q), ], type = "l", col = "blue")
polygon(c(expo$fdate[101:120], rev(expo$fdate[101:120])),
        c(pred.vals[which(qs == q), ] + qnorm(0.975)*pred.ses[which(qs == q), ],
          rev(pred.vals[which(qs == q), ] + qnorm(0.025)*pred.ses[which(qs == q), ])),
        col = rgb(0, 0, 1, 0.5), border = FALSE)

p <- 4
acf(y[sub], lag.max = 50)
acf <- ARMAacf(ma = coefs[which(q == qs), 1:q], lag.max = 50)
lines(0:50, acf, col = "red")
