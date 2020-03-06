# Let's fit ARIMA models to our new dataset
# Exports from Massachusetts
rm(list = ls())

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

# Choose the orders of the MA and AR models we want to consider
ps <- 0:5
qs <- 0:4
aics <- matrix(NA, nrow = length(ps), ncol = length(qs))
conv <- matrix(NA, nrow = length(ps), ncol = length(qs))

for (p in ps) {
  cat("p=", p, "\n")
  for (q in qs) {
    cat("q=", q, "\n")
    # fitarma <- arima(y[sub], order = c(p, 0, q))
    fitarma <- arima(y[sub], order = c(p, 0, q), method = "ML")
    aics[which(p == ps), which(q == qs)] <- log(mean(sum(fitarma$residuals^2))) + 
      (100 + 2*(p + q + 1))/100
    conv[which(p == ps), which(q == qs)] <- fitarma$code
}
}
plot(ps, aics[, 1], pch = 16, xlab = "p", ylab = "AIC",
     ylim = range(aics), type = "n")
text(ps, aics[, 1], "0", col = 1, cex = 0.5)
text(ps, aics[, 2], "1", col = 2, cex = 0.5)
text(ps, aics[, 3], "2", col = 3, cex = 0.5)
text(ps, aics[, 4], "3", col = 4, cex = 0.5)
text(ps, aics[, 5], "4", col = 5, cex = 0.5)

min.inds <- which(aics == min(aics), arr.ind = TRUE)
ps[min.inds[1]]
qs[min.inds[2]]

# Our best model is ARMA(4, 3) - Let's play around with this a bit
p <- 4
q <- 3
fitarma <- arima(y[sub], order = c(p, 0, q), method = "ML")
acf(y[sub])
mu.hat <- fitarma$coef[p + q + 1]
theta.hat <- fitarma$coef[p + 1:q]
phi.hat <- fitarma$coef[1:p]
est.acf <- ARMAacf(ar = phi.hat, ma = theta.hat, lag.max = 20)
points(0:20, est.acf, col = "blue", pch = 16)
lines(0:20, est.acf, col = "blue", lty = 3)

# What about the one-step-ahead forecasts?
solve.dl <- function(n, theta = 0, phi = 0, sig.sq.w = 1) {
  C <- matrix(nrow = n, ncol = n)
  v <- rep(NA, n + 1)
  # Get a quick approximation to the variance
  marep <- ARMAtoMA(ar = phi, ma = theta, lag.max = 1000)
  gamma.x <- ARMAacf(ar = phi, ma = theta, 
                     lag.max = n)*(1 + sum(marep^2))*sig.sq.w
  gamma.x.0 <- gamma.x[1]
  C[1, 1] <- gamma.x[2]/gamma.x.0
  v[1] <- gamma.x.0
  v[2] <- v[1]*(1 - C[1, 1]^2)
  for (i in 2:n) {
    C[i, i] <- gamma.x[i + 1]
    for (j in 1:(i - 1)) {
      C[i, i] <- C[i, i] - C[i-1, j]*gamma.x[i - j + 1]
    }
    C[i, i] <- C[i, i]/v[i]
    for (j in (i-1):1) {
      C[i, j] <- C[i - 1, j] - C[i, i]*C[i - 1, i - j]
    }
    v[i + 1] <- v[i]*(1 - C[i, i]^2)
  }
  return(list("C"=C, "c.n" = C[nrow(C), ], 
              "v" = v, "v.n" = v[length(v)]))
}

dl <- solve.dl(n = length(y), theta = theta.hat, phi = phi.hat, 
               sig.sq.w = fitarma$sigma2)
plot(dl$v)
plot(y, pch = 16)
vars <- fits <- rep(NA, length(y))
for (i in 2:length(y)) {
  fits[i] <- mu.hat + sum(dl$C[i, 1:(i - 1)]*(y[(i - 1):1] - mu.hat))
  vars[i] <- dl$v[i + 1]
}
points(fits, pch = 16, col = rgb(0, 0, 1, 0.5))
polygon(c(1:length(y),
          length(y):1),
        c(fits +
            qnorm(0.025)*sqrt(vars),
          rev(fits +
                qnorm(0.975)*sqrt(vars))),
        border = FALSE, col = rgb(0, 0, 1, 0.5))
