rm(list = ls())

link <- url("http://maryclare.github.io/stat697/content/data/expo.RData")
load(link)
close(link) 

plot(expo$fdate, expo$exports)

y <- expo$expo
m <- 285

X <- model.matrix(~fdate, data = expo)
arma.fit <- arima(y[1:m], order = c(3, 0, 1), 
                  xreg = X[1:m, , 
                           drop = FALSE],
                  include.mean = FALSE)
preds <- predict(arma.fit, n.ahead = length(y) - m,
                   newxreg = X[(m+1):nrow(X), , drop = FALSE])
lines(expo$fdate[(m+1):nrow(X)], preds$pred, col = "blue")
lines(expo$fdate[1:m], y[1:m] - arma.fit$residuals, col = "blue")

X <- model.matrix(~poly(fdate, 2), data = expo)
arma.fit <- arima(y[1:m], order = c(3, 0, 1), xreg = X[1:m, , 
                                                       drop = FALSE],
                  include.mean = FALSE)
preds <- predict(arma.fit, n.ahead = length(y) - m,
                 newxreg = X[(m+1):nrow(X), , drop = FALSE])
lines(expo$fdate[(m+1):nrow(X)], preds$pred, col = "red")
lines(expo$fdate[1:m], y[1:m] - arma.fit$residuals, col = "red")
