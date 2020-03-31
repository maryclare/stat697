rm(list = ls())

link <- url("http://maryclare.github.io/stat697/content/data/expo.RData")
load(link)
close(link) 

plot(expo$fdate, expo$exports)

y <- expo$expo
m <- 285

# By default - no intercept term! This means there is no time trend
arima.fit <- arima(y[1:m], order = c(3, 1, 1))
arima.fit

# What happens if we say "include.mean"?
arima(y[1:m], order = c(3, 1, 1), include.mean = TRUE) # Still no mean!

preds <- predict(arima.fit, n.ahead = length(y) - m)
lines(expo$fdate, c(y[1:m] - arima.fit$residuals, preds$pred), col = "blue")

# What if we really want to allow an intercept, i.e. time trend?
X <- model.matrix(~fdate, data = expo)
arima.fit <- arima(y[1:m], order = c(3, 1, 1), 
                  xreg = X[1:m, , 
                           drop = FALSE])
# Problem is that X includes an intercept, that is *not* identified
X <- model.matrix(~fdate-1, data = expo)
arima.fit <- arima(y[1:m], order = c(3, 1, 1), 
                   xreg = X[1:m, , 
                            drop = FALSE])

preds <- predict(arima.fit, n.ahead = length(y) - m,
                 newxreg = X[(m+1):nrow(X), , drop = FALSE])
lines(expo$fdate, c(y[1:m] - arima.fit$residuals,
                    preds$pred), col = "red")


