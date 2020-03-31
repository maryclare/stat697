rm(list = ls())

link <- url("http://maryclare.github.io/stat697/content/data/expo.RData")
load(link)
close(link)

plot(expo$fdate, expo$exports)

y <- expo$expo
m <- 285

arima.fit1 <- arima(y[1:m], order = c(1, 1, 0),
                  include.mean = TRUE)
z <- y[-1] - y[-length(y)]
arima.fit2 <- arima(z[1:(m-1)], order = c(1, 0, 0), 
                    include.mean = FALSE)

preds1 <- predict(arima.fit1, n.ahead = 3)
preds2 <- predict(arima.fit2, n.ahead = 3)

# Predictions are the same
preds1$pred[1:3]
cumsum(preds2$pred) + y[m]
pred1 <- y[m] + -0.3801*(y[m] - y[m - 1])
pred2 <- pred1 + -0.3801*(pred1 - y[m])
pred3 <- pred2 + -0.3801*(pred2 - pred1)


# Standard errors are different
preds1$se[1:3]
preds2$se[1:3] 
