# Removes everything in the workspace
rm(list = ls())

load("~/Dropbox/Teaching/TimeSeries2020/stat697/content/data/straw.RData")
head(straw)

n <- nrow(straw)

# Finishing up the parametric bootstrap

# If we believe our model and the null hypothesis, 
# the observed data has the following
# distribution
linmod1 <- lm(straw$price~straw$fdate)
plot(straw$fdate, straw$price, type = "b", pch = 16)
lines(straw$fdate, fitted.values(linmod1), type = "l",
      col = "blue")
pred <- predict(linmod1, se.fit = TRUE)
lines(straw$fdate, pred$fit + qnorm(0.975)*pred$se.fit, col = "blue", 
      lty = 2)
lines(straw$fdate, pred$fit + qnorm(0.025)*pred$se.fit, col = "blue", 
      lty = 2)
# Do we think this is a good model?

nboot <- 1000
fit.vals <- matrix(NA, nrow = nboot, ncol = n)
for (i in 1:nboot) {
   by <- rnorm(n, mean = fitted.values(linmod1), 
               sd = summary(linmod1)$sigma)
   blinmod1 <- lm(by~straw$fdate, data = straw)
   fit.vals[i, ] <- fitted.values(blinmod1)
}
lines(straw$fdate, pred$fit + qnorm(0.975)*apply(fit.vals, 2, sd), col = "red", 
      lty = 2)
lines(straw$fdate, pred$fit + qnorm(0.025)*apply(fit.vals, 2, sd), col = "red", 
      lty = 2)
lines(straw$fdate, apply(fit.vals, 2, mean), col = "red", 
      lty = 2)

# Introduction to cross validation
# Let's try using cross validation
nfolds <- n
# Make a full design matrix
X <- cbind(model.matrix(~factor(straw$month)),
           straw$fdate,
           as.numeric(straw$fdate)^2,
           as.numeric(straw$fdate)^3,
           as.numeric(straw$fdate)^4,
           as.numeric(straw$fdate)^5)
max.degree <- ncol(X) - 12
test.mse <- matrix(NA, nrow = nfolds, ncol = max.degree)
for (o in 1:n) {
   y.test <- straw[o, "price"]
   X.test <- X[o, , drop = FALSE]
   
   y.train <- straw[-o, "price"]
   X.train <- X[-o, , drop = FALSE]
   for (i in 1:max.degree) {
      beta <- lm(y.train~X.train[, c(1:12, 12 + 1:i)]-1)$coef
      fit.test <- X.test[, c(1:12, 12 + 1:i), drop = FALSE]%*%beta
      test.mse[o, i] <- mean((y.test - fit.test)^2)
   }
}
plot(colMeans(test.mse), ylab = "Average MSE", xlab = "Max. Degree")
# We have a problem here...


linmod.naive <- lm(price~factor(month)+fdate+I(as.numeric(fdate)^2)+
                      I(as.numeric(fdate)^3),
                   data=straw)
linmod.smart <- lm(price~factor(month)+poly(fdate, 3),
                   data=straw)

pred.naive <- predict(linmod.naive, se.fit = TRUE)
pred.smart <- predict(linmod.smart, se.fit = TRUE)

plot(straw$fdate, straw$price,
     type = "b", pch = 16, xlab = "Date", ylab = "Price")
lines(straw$fdate, fitted(linmod.naive), col = "blue")
lines(straw$fdate, pred.naive$fit + qnorm(0.975)*pred.naive$se.fit, col = "blue", 
      lty = 2)
lines(straw$fdate, pred.naive$fit + qnorm(0.025)*pred.naive$se.fit, col = "blue", 
      lty = 2)

lines(straw$fdate, fitted(linmod.smart), col = "red")
lines(straw$fdate, pred.smart$fit + qnorm(0.975)*pred.smart$se.fit, col = "red", 
      lty = 2)
lines(straw$fdate, pred.smart$fit + qnorm(0.025)*pred.smart$se.fit, col = "red", 
      lty = 2)

linmod.naive <- lm(price~factor(month)+fdate+I(as.numeric(fdate)^2)+
                      I(as.numeric(fdate)^3)+I(as.numeric(fdate)^4),
                   data=straw)
linmod.smart <- lm(price~factor(month)+poly(fdate, 4),
                   data=straw)

pred.naive <- predict(linmod.naive, se.fit = TRUE)
pred.smart <- predict(linmod.smart, se.fit = TRUE)

plot(straw$fdate, straw$price,
     type = "b", pch = 16, xlab = "Date", ylab = "Price")
lines(straw$fdate, fitted(linmod.naive), col = "blue")
lines(straw$fdate, pred.naive$fit + qnorm(0.975)*pred.naive$se.fit, col = "blue", 
      lty = 2)
lines(straw$fdate, pred.naive$fit + qnorm(0.025)*pred.naive$se.fit, col = "blue", 
      lty = 2)

lines(straw$fdate, fitted(linmod.smart), col = "red")
lines(straw$fdate, pred.smart$fit + qnorm(0.975)*pred.smart$se.fit, col = "red", 
      lty = 2)
lines(straw$fdate, pred.smart$fit + qnorm(0.025)*pred.smart$se.fit, col = "red", 
      lty = 2)
# This is a *numerical* problem

# Let's try again, but use orthogonal polynomials
nfolds <- n
# Make a full design matrix
X <- cbind(model.matrix(~factor(straw$month)),
           poly(as.numeric(straw$fdate), 
                max.degree))
max.degree <- ncol(X) - 12
test.mse <- array(NA, dim = c(nfolds, max.degree))
aics <- aiccs <- bics <- rep(NA, max.degree)
for (i in 1:max.degree) {

   linmod <- lm(straw$price~X[, c(1:12, 12 + 1:i)]-1)
   msr <- mean(linmod$residuals^2)
   p <- ncol(X)
   aics[i] <- log(msr) + (n + 2*p)/n
   aiccs[i] <- log(msr) + (n + p)/(n - p - 2)
   bics[i] <- log(msr) + log(n)*p/n
   
   for (o in 1:n) {
      y.test <- straw[o, "price"]
      y.train <- straw[-o, "price"]
      X.test <- X[o, , drop = FALSE]
      X.train <- X[-o, , drop = FALSE]
      beta <- lm(y.train~X.train[, c(1:12, 12 + 1:i)]-1)$coef
      fit.test <- X.test[, c(1:12, 12 + 1:i), drop = FALSE]%*%beta
      test.mse[o, i] <- mean((y.test - fit.test)^2)
   }
   # Fit model to full data to get AIC/BIC/SIC
   
   
}
par(mfrow = c(2, 2))
plot(colMeans(test.mse), ylab = "1-Fold CV Average MSE", 
     xlab = "Max. Degree", pch = 16)
plot(aics, ylab = "AIC", xlab = "Max. Degree",
     pch = 16)
plot(aiccs, ylab = "AICc", xlab = "Max. Degree", pch = 16)
plot(bics, ylab = "SIC/BIC", xlab = "Max. Degree", pch = 16)

# How does this compare to the decision we would make based on AIC/BIC/SIC?