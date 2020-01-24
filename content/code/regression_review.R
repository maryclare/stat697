rm(list = ls())

load("~/Dropbox/Teaching/TimeSeries2020/stat697/content/data/straw.RData")
head(straw)

plot(straw$fdate, straw$price, type = "l")
plot(straw$fdate, straw$price, type = "b")
plot(straw$fdate, straw$price, type = "b", pch = 16,
     xlab = "Date", ylab = "Price")

linmod <- lm(price~factor(month), data = straw)
linmod <- lm(straw$price~factor(straw$month))

lines(straw$fdate, fitted.values(linmod), col = "blue", lwd = 2)

# We should understand exactly what "linmod" is doing! 
# First, construct the response y and covariate matrix X
y <- straw$price
# This is the slow, direct way. Could also do X <- model.matrix(~factor(straw$month))
X <- cbind(rep(1, nrow(straw)), 
           straw$month == "02",
           straw$month == "03",
           straw$month == "04",
           straw$month == "05",
           straw$month == "06",
           straw$month == "07",
           straw$month == "08",
           straw$month == "09",
           straw$month == "10",
           straw$month == "11",
           straw$month == "12")
dim(X)
betas <- solve(crossprod(X))%*%crossprod(X, y)
# Check regression coefficients!
cbind(summary(linmod)$coefficients[, 1], betas)

preds <- X%*%betas
resids <- y - preds
sse <- sum((resids)^2)/(nrow(X) - ncol(X))
# Check error variance!
c(sqrt(sse), summary(linmod)$sigma)

betas.cov <- solve(crossprod(X))*sse
# Check beta standard errors!
cbind(summary(linmod)$coef[, 2], sqrt(diag(betas.cov)))

# We can get predictions and standard errors from the predict function
pred <- predict(linmod, se.fit = TRUE)
# But we should know how they're generated! We already have
# predictions from above
cbind(pred$fit, preds); all.equal(as.numeric(pred$fit), as.numeric(preds))
# How do we get the standard errors? Well, the predictions are
# X\hat{\beta}, so their standard errors are sqrt(diag(X\Sigma_\betaX'))
pred.ses <- sqrt(diag(X%*%betas.cov%*%t(X)))
cbind(pred$se.fit, pred.ses) # Looks good!

# One more comment - we could also do this with optim!
# This is pretty much where we stopped at the end of Thursday's class
linmod.obj <- function(beta, sigsq, y, X) {
  length(y)*log(sigsq)/2 + sum((y - X%*%beta)^2)/sigsq
}
linmod.optim <- function(pars, y, X) {
  beta <- pars[1:(length(pars) - 1)]
  sigsq <- exp(pars[length(pars)])
  linmod.obj(beta = beta, sigsq = sigsq, y = y, X = X)
}
opt <- optim(par = c(rnorm(ncol(X) + 1)), fn = linmod.optim, 
             method = "L-BFGS-B", y = y, X = X)
beta.opt <- opt$par[1:(length(opt$par) - 1)]
sigsq.opt <- exp(opt$par[length(opt$par)])

lines(straw$fdate, pred$fit + pred$se.fit*qnorm(0.975), 
      col = "blue", lty = 2)
lines(straw$fdate, pred$fit - pred$se.fit*qnorm(0.975), 
      col = "blue", lty = 2)
lines(straw$fdate, pred$fit + pred$se.fit*qnorm(0.025), 
      col = "blue", lty = 2)

plot(residuals(linmod), type = "b", pch = 16)
abline(h = 0, lty = 3)

straw$t <- 1:nrow(straw)
plot(straw$fdate, straw$price, type = "b", pch = 16,
     xlab = "Date", ylab = "Price")
linmod.t <- lm(price~factor(month) + t + I(t^2), data = straw)
lines(straw$fdate, fitted.values(linmod.t), col = "red", lwd = 2)

pred.t <- predict(linmod.t, se.fit = TRUE)

lines(straw$fdate, pred.t$fit + pred.t$se.fit*qnorm(0.975), 
      col = "red", lty = 2)
lines(straw$fdate, pred.t$fit - pred.t$se.fit*qnorm(0.975), 
      col = "red", lty = 2)

plot(straw$fdate, residuals(linmod.t), ylab = "Residuals",
     xlab = "Date", type = "b", pch = 16)
abline(h = 0, lty = 3)

# Let's do a z-test of the quadratic term
X.t <- model.matrix(~factor(month)+t+I(t^2), data = straw)
sse <- summary(linmod.t)$sigma^2
t.stat <- coef(linmod.t)[14]/(sqrt(sse*solve(crossprod(X.t))[14, 14]))

qt(0.975, df = nrow(X.t) - ncol(X.t))
2*pt(abs(t.stat), df = nrow(X.t) - ncol(X.t), lower.tail = FALSE)




straw$price.l <- c(NA, straw$price[-nrow(straw)])
plot(straw$fdate, straw$price, type = "b", pch = 16,
     xlab = "Date", ylab = "Price")
linmod.lag <- lm(price~factor(month) + price.l, data = straw)
lines(straw$fdate[-1], fitted.values(linmod.lag), col = "red", lwd = 2)

pred.lag <- predict(linmod.lag, se.fit = TRUE)

lines(straw$fdate[-1], pred.lag$fit + pred.lag$se.fit*qnorm(0.975), 
      col = "red", lty = 2)
lines(straw$fdate[-1], pred.lag$fit - pred.lag$se.fit*qnorm(0.975), 
      col = "red", lty = 2)

# AIC/t-test(z-test)/F-test
# Compute fitted value intervals by hand?


# Do simulations with AR-1 correlation structure? Parametric bootstrap?