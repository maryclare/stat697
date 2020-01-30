# Removes everything in the workspace
rm(list = ls())

load("~/Dropbox/Teaching/TimeSeries2020/stat697/content/data/straw.RData")
head(straw)

n <- nrow(straw)

plot(straw$fdate, straw$price, type = "l")
plot(straw$fdate, straw$price, type = "b")
plot(straw$fdate, straw$price, type = "b", pch = 16,
     xlab = "Date", ylab = "Price")

linmod <- lm(price~factor(month), data = straw)
linmod <- lm(straw$price~factor(straw$month))

lines(straw$fdate, fitted.values(linmod), 
      col = "blue", lwd = 2)

# We should understand exactly what "lm" is doing! 
# First, construct the response y and covariate matrix X
y <- straw$price
# This is the slow, direct way. Could also do X <- model.matrix(~factor(straw$month))
X <- cbind(rep(1, n), 
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
sse <- sum((resids)^2)/(n - ncol(X))
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

lines(straw$fdate, pred$fit + pred$se.fit*qnorm(0.975), 
      col = "blue", lty = 2)
lines(straw$fdate, pred$fit - pred$se.fit*qnorm(0.975), 
      col = "blue", lty = 2)
lines(straw$fdate, pred$fit + pred$se.fit*qnorm(0.025), 
      col = "blue", lty = 2)

# One more comment - we could also do this with optim!
# This is pretty much where we stopped at the end of Thursday's class
linmod.obj <- function(beta, sigsq, y, X) {
  n <- length(y)
  n*log(sigsq)/2 + sum((y - X%*%beta)^2)/(2*sigsq)
}
linmod.optim <- function(pars, y, X) {
  beta <- pars[1:(length(pars) - 1)]
  sigsq <- exp(pars[length(pars)])
  linmod.obj(beta = beta, sigsq = sigsq, y = y, X = X)
}
opt <- optim(par = c(rnorm(ncol(X) + 1)), 
             fn = linmod.optim, 
             method = "L-BFGS-B", 
             y = y, X = X)
beta.opt <- opt$par[1:(length(opt$par) - 1)]
sigsq.opt <- exp(opt$par[length(opt$par)])
cbind(beta.opt, betas)
c(sigsq.opt*n/(n - ncol(X)), sse)

# Try the profile method
linmod.profobj <- function(beta, y, X) {
  n <- length(y)
  sigsq <- mean((y - X%*%beta)^2)
  n*log(sigsq)/2 + sum((y - X%*%beta)^2)/(2*sigsq)
}
proflinmod.optim <- function(pars, y, X) {
  beta <- pars
  linmod.profobj(beta = beta, y = y, X = X)
}
profopt <- optim(par = c(rnorm(ncol(X))), 
             fn = proflinmod.optim, 
             method = "L-BFGS-B", y = y, X = X)
cbind(beta.opt, profopt$par)
c(sigsq.opt, mean((y - X%*%profopt$par)^2))

plot(straw$fdate, residuals(linmod), type = "b", pch = 16)
abline(h = 0, lty = 3)

straw$t <- 1:nrow(straw)
plot(straw$fdate, straw$price, type = "b", pch = 16,
     xlab = "Date", ylab = "Price")
linmod.t <- lm(price~factor(month) + t + I(t^2), 
               data = straw)
lines(straw$fdate, fitted.values(linmod.t), col = "red", lwd = 2)

pred.t <- predict(linmod.t, se.fit = TRUE)

lines(straw$fdate, pred.t$fit + pred.t$se.fit*qnorm(0.975), 
      col = "red", lty = 2)
lines(straw$fdate, pred.t$fit - pred.t$se.fit*qnorm(0.975), 
      col = "red", lty = 2)

# Let's do a z-test of the quadratic term
X.t <- model.matrix(~factor(month)+t+I(t^2), 
                    data = straw)
sse <- summary(linmod.t)$sigma^2
t.stat <- coef(linmod.t)[14]/(sqrt(sse*solve(crossprod(X.t))[14, 14]))

alpha <- 0.05
qt(1 - alpha/2, df = nrow(X.t) - ncol(X.t))
qt(alpha/2, df = nrow(X.t) - ncol(X.t))
t.stat

2*pt(abs(t.stat), df = nrow(X.t) - ncol(X.t), lower.tail = FALSE)

### Intuition and introduction to the (parametric) bootstrap
# If we believe our model and the null hypothesis, 
# the observed data has the following
# distribution
linmod.t1 <- lm(price~factor(month) + t, data = straw)

nboot <- 1000
beta.vals <- rep(NA, nboot)
for (i in 1:nboot) {
  by <- rnorm(n, mean = fitted.values(linmod.t1), sd = summary(linmod.t1)$sigma)
  blinmod.t <- lm(by~factor(straw$month) + straw$t + I(straw$t^2), data = straw)
  beta.vals[i] <- blinmod.t$coef[length(blinmod.t$coef)]
}
hist(beta.vals)
abline(v = linmod.t$coef[length(linmod.t$coef)], col = "blue")

# Let's do a F-test of the month term - 
# fit two models, one without month indicators
linmod.nm <- lm(price~t+I(t^2), data = straw)

# Fast way - 
anova(linmod.nm, linmod.t)

# By hand
n <- nrow(straw)
p <-  length(linmod.t$coef)
p1 <- length(linmod.nm$coef)
ssr1 <- sum(linmod.nm$residuals^2)
ssr <- sum(linmod.t$residuals^2)
f.stat <- ((ssr1 - ssr)/ssr)*
  (n - p)/(p - p1)
f.stat
1 - pf(f.stat, p - p1, n - p)
