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
# Has 120 time points - lets plan to leave
# out the last 20 when we're fitting our models so we can visually 
# assess performance
sub.keep <- 1:100
sub.pred <- 101:length(y)

# Set the last 20 values to NA if you want to predict them
y.marss <- y
y.marss[sub.pred] <- NA


model <- list(
  B=matrix("phi"), U=matrix(0), Q=matrix("sig.sq.w"),
  Z=matrix("a"), A=matrix(0), R=matrix("sig.sq.v"),
  x0=matrix("mu"), tinitx=0 )

# Use EM to get starting values for direct maximization
fit <- MARSS(c(y.marss), model=model, method = "kem") 
# Direct maximization starting at EM starting values
fit.ml <- MARSS(c(y.marss), model = model, method = "BFGS", inits = fit)

forc <- fit.ml$ytT # E[y_t | y_1,...,y_m]
forc.se <- fit.ml$ytT.se

plot(expo$fdate, expo$exports, type = "l",
     xlab = "Date", ylab = "Exports",
     col = "gray", ylim = c(1500, 3000))
lines(expo$fdate[sub.pred], forc[sub.pred], col = "blue")
lines(expo$fdate[sub.pred], forc[sub.pred] + forc.se[sub.pred]*qnorm(0.025), 
      col = "blue",
      lty = 2)
lines(expo$fdate[sub.pred], forc[sub.pred] + forc.se[sub.pred]*qnorm(0.975), 
      col = "blue",
      lty = 2)
# Note - these are prediction intervals that do not account for uncertainty
# in estimating a, phi, sigma_w, sigma_v, mu

# Let's add covariates - make a design matrix of indicators for each month
covariates <- t(model.matrix(~month, data = expo)[, -1])

# We'll put the covariates in the observation part of the equation
model.covy <- list(B=matrix("phi"), U=matrix(0), Q=matrix("sig.sq.w"), 
                   Z=matrix("a"), A=matrix(0), R=matrix("sig.sq.v"),
                   x0=matrix("mu"), tinitx=0, 
                   D="unconstrained", d=covariates)

fit.covy <- MARSS(c(y.marss), model=model.covy, method = "kem")
fit.covy <- MARSS(c(y.marss), model=model.covy,  method = "BFGS",
                  inits = fit.covy)

forc.covy <- fit.covy$ytT
forc.covy.se <- fit.covy$ytT.se

plot(expo$fdate, expo$exports, type = "l",
     xlab = "Date", ylab = "Exports",
     col = "gray", ylim = c(1500, 3000))
lines(expo$fdate[sub.pred], forc[sub.pred], col = "blue")
lines(expo$fdate[sub.pred], forc[sub.pred] + forc.se[sub.pred]*qnorm(0.025), 
      col = "blue",
      lty = 2)
lines(expo$fdate[sub.pred], forc[sub.pred] + forc.se[sub.pred]*qnorm(0.975), 
      col = "blue",
      lty = 2)

lines(expo$fdate[sub.pred], forc.covy[sub.pred], col = "red")
lines(expo$fdate[sub.pred], forc.covy[sub.pred] + forc.covy.se[sub.pred]*qnorm(0.025), 
      col = "red",
      lty = 2)
lines(expo$fdate[sub.pred], forc.covy[sub.pred] + forc.covy.se[sub.pred]*qnorm(0.975), 
      col = "red",
      lty = 2)

# Now put covariates in state part of the equation
model.covx <- list(B=matrix("phi"), U=matrix(0), Q=matrix("sig.sq.w"), 
                   Z=matrix("a"), A=matrix(0), R=matrix("sig.sq.v"),
                   x0=matrix("mu"), tinitx=0,
                   C="unconstrained", c=covariates)

fit.covx <- MARSS(c(y.marss), model=model.covx, method = "kem")
fit.covx <- MARSS(c(y.marss), model=model.covx,  method = "BFGS",
                  inits = fit.covx)

forc.covx <- fit.covx$ytT
forc.covx.se <- fit.covx$ytT.se

plot(expo$fdate, expo$exports, type = "l",
     xlab = "Date", ylab = "Exports",
     col = "gray", ylim = c(1500, 3000))

lines(expo$fdate[sub.pred], forc.covy[sub.pred], col = "red")
lines(expo$fdate[sub.pred], forc.covy[sub.pred] + forc.covy.se[sub.pred]*qnorm(0.025), 
      col = "red",
      lty = 2)
lines(expo$fdate[sub.pred], forc.covy[sub.pred] + forc.covy.se[sub.pred]*qnorm(0.975), 
      col = "red",
      lty = 2)

lines(expo$fdate[sub.pred], forc.covx[sub.pred], col = "blue")
lines(expo$fdate[sub.pred], forc.covx[sub.pred] + forc.covx.se[sub.pred]*qnorm(0.025), 
      col = "blue",
      lty = 2)
lines(expo$fdate[sub.pred], forc.covx[sub.pred] + forc.covx.se[sub.pred]*qnorm(0.975), 
      col = "blue",
      lty = 2)

# Putting covariates in the state part of the equation appears to provide
# better forecasts in this case
# We can use criteria like AIC to compare the two models
AIC(fit.covx)
AIC(fit.covy)
# AIC also favors putting the covariates in the state part of the model

# You will get a warning if you try to put covariates in both! Try it!
model.covxy <- list(B=matrix("phi"), U=matrix(0), Q=matrix("sig.sq.w"), 
                   Z=matrix("a"), A=matrix(0), R=matrix("sig.sq.v"),
                   x0=matrix("mu"), tinitx=0,
                   C="unconstrained", c=covariates,
                   D="unconstrained", d=covariates)

fit.covxy <- MARSS(c(y.marss), model=model.covxy, method = "kem")
fit.covxy <- MARSS(c(y.marss), model=model.covxy,  method = "BFGS",
                  inits = fit.covx)

# Now - fit a stochastic regression model
n <- length(y)
model.streg <- list(B=matrix("phi"), U=matrix(0), Q=matrix("sig.sq.w"), 
                    x0=matrix("mu"), tinitx=0,
                    D="unconstrained", d=matrix(1, nrow = 1, ncol = n), # Add an intercept in the observation equation
                    Z=array(1:n, dim = c(1, 1, n)), A=matrix(0), 
                    R=matrix("sig.sq.v"))

fit.streg <- MARSS(c(y.marss), model=model.streg, method = "kem")
fit.streg <- MARSS(c(y.marss), model=model.streg,  method = "BFGS",
                   inits = fit.streg)

forc.streg <- fit.streg$ytT
forc.streg.se <- fit.streg$ytT.se

plot(expo$fdate, expo$exports, type = "l",
     xlab = "Date", ylab = "Exports",
     col = "gray", ylim = c(1000, 4000))
lines(expo$fdate[sub.pred], forc.streg[sub.pred], col = "blue")
lines(expo$fdate[sub.pred], forc.streg[sub.pred] + 
        forc.streg.se[sub.pred]*qnorm(0.025), 
      col = "blue",
      lty = 2)
lines(expo$fdate[sub.pred], forc.streg[sub.pred] + 
        forc.streg.se[sub.pred]*qnorm(0.975), 
      col = "blue",
      lty = 2)

