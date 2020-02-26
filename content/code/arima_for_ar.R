# Removes everything in the workspace
rm(list = ls())

load("~/Dropbox/Teaching/TimeSeries2020/stat697/content/data/straw.RData")
straw$dayssincestart <- as.numeric(straw$fdate) - min(as.numeric(straw$fdate))

# Let's apply a time series model to the residuals, having subtracted out a time trend
# and month effects
linmod.straw <- lm(price~factor(month)+dayssincestart, data = straw)
y <- linmod.straw$residuals

# The arima function estimates the parameters of an AR-p model
ar0 <- arima(y, order = c(0, 0, 0)) # Fits an AR-0 model
ar1 <- arima(y, order = c(1, 0, 0)) # Fits an AR-1 model
ar2 <- arima(y, order = c(2, 0, 0)) # Fits an AR-2 model

# We can view the parameter estimates, standard errors by printing the arima object
ar1
names(ar1) # There are lots of useful quantities we can extract from the AR objject

# By default, arima will get initial estimates of the parameters using
# the conditional sums of squares method, i.e. conditioning on the first p values
# and treating the problem like a linear regression problem. Then it plugs
# those starting values in to a numerical optimization routine that finds the
# parameter values that maximize the likelihood. There are other options,
# which can be needed if the conditional sums of squares method returns an
# estimate of phi_1,...,phi_p that corresponds to a non-stationary model

arima(y, order = c(20, 0, 0), method = "CSS-ML") # Default
arima(y, order = c(20, 0, 0), method = "ML") # Alternative, picks a starting value that will
# definitely be stationary
