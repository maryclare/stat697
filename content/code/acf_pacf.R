# Removes everything in the workspace
rm(list = ls())

load("~/Dropbox/Teaching/TimeSeries2020/stat697/content/data/straw.RData")
straw$dayssincestart <- as.numeric(straw$fdate) - min(as.numeric(straw$fdate))

# Let's apply a time series model to the residuals, having subtracted out a time trend
# and month effects
linmod.straw <- lm(price~factor(month)+dayssincestart, data = straw)
y <- linmod.straw$residuals

acf(y) # We would choose MA(0)
pacf(y) # We would choose AR(0)

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

acf(y, lag.max = 100)
pacf(y, lag.max = 100)
pacf(y, lag.max = 20)
