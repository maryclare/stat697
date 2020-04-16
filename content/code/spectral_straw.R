# Let's fit ARIMA models to the strawberry data now
rm(list = ls())

load("~/Dropbox/Teaching/TimeSeries2020/stat697/content/data/straw.RData")
plot(straw$fdate, straw$price, type = "l",
     xlab = "Date", ylab = "Price")
# Our time series will be price
y <- straw$price

sp <- spectrum(y, fast = FALSE, # If fast=TRUE, some extra 0's may get appendeded to x to make the FFT faster
         log = "no", # We want to look at this on the original scale right now
         detrend = FALSE, # We're not going to worry about subtracting off a linear trend
         taper = 0 # We haven't talked about tapering in class, so set this to 0
) 
head((1/sp$freq)[order(sp$spec, decreasing = TRUE)])
# The big peak corresponds to approximately annual trends - this is 
# super clear even from the raw periodogram, but let's see if smoothing
# changes much

spectrum(y, fast = FALSE, # If fast=TRUE, some extra 0's may get appendeded to x to make the FFT faster
         log = "no", # We want to look at this on the original scale right now
         detrend = FALSE, # We're not going to worry about subtracting off a linear trend
         taper = 0, # We haven't talked about tapering in class, so set this to 0
         span = 2) 
head((1/sp$freq)[order(sp$spec, decreasing = TRUE)])
spectrum(y, fast = FALSE, # If fast=TRUE, some extra 0's may get appendeded to x to make the FFT faster
         log = "no", # We want to look at this on the original scale right now
         detrend = FALSE, # We're not going to worry about subtracting off a linear trend
         taper = 0, # We haven't talked about tapering in class, so set this to 0
         span = 5) 
head((1/sp$freq)[order(sp$spec, decreasing = TRUE)])
# Smoothing shows the same patterns!

# What about the AIC minimizing AR estimate of the spectral density?
sp <- spec.ar(y, main = "Parametric SD Estimate")
mtext(gsub(") spectrum ", "", 
           gsub("(", "", sp$method, fixed = TRUE), fixed = TRUE), 3,
      line = 0.25)
head((1/sp$freq)[order(sp$spec, decreasing = TRUE)])

# Was this apparent from the ACF/PACF?
acf(y, main = "Autocorrelation Function")
pacf(y, main = "Autocorrelation Function")
# Not quite!

# ANOVA suggests that we should be including month effects in our
# model (which was readily apparent from the plot but it doesn't
# hurt to check). This should allow us to account for
# annual trends.
anova(lm(price~factor(month), data = straw))

# Let's look at the periodogram of the residuals after regressing
# out annual trends
r <- lm(price~factor(month), data = straw)$residuals

sp <- spectrum(r, fast = FALSE, # If fast=TRUE, some extra 0's may get appendeded to x to make the FFT faster
               log = "no", # We want to look at this on the original scale right now
               detrend = FALSE, # We're not going to worry about subtracting off a linear trend
               taper = 0 # We haven't talked about tapering in class, so set this to 0
) 
abline(v = 1/12, col = "blue") # There isn't a peak there anymore!
head((1/sp$freq)[order(sp$spec, decreasing = TRUE)])
# Now there isn't much of anything standing out in the periodogram

# What about the AIC-minimizing AR estimate of the spectral density
sp <- spec.ar(r, main = "Parametric SD Estimate")
mtext(gsub(") spectrum ", "", gsub("(", "", sp$method, fixed = TRUE), 
           fixed = TRUE), 3,
      line = 0.25)
# Controlling for similarities between observations that are one year
# apart eliminated basically all of the remaining time series
# dependence in our data!!
head((1/sp$freq)[order(sp$spec, decreasing = TRUE)])
