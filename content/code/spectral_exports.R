# Let's fit ARIMA models to our new dataset
# Exports from Massachusetts
rm(list = ls())

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

# Let's look at the periodogram
sp <- spectrum(y, fast = FALSE, # If fast=TRUE, some extra 0's may get appendeded to x to make the FFT faster
         log = "no", # We want to look at this on the original scale right now
         detrend = FALSE, # We're not going to worry about subtracting off a linear trend
         taper = 0 # We haven't talked about tapering in class, so set this to 0
) 
# It's very noisy, but there appear to be some very distinct peaks
# What frequencies to the peaks correspond to?
head((sp$freq)[order(sp$spec, decreasing = TRUE)])
# Frequencies are not so easy to interpret - let's convert
# them to time intervals
head((1/sp$freq)[order(sp$spec, decreasing = TRUE)])
# It looks like there observations that are 3 months apart
# tend to be similar, also observations 40 months apart, 
# 6 months apart, two years apart
# But this is very noisy! Maybe smoothing the periodogram by 
# averaging consecutive values will help

sp <- spectrum(y, fast = FALSE, # If fast=TRUE, some extra 0's may get appendeded to x to make the FFT faster
         log = "no", # We want to look at this on the original scale right now
         detrend = FALSE, # We're not going to worry about subtracting off a linear trend
         taper = 0, # We haven't talked about tapering in class, so set this to 0
         span = 2) 
head((1/sp$freq)[order(sp$spec, decreasing = TRUE)])
# We're still seeing peaks at frequencies that correspond to 
# similarity of observations 3, 40, 30, and 24 months apart

# Let's smooth a little more and see how things change
# Average over 5 neighboring periodogram values
sp <- spectrum(y, fast = FALSE, # If fast=TRUE, some extra 0's may get appendeded to x to make the FFT faster
         log = "no", # We want to look at this on the original scale right now
         detrend = FALSE, # We're not going to worry about subtracting off a linear trend
         taper = 0, # We haven't talked about tapering in class, so set this to 0
         span = 5) 
head((1/sp$freq)[order(sp$spec, decreasing = TRUE)])
# This smooths away the frequency that corresponds to
# similarity of observations three months apart,
# It seems reasonable to believe that there are
# relationships between observations that are three months
# apart because of some features of the business cycle

# As one other way of smoothing the periodogram,
# let's look at the spectral density of the AIC minimizing
# AR(p) model
sp <- spec.ar(y, main = "Parametric SD Estimate")
mtext(gsub(") spectrum ", "", gsub("(", "", sp$method, fixed = TRUE), 
           fixed = TRUE), 3,
      line = 0.25)
# This gives us a continuous estimate of the spectral density - 
# as our estimate of the spectral density is just the spectral
# density of an AR(15) process with parameters as estimated from the
# data
head((1/sp$freq)[order(sp$spec, decreasing = TRUE)])
# This provides more evidence of 
# similarity of observations that are three or thirty months apart

# Not that this was *not* easy to see from the autocorrelation
# or partial autocorrelation function
acf(y, main = "Autocorrelation Function")
pacf(y, main = "Partial Autocorrelation Function")

# Now - let's construct an indicator that lets observations
# that are three months apart share a common overall level
expo$three <- NA
expo$three[expo$month %in% c("01", "04", "07", "10")] <- 1
expo$three[expo$month %in% c("02", "05", "08", "11")] <- 2
expo$three[expo$month %in% c("03", "06", "09", "12")] <- 3

# ANOVA suggests that this is something that we should
# include in our model
anova(lm(exports~factor(three), data = expo))

# Let's look at the periodogram of the residuals to see
# if regressing out a common mean for observations three months apart
# eliminates the corresponding peak we saw in the periodogram
r <- lm(exports~factor(three), data = expo)$residuals

sp <- spectrum(r, fast = FALSE, # If fast=TRUE, some extra 0's may get appendeded to x to make the FFT faster
               log = "no", # We want to look at this on the original scale right now
               detrend = FALSE, # We're not going to worry about subtracting off a linear trend
               taper = 0 # We haven't talked about tapering in class, so set this to 0
) 
abline(v = 1/3, col = "blue") # There isn't a peak there anymore!
head((1/sp$freq)[order(sp$spec, decreasing = TRUE)])

# Is this just noise? Let's look at the AIC-minimizing AR estimate
# of the spectral density
sp <- spec.ar(r, main = "Parametric SD Estimate")
mtext(gsub(") spectrum ", "", gsub("(", "", sp$method, fixed = TRUE), 
           fixed = TRUE), 3,
      line = 0.25)
# Controlling for similarities between observations that are three
# months apart eliminated a lot of the complexity in our data!!
# The AIC minimizing model is a very simple one!
head((1/sp$freq)[order(sp$spec, decreasing = TRUE)])
