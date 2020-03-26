rm(list = ls())

library(forecast)

link <- url("http://maryclare.github.io/stat697/content/data/expo.RData")
load(link)
close(link) 

plot(expo$fdate, expo$exports)
abline(v = min(expo$fdate[expo$year >= 2010]),
       col = "blue")
y <- expo$exports

ndiffs(expo[expo$year >= 2010, "exports"], 
       alpha = 0.05, test = "adf", type = "level")
ndiffs(expo[expo$year >= 2010, "exports"], 
       alpha = 0.05, test = "pp", type = "level")

ndiffs(expo[expo$year >= 2010, "exports"], 
       alpha = 0.05, test = "adf", type = "trend")
ndiffs(expo[expo$year >= 2010, "exports"], 
       alpha = 0.05, test = "pp", type = "trend")


ndiffs(expo[expo$year < 2010, "exports"], 
       alpha = 0.05, test = "adf", type = "level")
ndiffs(expo[expo$year < 2010, "exports"], 
       alpha = 0.05, test = "pp", type = "level")

ndiffs(expo[expo$year < 2010, "exports"], 
       alpha = 0.05, test = "adf", type = "trend")
ndiffs(expo[expo$year < 2010, "exports"], 
       alpha = 0.05, test = "pp", type = "trend")

ndiffs(expo[, "exports"], 
       alpha = 0.05, test = "adf", type = "level")
ndiffs(expo[, "exports"], 
       alpha = 0.05, test = "pp", type = "level")

ndiffs(expo[, "exports"], 
       alpha = 0.05, test = "adf", type = "trend")
ndiffs(expo[, "exports"], 
       alpha = 0.05, test = "pp", type = "trend")

plot(y)
plot(y[-1] - y[-length(y)])
z <- y[-1] - y[-length(y)]
plot(z[-1] - z[-length(z)])
