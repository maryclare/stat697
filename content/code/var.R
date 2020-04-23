# Let's analyze the exports and strawberry data simultaneously
rm(list = ls())

load("~/Dropbox/Teaching/TimeSeries2020/stat697/content/data/expo.RData")
load("~/Dropbox/Teaching/TimeSeries2020/stat697/content/data/straw.RData")

comb <- merge(expo, straw, 
              by = c("fdate"),
              all = TRUE)
Y <- as.matrix(comb[!is.na(comb$price) & !is.na(comb$exports), c("exports", "price")])
Y <- apply(Y, 2, function(y) {(y - mean(y))/sd(y)})

plot(Y[, 1], Y[, 2],
     xlab = "Exports", ylab = "Stawberry Prices")
plot(Y[, 2], Y[, 1],
     ylab = "Exports", xlab = "Stawberry Prices")

Y.lag1 <- rbind(rep(NA, 2), 
                Y[-nrow(Y), ])

plot(Y.lag1[, 1], Y[, 2],
     xlab = "Exports", ylab = "Stawberry Prices")
plot(Y.lag1[, 2], Y[, 1],
     ylab = "Exports", xlab = "Stawberry Prices")

acf(Y)

ar.fit1 <- ar(Y, aic = FALSE, order.max = 1)
ar.fit2 <- ar(Y, aic = FALSE, order.max = 2)
ar.fit3 <- ar(Y, aic = FALSE, order.max = 3)

# If we want to estimate vector autoregressive
# processes with maximum likelihood we need to 
# download additional packages
# https://cran.r-project.org/web/views/TimeSeries.html

linmod <- lm(Y~Y.lag1+I(1:nrow(Y)))
linmod$coef
ar.fit1$ar[1 , , ]
