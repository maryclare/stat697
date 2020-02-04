# Removes everything in the workspace
rm(list = ls())

load("~/Dropbox/Teaching/TimeSeries2020/stat697/content/data/straw.RData")
straw$dayssincestart <- as.numeric(straw$fdate) - min(as.numeric(straw$fdate))

# Let's use leave-one-out to pick the "best" model for the first three years of data
sub <- 1:(nrow(straw) - 12)

straw.poly.mses.iid <- matrix(nrow = length(sub), ncol = 11)
straw.poly.mses.dep <- matrix(nrow = 12, ncol = 11)


for (p in 0:(ncol(straw.poly.mses.iid) - 1)) {
  # IID Style Cross Validation
for (i in sub) {
    if (p == 0) {
      linmod.straw <- lm(price~factor(month), data = straw,
                         subset = sub[-i])
    } else {
      linmod.straw <- lm(price~poly(dayssincestart, p)+factor(month), data = straw,
                         subset = sub[-i])
      
    }
    straw.poly.mses.iid[i, p + 1] <- (straw$price[i] - predict(linmod.straw, straw)[i])^2
}
  for (j in 1:12) {
    
    if (p == 0) {
      linmod.straw <- lm(price~factor(month), data = straw,
                         subset = j - 1 + 1:24)
    } else {
      linmod.straw <- lm(price~poly(dayssincestart, p)+factor(month), data = straw,
                         subset = j - 1 + 1:24)
      
    }
    straw.poly.mses.dep[j, p + 1] <- (straw$price[j + 24] - predict(linmod.straw, straw)[j + 24])^2
    
  }
}
dev.off()
plot(colMeans(straw.poly.mses.iid), ylab = "Leave-One-Out MSE", 
     xlab = "Polynomial Degree", col = "blue")
points(colMeans(straw.poly.mses.dep), col = "red")
best.iid <- which(colMeans(straw.poly.mses.iid) == min(colMeans(straw.poly.mses.iid))) - 1
best.dep <- which(colMeans(straw.poly.mses.dep) == min(colMeans(straw.poly.mses.dep))) - 1

linmod.straw1 <- lm(price~poly(dayssincestart, best.iid)+factor(month), data = straw,
                   subset = sub)
linmod.straw2 <- lm(price~factor(month), data = straw,
                    subset = sub)


par(mfrow = c(1, 3))
plot(straw$fdate, straw$price, type = "l", pch = 16,
     ylim = c(1, 4), col = "gray",
     xlab = "Date", ylab = "Price")


pred.iid <- predict(linmod.straw1, straw, se.fit = TRUE)
lines(straw$fdate, pred.iid$fit, col = "blue")
polygon(c(straw$fdate, rev(straw$fdate)),
        c(pred.iid$fit + qnorm(0.975)*pred.iid$se.fit,
          rev(pred.iid$fit + qnorm(0.025)*pred.iid$se.fit)),
        col = rgb(0, 0, 1, 0.5), border = FALSE)

pred.dep <- predict(linmod.straw2, straw, se.fit = TRUE)
lines(straw$fdate, pred.dep$fit, col = "red")
polygon(c(straw$fdate, rev(straw$fdate)),
        c(pred.dep$fit + qnorm(0.975)*pred.dep$se.fit,
          rev(pred.dep$fit + qnorm(0.025)*pred.dep$se.fit)),
        col = rgb(1, 0, 0, 0.5), border = FALSE)
legend("bottomleft", col = c("blue", "red"), lty = c(1, 1),
       legend = c("IID", "One-Step-Ahead"),
       cex = 0.5)


plot(straw$fdate[1:length(sub)], 
     linmod.straw1$residuals, type = "l",
     xlab = "Date", ylab = "Residuals", xlim = range(straw$fdate),
     col = "blue")
lines(straw$fdate[1:length(sub)], 
     linmod.straw2$residuals, type = "l",
     col = "red")
abline(h = 0, lty = 3)

acf1 <- acf(linmod.straw1$residuals, plot = FALSE)
acf2 <- acf(linmod.straw2$residuals, plot = FALSE)
plot(acf1$lag, acf1$acf, type = "h", col = "blue",
     xlab = "Lag", ylab = expression(gamma[y](h)))
lines(acf2$lag + 0.25, acf2$acf, type = "h", col = "red")
abline(h = 0, lty = 2)
abline(h = qnorm(c(0.025, 0.975), 
                 mean = 0, 
                 sd = 1/sqrt(length(linmod.straw1$residuals))),
       lty = 3)
