rm(list = ls())

# Define the autocovariance function for a mean-zero
# ARMA(1, 1) model
# ARMA(1, 0) = AR(1)
# ARMA(0, 1) = MA(1)
gamma.x <- function(h, phi1, theta1, sig.sq.w) {
  h <- abs(h) 
  if (h == 0) {
    g.x <- (theta1^2 + 2*phi1*theta1 + 1)*sig.sq.w/(1 - phi1^2)
  } else {
    g.x <- sig.sq.w*phi1^(h - 1)*((1 + theta1*phi1)*(phi1 + theta1)/(1 - phi1^2))
  }
  return(g.x)
}

# Durbin-Levinson algorithm, recovers all of the one-step-ahead prediction coefficients 
# up to one future prediction and returns the expected squared error loss of each 
# one-step-ahead prediction
solve.dl <- function(n, phi1 = 0, theta1 = 0, sig.sq.w = 1) {
  C <- matrix(nrow = n, ncol = n)
  v <- rep(NA, n + 1)
  gamma.x.0 <- gamma.x(0, 
                       phi1 = phi1, 
                       theta1 = theta1, 
                       sig.sq.w = sig.sq.w)
  C[1, 1] <- gamma.x(1, 
                     phi1 = phi1, 
                     theta1 = theta1, 
                     sig.sq.w = sig.sq.w)/gamma.x.0
  v[1] <- gamma.x.0
  v[2] <- v[1]*(1 - C[1, 1]^2)
  for (i in 2:n) {
    C[i, i] <- gamma.x(i, 
                       phi1 = phi1, 
                       theta1 = theta1, 
                       sig.sq.w = sig.sq.w)
    for (j in 1:(i - 1)) {
      C[i, i] <- C[i, i] - C[i-1, j]*gamma.x(i - j, 
                                             phi1 = phi1, 
                                             theta1 = theta1, 
                                             sig.sq.w = sig.sq.w)
    }
    C[i, i] <- C[i, i]/v[i]
    for (j in (i-1):1) {
      C[i, j] <- C[i - 1, j] - C[i, i]*C[i - 1, i - j]
    }
    v[i + 1] <- v[i]*(1 - C[i, i]^2)
  }
  return(list("C"=C, "c.n" = C[nrow(C), ], 
              "v" = v, "v.n" = v[length(v)]))
}

# Let's look at the coefficients and expected squared error losses!

# First, just consider AR(1)
phi1 <- 0.5
theta1 <- 0
sig.sq.w <- 1
n <- 5
dl <- solve.dl(n = n, phi1 = phi1, theta1 = theta1, sig.sq.w = sig.sq.w)
dl
# What's happening to our expected prediction loss as we get more data?
plot(dl$v, type = "b", ylim = c(0, 2))
abline(h = 0, lty = 3)

# What do the partial autocorrelation coefficients look like?
plot(diag(dl$C), 
     type = "b", ylab = expression(c[jj]), xlab = "j")
abline(h = 0, lty = 3)




