# Now let's type up the innovation algorithm
solve.innov <- function(n, phi = 0, theta = 0, sig.sq.w = 1) {
  D <- matrix(nrow = n, ncol = n)
  v <- rep(NA, n + 1)
  acv <- arma.acv(lag.max = n, phi = phi, theta = theta,
                  sig.sq = sig.sq.w, corr = FALSE)
  gamma.x.0 <- acv[1]
  v[1] <- gamma.x.0
  for (i in 1:n) {
    for (j in 0:(i-1)) {
      D[i, i - j] <- acv[abs(i - j) + 1]
      if (j > 0) {
        for (k in 0:(j - 1)) {
          D[i, i - j] <- D[i, i - j] - D[j, j - k]*D[i, i - k]*v[k + 1]
        }
      }
      D[i, i - j] <- D[i, i - j]/v[j + 1]
    }
    v[i + 1] <- gamma.x.0 - sum(D[i, i:1]^2*v[1:i], na.rm = TRUE)
  }
  return(list("D"=D, "d.n" = D[nrow(D), ], 
              "v" = v, "v.n" = v[length(v)]))
}

# Now let's write up a function that gives fitted values,
# predictions, and variances obtained from the innovations algorithm
get.preds <- function(y, phi, theta, sig.sq.w, h = 0) {
  innov <- solve.innov(n = length(y) - 1 + h, phi = phi, theta = theta, sig.sq.w = sig.sq.w)
  pred.vars <- pred.vals <- rep(NA, length(y) + h)
  pred.vals[1] <- 0
  pred.vars[1:length(y)] <- innov$v[1:length(y)]
  for (i in 2:(length(y))) {
    pred.vals[i] <- sum(innov$D[i - 1, 1:(i - 1)]*(y[(i-1):1] - pred.vals[(i - 1):1]))
  }
  if (h > 0) {
    for (i in 1:h) {
      pred.vals[length(y) + i] <- sum(innov$D[length(y) + i - 1, (1 + i - 1):(length(y) + i - 1)]*(y[length(y):1] - pred.vals[length(y):1]))
      pred.vars[length(y) + i] <- innov$v[1] - sum(innov$D[length(y) + i - 1, (1 + i - 1):(length(y) + i - 1)]^2*(innov$v[length(y):1]))
    }
  }
  
  return(list("preds" = pred.vals, "ses" = sqrt(pred.vars)))
}