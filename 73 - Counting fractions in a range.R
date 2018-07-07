# For any given positive integer `n`, the number of proper reduced 
# fractions includes the number of reduced fractions given `n-1` + 
# additional fractions that depend solely on `n`. If `f(n)` is 
# total number of reduced proper fractions for `n`, then 
# `f(n) = nfrac(n) + f(n - 1)`, where `nfrac(n)` is the contribution 
# by n.

sieve <- function(n) {
  a <- c(2:n)
  l <- 2
  r <- c()
  while (l*l <= n) {
    r <- c(r, a[1])
    a <- a[-(which(a %% l ==0))]
    l <- a[1]
  }
  c(r, a)
}

nfrac <- function(n, sieveN) {
  cfMatrix <- matrix(data = 1L, ncol = n, nrow = n)
  for (i in sieveN) {
    maxIdx <- floor(n / i)
    cfMatrix[i * (1:maxIdx), i * (1:maxIdx)] <- 0L
  }
  cfMatrix[lower.tri(cfMatrix)] <- NA_integer_
  out <- which(cfMatrix == 1L, arr.ind = TRUE)
  out <- cbind(out, out[, 1] / out[, 2])
  out <- out[order(out[, 3]), ]
  colnames(out) <- c('row', 'col', 'ratio')
  return(out[1:(nrow(out) - 1), ])
}

n <- 12000L
system.time(x <- nfrac(n = n, sieveN = sieve(n)))
# user  system elapsed 
# 7.68    1.35    9.08
sum((x[, 3] > (1 / 3)) & (x[, 3] < (1 / 2)))
# 7295372