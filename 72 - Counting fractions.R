# For any given positive integer `n`, the number of proper reduced 
# fractions includes the number of reduced fractions given `n-1` + 
# additional fractions that depend solely on `n`. If `f(n)` is 
# total number of reduced proper fractions for `n`, then 
# `f(n) = nfrac(n) + f(n - 1)`, where `nfrac(n)` is the contribution 
# by n.


seive <- function(n) {
  a <- c(2:n)
  l <- 2
  r <- c()
  while (l*l < n) {
    r <- c(r,a[1])
    a <- a[-(which(a %% l ==0))]
    l <- a[1]
  }
  c(r,a)
}

nfrac <- function(n, seiveN) {
  cfMatrix <- matrix(data = 1L, ncol = n, nrow = n)
  for (i in seiveN) {
    maxIdx <- floor(n / i)
    cfMatrix[i * (1:maxIdx), i * (1:maxIdx)] <- 0L
  }
  cfMatrix[lower.tri(cfMatrix)] <- NA_integer_
  out <- colSums(cfMatrix, na.rm = TRUE)
  out[1L] <- 0L
  return(out)
}

nfrac <- function(n, seiveN) {
   cfMatrix <- matrix(data = 1L, ncol = n, nrow = n)
   for (i in seiveN) {
     maxIdx <- floor(n / i)
     cfMatrix[i * (1:maxIdx), i * (1:maxIdx)] <- 0L
   }
   cfMatrix[lower.tri(cfMatrix)] <- NA_integer_
   return(colSums(cfMatrix, na.rm = TRUE))
}

n <- 12L
sum(nfrac(n = n, seiveN = seive(n / 2)))
