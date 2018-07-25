library(projecteuler) # For permutation function

substringDiv <- function(m, vec, strlen) {
  if (!is.integer(strlen) | (strlen <= 0L)) {
    stop('\'strlen\' must be a positive integer.')
  }
  nCols <- ncol(m)
  if (length(vec) != (nCols - strlen + 1L)) {
    stop('\'vec\' length must be exactly ncol(m) - strlen + 1L.')
  }
  vec <- as.integer(vec)
  subsetCols <- lapply(0L:(nCols - strlen), `+`, 1:strlen)
  subsetCols <- lapply(subsetCols, function(ind) m[, ind])
  l <- lapply(subsetCols, `%*%`, matrix(10L**((strlen - 1L):0L)))
  substrings <- do.call(cbind, l)
  remainders <- t(apply(substrings, 1L, `%%`, vec))
  divisible <- apply(remainders, 1L, function(x)(all(x == 0)))
  mNums <- m %*% 10L**((nCols - 1L):0L)
  return(list(pandigitals = mNums, divisible = divisible))
}
system.time({
  p <- permuteN(10L) - 1L
  p <- p[p[, 1L] != 0L, ]
  primes <- c(1L, 2L, 3L, 5L, 7L, 11L, 13L, 17L)
  m <- substringDiv(p, primes, 3L)
})
# user  system elapsed 
# 8.49    0.14    8.63 
sum(m$pandigitals[m$divisible, ])
# 16695334890