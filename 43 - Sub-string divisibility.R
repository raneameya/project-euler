pandigital <- function(n) {
  if (!is.integer(n) | (n <= 0)) {
    stop('\'n\' must be a positive integer.')
  }
  if (n == 1L) {
    return(matrix(1L, ncol = 1L))
  } else {
    p <- pandigital(n - 1L)
    bindfn <- function(i, m) {
      n <- ncol(m)
      if (i == (n + 1L)) {
        return(cbind(m, n + 1L))
      } else if (i == 1L) {
        return(cbind(n + 1L, m))
      } else {
        return(cbind(m[, 1:(i - 1L)], n + 1L, m[, i:n]))
      }
    }
    l <- lapply(n:1, bindfn, p)
    m <- do.call(rbind, l)
    colnames(m) <- NULL
  }
  return(m)
}

substringDiv <- function(m, vec, strlen) {
  if (!is.integer(strlen) | (strlen <= 0L)) {
    stop('\'strlen\' must be a positive integer.')
  }
  nCols <- ncol(m)
  if (length(vec) != (nCols - strlen + 1L)) {
    stop('\'vec\' length must be exactly ncol(m) - strlen + 1L.')
  }
  vec <- as.integer(vec)
  subsetCols <- lapply(0:(nCols - strlen), `+`, 1:strlen)
  subsetCols <- lapply(subsetCols, function(ind) m[, ind])
  l <- lapply(subsetCols, `%*%`, matrix(10L**((strlen - 1L):0L)))
  substrings <- do.call(cbind, l)
  remainders <- t(apply(substrings, 1, `%%`, vec))
  divisible <- apply(remainders, 1, function(x)(all(x == 0)))
  mNums <- m %*% 10L**((nCols - 1L):0L)
  return(list(pandigitals = mNums, divisible = divisible))
}
system.time({
  p <- pandigital(10L) - 1L
  p <- p[p[, 1] != 0L, ]
  primes <- c(1L, 2L, 3L, 5L, 7L, 11L, 13L, 17L)
  m <- substringDiv(p, primes, 3L)
})
# user  system elapsed 
# 8.49    0.14    8.63 
sum(m$pandigitals[m$divisible, ])
# 16695334890


# --------------------------------------------------------------
# Appendix: Old function to compute permutaitons found on SO. 
# Above solution outperforms this solution.
# 
# permutations <- function(n) {
#   if(n == 1) {
#     return(matrix(1))
#   } else {
#     sp <- permutations(n - 1)
#     p <- nrow(sp)
#     A <- matrix(nrow = n * p, ncol = n)
#     for(i in 1:n) {
#       A[(i - 1) * p +  1:p, ] <- cbind(i, sp + (sp >= i))
#     }
#     return(A)
#   }
# }
# n <- 11L
# p1 <- pandigitals(n)
# p2 <- permutations(n)
# library(data.table)
# p1 <- as.data.table(p1)
# p2 <- as.data.table(p2)
# setorderv(p1, names(p1))
# setorderv(p2, names(p2))
# 
# for (i in names(p1)) {
#   print(paste0('Column ', i, ':', all(p1[[i]] == p2[[i]])))
# }
# 
# library(microbenchmark)
# microbenchmark(
#   new = pandigitals(n),
#   old = permutations(n), 
#   times = 5L
# )