library(projecteuler) # For dec2bin function

# Function to generate palindromes in base 10, given number of digits.
# Note that if n = 4, it will generate all palindromes between 1000 
# and 9999, not between 1 and 9999 as can be expected.
palindromes <- function(n) {
  if (n == 1L) {
    return(matrix(1:9L, ncol = 1L))
  } else if (n == 2L) {
    return(matrix(rep(1:9L, times = 2L), ncol = 2L))
  } else if (n > 2L & is.integer(n)) {
    p <- palindromes(n - 2L)
    tempfun <- function(m, palindrome = p) {
      return(cbind(m, rbind(0L, palindrome), m))
    }
    out <- do.call(rbind, lapply(1:9L, tempfun))
    colnames(out) <- NULL
    return(out)
  }
}

# Function to convert matrix of digits to number in base 10.
matrix2dec <- function(m) {
  if (!is.matrix(m) | !is.integer(m)) {
    stop('m must be an integer matrix.')
  }
  n <- matrix(10L**((ncol(m) - 1L):0L), ncol = 1L)
  return(m%*%n)
}

# Function to check if a vector is palindromic
is.palindrome <- function(vec) {
  vec <- vec[cumsum(vec) > 0] # To get rid of leading zeroes.
  return(all(vec == rev(vec)))
}

n <- 1:6L
dbpSum <- 0L
system.time(for (i in n) {
  p <- matrix2dec(palindromes(i))
  b <- sapply(p, dec2bin)
  dbp <- sapply(b, is.palindrome)
  dbpSum <- dbpSum + sum(p[dbp])
})
# user  system elapsed 
# 0.06    0.00    0.07 
dbpSum
# 872187L