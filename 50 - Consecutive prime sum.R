sieve <- function(n) {
  n <- tryCatch({
    as.integer(n)
  }, warning = function(W) {
    stop('n must be an integer.')
  })
  if (n <= 0L) {
    stop('n must be a positive integer.')
  }
  if (n == 1L) {
    return(integer(0L))
  }
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

primes <- sieve(1e6)
conPrimeSum <- function(
  p, # sorted vector of primes, like the one produced by sieve
  n # the number of consecutive primes to sum
) {
  x <- sapply(
    X = 1:(length(p) - n + 1), 
    FUN = function(x) {
      sum(p[x:(x + n - 1)])
    }
  )
  return(x)
}

getMaxPrime <- function(x, p) {
  primesInX <- intersect(x, p)
  if (length(primesInX) > 0) {
    return(max(primesInX))
  } else {
   return(NULL) 
  }
}

system.time(primeSums <- lapply(
  X = setNames(1:1000, as.character(1:1000)), 
  FUN = conPrimeSum, 
  p = primes
))
maxprimes <- lapply(
  X = primeSums, 
  FUN = getMaxPrime, 
  p = primes
)
do.call(c, maxprimes)
