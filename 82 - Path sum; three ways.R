setwd('D:/ProjectEuler/')
m <- as.matrix(read.csv('p081_matrix.txt', header = FALSE))
testMatrix <- as.matrix(read.table(
  text = '131	673	234	103	18
  201	96	342	965	150
  630	803	746	422	111
  537	699	437	121	956
  805	732	524	37	331', 
  header = FALSE
))
minPath <- function(m) {
  if (!is.matrix(m) | !is.numeric(m)) {
    stop('\'m\' must be a numeric matrix')
  }
  if (ncol(m) != nrow(m)) {
    stop('\'m\' must be a square matrix')
  }
  n <- ncol(m)
  for (j in (n - 1L):1L) {
    currCol <- m[, j]
    rightCol <- m[, j + 1L]
    for (i in 1L:n) {
      if (i == 1L) {
        upper <- integer(0L)
        lower <- cumsum(currCol[2L:n]) + rightCol[2L:n]
      } else if (i == n) {
        upper <- rev(cumsum(currCol[(n - 1L):1L]) + rightCol[(n - 1L):1L])
        lower <- integer(0L)
      } else {
        upper <- rev(cumsum(currCol[(i - 1L):1L]) + rightCol[(i - 1L):1L])
        lower <- cumsum(currCol[(i + 1L):n]) + rightCol[(i + 1L):n]
      }
      minVec <- c(upper, rightCol[i], lower)
      names(minVec) <- NULL
      m[i, j] <- currCol[i] + min(minVec)
    }
  }
  return(m)
}
min(minPath(m)[, 1])
# 260324
# user  system elapsed 
# 0.04    0.00    0.03 