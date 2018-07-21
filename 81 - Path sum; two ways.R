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
minPath <- function(m, ind = ncol(m)) {
  if (!is.matrix(m) | !is.numeric(m)) {
    stop('\'m\' must be a numeric matrix')
  }
  if (ncol(m) != nrow(m)) {
    stop('\'m\' must be a square matrix')
  }
  n <- ncol(m)
  m[, n] <- rev(cumsum(rev(m[, n])))
  m[n, ] <- rev(cumsum(rev(m[n, ])))
  for (i in (n - 1L):1L) {
    m[i, i] <- m[i, i] + min(m[i + 1L, i], m[i, i + 1L])
    if (i > 1L) {
      for (j in (i - 1L):1L) {
        m[i, j] <- m[i, j] + min(m[i + 1L, j], m[i, j + 1L])
        m[j, i] <- m[j, i] + min(m[j + 1L, i], m[j, i + 1L])
      }
    }
  }
  return(m)
}
system.time(minPath(m)[1, 1])
# 427337
# user  system elapsed 
# 0       0       0 