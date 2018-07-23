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
  iter0 <- m
  iter0[1L:n, n] <- rev(cumsum(m[n:1L, n]))
  iter0 <- t(apply(iter0, 1L, function(x) cumsum(rev(x))))[, n:1L]
  check <- TRUE
  iter1 <- iter0
  while(check) {
    # Update shortest path for corner cells except origin
    iter1[1L, 1L] <- m[1L, 1L] + min(iter0[1L, 2L], iter0[2L, 1L])
    iter1[n, 1L] <- m[n, 1L] + min(iter0[n, 2L], iter0[n - 1L, 1L])
    iter1[1L, n] <- m[1L, n] + min(iter0[1L, n - 1L], iter0[2L, n])
    # Update shortest path for edge cells
    iter1[1L, 2L:(n - 1L)] <- m[1L, 2L:(n - 1L)] + 
      pmin(
        iter0[1L, 1L:(n - 2L)], 
        iter0[1L, 3L:n], 
        iter0[2L, 2:(n - 1L)]
      )
    iter1[n, 2L:(n - 1L)] <- m[n, 2L:(n - 1L)] + 
      pmin(
        iter0[n, 3L:n], 
        iter0[n, 1L:(n - 2L)], 
        iter0[n - 1L, 2L:(n - 1L)]
      )
    iter1[2L:(n - 1L), n] <- m[2L:(n - 1L), n] + 
      pmin(
        iter0[3L:n, n], 
        iter0[1L:(n - 2L), n], 
        iter0[2L:(n - 1L), (n - 1L)]
      )
    iter1[2L:(n - 1L), 1L] <- m[2L:(n - 1L), 1L] + 
      pmin(
        iter0[3L:n, 1L], 
        iter0[1L:(n - 2L), 1L], 
        iter0[2L:(n - 1L), 2L]
      )
    # Update shortest path for all other cells
    iter1[2:(n - 1L), 2L:(n - 1L)] <- m[2L:(n - 1L), 2L:(n - 1L)] + 
      pmin(
        iter0[1L:(n - 2L), 2L:(n - 1L)], iter0[2L:(n - 1L), 1L:(n - 2L)], 
        iter0[3L:n, 2L:(n - 1L)], iter0[2L:(n - 1L), 3L:n]
      )
    check <- !identical(iter1, iter0)
    iter0 <- iter1
  }
  return(iter1)
}
system.time(minPath(m)[1, 1])
# 425185
# user  system elapsed 
# 0.05    0.00    0.05 

## Appendix - unvectorized solution. Does not scale well
# minPath <- function(m) {
#   if (!is.matrix(m) | !is.numeric(m)) {
#     stop('\'m\' must be a numeric matrix')
#   }
#   if (ncol(m) != nrow(m)) {
#     stop('\'m\' must be a square matrix')
#   }
#   n <- ncol(m)
#   iter0 <- m
#   iter0[1L:n, n] <- rev(cumsum(m[n:1L, n]))
#   iter0 <- t(apply(iter0, 1L, function(x) cumsum(rev(x))))[, n:1L]
#   check <- TRUE
#   iter1 <- iter0
#   while(check) {
#     for (j in 1L:n) {
#       for (i in 1L:n) {
#         # Update shortest path for corner cells
#         if ((i == n) & (j == n)) {
#           # Update shortest path for origin cell
#           iter1[i, j] <- iter0[i, j]
#         } else if ((i == 1L) & (j == n)) {
#           iter1[i, j] <- m[i, j] + min(
#             iter0[i, j - 1L], iter0[i + 1L, j]
#           )
#         } else if ((i == n) & (j == 1L)) {
#           iter1[i, j] <- m[i, j] + min(
#             iter0[i, j + 1L], iter0[i - 1L, j]
#           )
#         } else if ((i == 1L) & (j == 1L)) {
#           iter1[i, j] <- m[i, j] + min(
#             iter0[i, j + 1L], iter0[i + 1L, j]
#           )
#         } # Update shortest path for edge cells 
#         else if (i == 1L) {
#           iter1[i, j] <- m[i, j] + min(
#             iter0[i, j - 1L], iter0[i, j + 1L], iter0[i + 1L, j]
#           )
#         } else if (j == 1L) {
#           iter1[i, j] <- m[i, j] + min(
#             iter0[i - 1L, j], iter0[i, j + 1L], iter0[i + 1L, j]
#           )
#         } else if (i == n) {
#           iter1[i, j] <- m[i, j] + min(
#             iter0[i - 1L, j], iter0[i, j - 1L], iter0[i, j + 1L]
#           )
#         } else if (j == n) {
#           iter1[i, j] <- m[i, j] + min(
#             iter0[i, j - 1L], iter0[i - 1L, j], iter0[i + 1L, j]
#           )
#         } # Update shortest path for all other cells
#         else {
#           iter1[i, j] <- m[i, j] + min(
#             iter0[i - 1L, j], iter0[i + 1L, j], 
#             iter0[i, j - 1L], iter0[i, j + 1L]
#           )
#         }
#       }
#     }
#     check <- !identical(iter1, iter0)
#     iter0 <- iter1
#   }
#   return(iter1)
# }
# system.time(minPath(m)[1, 1])
# 425185
# user  system elapsed
# 4       0       4