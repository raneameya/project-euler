library(projecteuler)
system.time({n <- 10L
m <- 1000000L
x <- permuteN(n)
for (i in rev(seq_len(n))) {
  x <- x[order(x[, i]), ]
}})
# user  system elapsed 
# 1.20    0.24    1.43 
x[m, ] - 1L
# [1] 2 7 8 3 9 1 5 4 6 0