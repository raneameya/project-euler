permutations <- function(n) {
  if(n == 1) {
    return(matrix(1))
  } else {
    sp <- permutations(n - 1)
    p <- nrow(sp)
    A <- matrix(nrow = n * p, ncol = n)
    for(i in 1:n) {
      A[(i - 1) * p +  1:p, ] <- cbind(i, sp + (sp >= i))
    }
    return(A)
  }
}
x <- permutations(9)

partition.check <- function(x, p1, p2) {
  powers_mult1 <- 10**rev(p1 - 1)
  powers_mult2 <- 10**rev(1:length(p2) - 1)
  powers_res <- 10**rev(1:length((1:ncol(x))[-c(p1, p2)]) - 1)
  mult1 <- x[, p1]%*%matrix(powers_mult1, nrow = length(p1))
  mult2 <- x[, p2]%*%matrix(powers_mult2, nrow = length(p2))
  result <- x[, (1:ncol(x))[-c(p1, p2)]]%*%powers_res
  flag <- mult1*mult2==result
  out <- cbind(mult1[flag, ], mult2[flag, ], result[flag, ])
  return(out)
}

p1_1 <- 1:2
p2_1 <- 3:5
out_1 <- partition.check(x, p1_1, p2_1)

p1_2 <- 1:1
p2_2 <- 2:5
out_2 <- partition.check(x, p1_2, p2_2)

p1_3 <- 1:4
p2_3 <- 5:5
out_3 <- partition.check(x, p1_3, p2_3)

colSums(out_1[!duplicated(out_1[, 3]), ])
colSums(out_2[!duplicated(out_2[, 3]), ])