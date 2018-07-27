library(gmp)
digitsum <- function(a, b) {
  a <- as.bigz(a)
  b <- as.bigz(b)
  sum(as.numeric(strsplit(as.character(a^b), split = "")[[1]]))
}
x <- expand.grid(1:99L, 1:99L)
system.time(out <- mapply(digitsum, a = x$Var1, b = x$Var2))
# user  system elapsed 
# 0.27    0.00    0.26 
max(out)
# 972