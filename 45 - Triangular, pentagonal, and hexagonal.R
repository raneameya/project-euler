pentNum <- function(x) {
  x * (3 * x - 1) / 2
}
hexNum <- function(x) {
  x * (2 * x - 1)
}
x <- 1:50000000
x[hexNum(x)%in%pentNum(x)]
# 27693
hexNum(27693)