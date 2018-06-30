champDigit <- function(x) {
  x <- as.integer(x)
  if (x < 1L | is.na(x)) {
    stop('Please enter positive integers only.')
  } else if (x < 9L) {
    return(x)
  }
  # `nchar(x)` yields the same result as below, but is much slower, relatively
  # Find the number of digits in `x`. Not entirely necessary, but provides an
  # upper bound for the next step.
  logTen <- ceiling(log10(x))
  # Create a vector of turning points. e.g. at `x` = 9L, the number of digits
  # in the integers being concatenated to form the Champernowne's constant
  # changes from 1 to 2; at 189L, the number of digits changes from 2 to 3.
  checkVec <- cumsum(sapply(1:log10, function(z) {z * 9 * (10^(z - 1))}))
  # Find the operating zone of `x`. If, 2889L < `x` <= 38889L, then we are
  # concatenating 4 digit integers. `pos` = 3 in this example.
  pos <- findInterval(x, checkVec)
  # Find the "remainder" after the numbers from 1 digits to pos digits 
  # have been dealt with.
  out <- ((x - checkVec[pos]) / (pos + 1)) + 10^(pos) - 1
  if (out%%1 == 0) {
    # Find last digit of number for integral `out`.
    out <- out%%10
  } else {
    # Find the correct digit for fractional `out`.
    frac <- round((out%%1) * (pos + 1))
    integral <- floor(out) + 1
    out <- as.numeric(strsplit(as.character(integral), '')[[1]][frac])
  }
  return(out)
}
x <- 10^(1:7L)
x <- sapply(x, champDigit)
cumprod(x)
