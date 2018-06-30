library(data.table)
perimMax <- function(perimeter) {
  perimeter <- as.integer(perimeter)
  # Using the Pythagorean formula for finding primitive Pythagorean triples
  # and calculating the perimeter in terms of the generating integers, 
  # we can deduce that `ceiling(sqrt(perimeter / 2))` is an upper bound for 
  # the generating integers to create a Pythagorean triple with `perimeter`.
  primitiveTriples <- 1:ceiling(sqrt(perimeter / 2))
  # Create data.table of generating integers
  primitiveTriples <- setDT(expand.grid(primitiveTriples, primitiveTriples))
  setnames(primitiveTriples, c('m', 'n'))
  # Use fact that `m > n` for generating integers. The definitions of `m`
  # and `n` are as descibed in the `makeSides` function below.
  primitiveTriples <- primitiveTriples[m > n]
  # Make perimeter
  primitiveTriples[, p:=2 * m * (m + n)]
  # Filter any generating integer pair that creates a perimeter 
  # greater than `perimeter`.
  primitiveTriples <- primitiveTriples[p < (perimeter + 1)]
  # 12 is the least perimeter for an integral right triangle. Note that 
  # only an even perimeter can be generated in the maximum number of ways.
  # Hence, generate only a vector of even numbers from 12.
  perimeters <- seq(from = 12L, to = perimeter, by = 2L)
  # Function to make the sides of the right triangle, given the 
  # generating pair (m, n).
  makeSides <- function(m, n) {
    l <- list(
      a = m^2 + n^2, 
      b = m^2 - n^2, 
      c = 2 * m * n
    )
    return(l)
  }
  # Create sides as columns to data.table.
  primitiveTriples[, (c('a', 'b', 'c')):= makeSides(m, n)]
  # Function to calculate number of generating pairs that can create 
  # a right triange of perimeter `perimeter`.
  nPerim <- function(perim, primitiveTriples) {
    # Check not only for generating pairs that have `perimeter == p`, 
    # but also for pairs where `perim` is a multiple of `p`.
    # All such sides may not be unique triplets, (a, b, c) of sides, 
    # ignoring the order of the sides. However it suffices to check 
    # for the uniqueness of the hypotenuse.
    return(primitiveTriples[perim%%p == 0, uniqueN(a * perim / p)])
  }
  # Run the function for integers `12:perimeter` and find out 
  # where the maximum occurs.
  x <- mapply(
    FUN = nPerim, 
    perim = perimeters, 
    MoreArgs = list(primitiveTriples = primitiveTriples)
  )
  return(perimeters[which.max(x)])
}
perimMax(1000)