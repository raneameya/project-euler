denom <- c(1L, 2L, 5L, 10, 20L, 50L, 100L, 200L)
target <- 200L

# Given a target and a vector of denominations, fix the 
# number of coins, say n, of a particular denomination, say d, 
# and find the number of ways in which the reduced target of 
# `target - (n * d)` can be made from the vector of denominations
# excluding d. Summing the number of ways over n = 1 to floor(target / d)
# yields the desired answer.

# This solution does not use memoization which would of course be faster.
# See the problem solution in the pdf file for an explanation of this.
numWays <- function(denom, target) {
  if (target == 0L) {
    return(1L)
  } else if (length(denom) == 1L) {
    return(1L)
  } else {
    denomMax <- max(denom)
    nMax <- floor(target / denomMax)
    targets <- target - denomMax * (0:nMax)
    out <- mapply(
      FUN = numWays,
      MoreArgs = list(denom = denom[-which(denom == denomMax)]),
      target = targets
    )
    return(sum(out))
  }
}

numWays(denom, target)


# c++ implementation of memoization. Included for comparison sake.
# See how much faster (~ 11,500 times faster)!
library(Rcpp)
cppFunction('int NWays(NumericVector coinSizes, int target){
  NumericVector ways(target + 1);
  ways[0] = 1;
  for (int i = 0; i < coinSizes.size(); i++) {
    for (int j = coinSizes[i]; j <= target; j++) {
      ways[j] += ways[j - coinSizes[i]];
    }
  }
  return(ways[target]);
}')

NWays(denom, target)

identical(numWays(denom, target), NWays(denom, target))

library(microbenchmark)
microbenchmark(numWays(denom, target), NWays(denom, target))

# Unit: microseconds
#                   expr       min        lq        mean    median         uq        max neval
# numWays(denom, target) 95453.583 98382.223 100403.9116 99197.014 100298.951 133979.307   100
#   NWays(denom, target)     2.276     2.844      6.7333     8.676      9.956     24.748   100