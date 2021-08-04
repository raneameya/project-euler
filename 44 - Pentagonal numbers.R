Pn <- function(n) {
  n * (3 * n - 1) / 2
}
pentagons <- Pn(1:1e4)
pentasums <- outer(X = pentagons, Y = pentagons, FUN = '+')
pentadiff <- outer(X = pentagons, Y = pentagons, FUN = '-')
max(pentasums)
pentagons <- Pn(1:1e5)
max(pentagons)
sumIsPenta <- which(pentasums %in% pentagons)
diffIsPenta <- which(pentadiff %in% pentagons)
pentadiff[intersect(sumIsPenta, diffIsPenta)]
