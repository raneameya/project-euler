df <- 1:99L
df <- expand.grid(df, df)
# Create a dumb division function as described in the question.
dumbDiv <- function(x, y) {
  out <- NA_real_
  if (nchar(x) == 2) {
    if (nchar(y) == 2) {
      test <- do.call(intersect, strsplit(as.character(c(x, y)), ''))
      if (length(test) == 1L) {
        x <- strsplit(as.character(x), '')[[1]]
        x <- as.numeric(rev(x)[match(test, x)])
        y <- strsplit(as.character(y), '')[[1]]
        y <- as.numeric(rev(y)[match(test, y)])
        out <- x/y
      } else if (length(test) == 2L) {
        out <- 1
      }
    }
  }
  return(out)
}
df$dumbDiv <- mapply(FUN = dumbDiv, x = df$Var1, y = df$Var2)
df$Div <- df$Var1/df$Var2
df <- df[complete.cases(df), ]
# Use the filters to find the cases of interest.
df[
  df$dumbDiv == df$Div 
  & (df$Var1%%10 != 0 | df$Var2%%10 != 0) 
  & (df$Var1 != df$Var2)
  & !(is.na(df$Var1)), 
]
# bottom four rows