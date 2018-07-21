setwd('D:/ProjectEuler/')
words <- scan('p042_words.txt', what = '', sep = ',')
# Function to calculate the word sum
wordSum <- function(w) {
  w <- as.data.frame(strsplit(w, '')[[1]])
  colnames(w) <- 'word'
  x <- data.frame(LETTERS, 1:26)
  w <- merge(w, x, by.x = 'word', by.y = 'LETTERS', all.x = TRUE)
  return(sum(w[['X1.26']]))
}
wSums <- sapply(words, wordSum)
sqrt(2 * max(wSums))
triNum <- function(n) {
  return(n * (n + 1) / 2)
}
tNums <- triNum(1:19)
sum(wSums%in%tNums)
# 162