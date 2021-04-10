Score <- function(l,s,DNA){
  t <- nrow(DNA)
  ScoreMatrix <- matrix(0, 4, l)
  BaseMatrix <- matrix(NA, t, l)
  ScoreList <- rep(0,l)
  NBase <- c('A','T','G','C')
  for (r in 1:t) {
    if ((s[r] + l - 1) > ncol(DNA)) {
      return('Error!')
      }
    BaseMatrix[r, ] = DNA[r, s[r]:(s[r]+l-1)]
  }
  for (sc in 1:l) {
    for (sr in 1:4){
      ScoreMatrix[sr,sc] = length(which(BaseMatrix[ ,sc] == NBase[sr]))
    }
  }
  for (i in 1:l) {
    ScoreList[i] = max(ScoreMatrix[ ,i])
  }
  return(sum(ScoreList))
}
