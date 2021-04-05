BruteForceMotifSearch <- function(DNA,l){
  t <- nrow(DNA)
  n <- ncol(DNA)
  bestscore <- 0
  s <- AllLeaves(t,n-l+1)
  for (i in 1:nrow(s)){
    score_st = Score(l,s[i,],DNA)
    if (score_st > bestscore){
      bestscore = score_st
      bestMotif = s[i,]
    }
  }
  print(paste('Best Motif Position:', paste(bestMotif, collapse = ' '), sep = ' '))
  print(paste('Best Score:', bestscore, sep = ' '))
  #return(bestMotif)
  return(MotifMatrixOutput(DNA, bestMotif, l))
}