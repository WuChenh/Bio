Num2Nt <- function(n.vec) {
  Nt <- gsub('1', 'A', n.vec)
  Nt <- gsub('2', 'C', Nt)
  Nt <- gsub('3', 'G', Nt)
  Nt <- gsub('4', 'T', Nt)
  return(Nt)
}