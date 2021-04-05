BranchAndBoundMedianStringSearch <- function(DNA, l) {
  s <- rep(1, l)
  best.distance <- .Machine$integer.max
  i <- 1
  while (i > 0) {
    if (i < l) {
      prefix <- Num2Nt(s)
      opt.distance <- TotalDistance(prefix, DNA)
      if (opt.distance > best.distance) {
        s.tmp <- s
        s <- Bypass(s, i, 4)[[1]]
        i <- Bypass(s.tmp, i, 4)[[2]]
      } else {
        s.tmp <- s
        s <- NextVertex(s, i, 4)[[1]]
        i <- NextVertex(s.tmp, i, 4)[[2]]
      }
    } else {
      word <- Num2Nt(s)
      total.dist.tmp <- TotalDistance(word, DNA)
      if (total.dist.tmp < best.distance) {
        best.distance <- total.dist.tmp
        best.word <- word
      }
      s.tmp <- s
      s <- NextVertex(s, i, 4)[[1]]
      i <- NextVertex(s.tmp, i, 4)[[2]]
    }
  }
  return(best.word)
}