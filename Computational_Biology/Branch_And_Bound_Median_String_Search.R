BranchAndBoundMedianStringSearch <- function(DNA, l) {
  s <- rep(1, l)
  best.distance <- .Machine$integer.max
  i <- 1
  while (i > 0) {
    if (i < l) {
      prefix <- Num2Nt(s)
      opt.distance <- TotalDistance(prefix, DNA)
      if (opt.distance > best.distance) {
        tmp <- Bypass(s, i, 4)
        s <- tmp[[1]]
        i <- tmp[[2]]
      } else {
        tmp <- NextVertex(s, i, 4)
        s <- tmp[[1]]
        i <- tmp[[2]]
      }
    } else {
      word <- Num2Nt(s)
      total.dist.tmp <- TotalDistance(word, DNA)
      if (total.dist.tmp < best.distance) {
        best.distance <- total.dist.tmp
        best.word <- word
      }
      tmp <- NextVertex(s, i, 4)
      s <- tmp[[1]]
      i <- tmp[[2]]
    }
  }
  return(best.word)
}
