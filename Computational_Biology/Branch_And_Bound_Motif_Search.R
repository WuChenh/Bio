BranchAndBoundMotifSearch <- function(DNA,l) {
  t <- nrow(DNA)
  n <- ncol(DNA)
  s <- rep(1, t)
  motif.mx <- matrix('Z', t, l)
  best.score <- 0
  i <- 1
  while (i > 0) {
    score.tmp <- Score(l, s[1:i], matrix(DNA[1:i, ], i))
    if (i < t) {
      opt.score <- score.tmp + (t - i) * l
      if (opt.score < best.score) {
        tmp <- Bypass(s, i, n-l+1)
        s <- tmp[[1]]
        i <- tmp[[2]]
      } else {
        tmp <- NextVertex(s, i, n-l+1)
        s <- tmp[[1]]
        i <- tmp[[2]]
      }
    } else {
      if (score.tmp > best.score) {
        best.score <- score.tmp
        best.motif.position <- s
      }
      tmp <- NextVertex(s, i, n-l+1)
      s <- tmp[[1]]
      i <- tmp[[2]]
    }
  }
  best.motif <- MotifOutput(MotifMatrixOutput(DNA, best.motif.position, l))
  print(paste('Best Motif Position:', paste(best.motif.position, collapse = ' '), sep = ' '))
  print(paste('Best Score:', best.score, sep = ' '))
  return(best.motif)
}
