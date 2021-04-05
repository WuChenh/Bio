TotalDistance <- function(v, DNA) {
  nr.dna <- nrow(DNA)
  nc.dna <- ncol(DNA)
  len.v <- length(v)
  s <- rep(1, nr.dna)
  d.H.min <- .Machine$integer.max
  i <- 1
  while (i > 0) {
    d.H <- rep(0, nr.dna)
    for (r in 1:i) {
      for (c in 1:len.v) {
        if (v[c] != DNA[r, s[r]+c-1]) {
          d.H[r] <- d.H[r] + 1
        }
      }
    }
    d.H.sum <- sum(d.H)
    if (i < nr.dna) {
      opt.d.H <- d.H.sum + 0
      if (opt.d.H > d.H.min) {
        s.tmp <- s
        s <- Bypass(s, i, nc.dna - len.v + 1)[[1]]
        i <- Bypass(s.tmp, i, nc.dna - len.v + 1)[[2]]
      } else {
        s.tmp <- s
        s <- NextVertex(s, i, nc.dna - len.v + 1)[[1]]
        i <- NextVertex(s.tmp, i, nc.dna - len.v + 1)[[2]]
      }
    } else {
      if (d.H.sum < d.H.min) {
        d.H.min <- d.H.sum
      }
      s.tmp <- s
      s <- NextVertex(s, i, nc.dna - len.v + 1)[[1]]
      i <- NextVertex(s.tmp, i, nc.dna - len.v + 1)[[2]]
    }
  }
  return(d.H.min)
}