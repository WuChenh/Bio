MotifMatrixOutput <- function(DNA,s,l) {
  t <- nrow(DNA)
  n <- ncol(DNA)
  motif.mx <- matrix(NA, t, l)
  for (dna.r in 1:t) {
    if (s[dna.r]+l-1 > n) { return('Error! ') }
    motif.mx[dna.r, ] <- DNA[dna.r, s[dna.r]:(s[dna.r]+l-1)]
  }
  return(motif.mx)
}