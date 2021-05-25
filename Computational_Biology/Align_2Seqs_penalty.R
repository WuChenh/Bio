# Align two sequences with penalty.
Align_2Seqs_penalty <- function(v, w) {
  v <- Split_each_char(v)
  w <- Split_each_char(w)
  n <- length(v)
  m <- length(w)
  s <- matrix(0, n+1, m+1)
  b <- matrix(NA, n+1, m+1)
  rownames(b) <- c(0, v)
  colnames(b) <- c(0, w)
  rownames(s) <- c(0, v)
  colnames(s) <- c(0, w)
  s[ ,1] <- 0
  s[1, ] <- 0
  for (i in 2:(n+1)) {
    for (j in 2:(m+1)) {
      s.tmp <- rep(NA, 8)
      # If match
      if (v[i-1] == w[j-1]) { s.tmp[1] <- s[i-1, j-1] + 5 }
      else { # Not match.
        # Dismatch:
        s.tmp[8] <- s[i-1, j-1] - 4
        if (i > 2) {
          if (s[i-1,j-1]==s[i-2,j-1]-3) { s.tmp[3] <- s[i-1, j] -2 }
          if (s[i-1,j-1]!=s[i-2,j-1]-3) { s.tmp[6] <- s[i-1, j] -3 }
        }
        if (j > 2) {
          if (s[i-1,j-1]==s[i-1,j-2]-3) { s.tmp[2] <- s[i, j-1] -2 }
          if (s[i-1,j-1]!=s[i-1,j-2]-3) { s.tmp[4] <- s[i, j-1] -3 }
        }
        if(i==2&&j==2) { # When not match and i=2, j=2. So open a gap.
          s.tmp[7] <- 0 - 3
        }
      }
      s[i, j] <- max(na.omit(s.tmp))
    }
  }
  # Draw arrows
  for (i in 2:(n+1)) {
    for (j in 2:(m+1)) {
      # Match
      if (s[i, j] == s[i-1, j-1] + 5) { b[i, j] = "↖" }
      # Dismatch
      if (s[i, j] == s[i-1, j-1] - 4) { b[i, j] = "↖" }
      # Open a gap
      if (s[i, j] == s[i, j-1] - 3) { b[i, j] = "←" }
      if (s[i, j] == s[i-1, j] - 3) { b[i, j] = "↑" }
      # Extend a gap
      if (s[i, j] == s[i, j-1] - 2) { b[i, j] = "←" }
      if (s[i, j] == s[i-1, j] - 2) { b[i, j] = "↑" }
    }
  }
  alignment <- Print_Align_2Seqs(v, w, b[-1,-1], n, m)
  print(noquote(alignment))
  return(list(alignment=alignment, score=s[n+1,m+1], arrow_mx=b, score_mx=s))
}

w <- c('ATCTGATC')
v <- c('TGCATAC')
