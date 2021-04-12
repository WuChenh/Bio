# longest common subsequence.
LCS <- function(v, w) {
  n <- length(v)
  m <- length(w)
  s <- matrix(0, n+1, m+1)
  b <- matrix(NA, n+1, m+1)
  rownames(b) <- c(0, v)
  colnames(b) <- c(0, w)
  s[ ,1] <- 0
  s[1, ] <- 0
  for (i in 2:(n+1)) {
    for (j in 2:(m+1)) {
      s.tmp <- 0
      if (v[i-1] == w[j-1]) {
        s.tmp <- s[i-1, j-1] + 1
      }
      s[i, j] <- max(c(s[i-1, j], s[i, j-1], s.tmp))
    }
  }
  for (i in 2:(n+1)) {
    for (j in 2:(m+1)) {
      if (s[i, j] == s[i, j-1]) { b[i, j] = "←" }
      if (s[i, j] == s[i-1, j]) { b[i, j] = "↑" }
      if (s[i, j] == s[i-1, j-1] + 1) { b[i, j] = "↖" }
    }
  }
  b <- b[-1, -1]
  return(list(s[n+1,m+1], b, s))
}