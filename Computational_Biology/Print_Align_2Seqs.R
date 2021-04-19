Print_Align_2Seqs <- function(v, w, b, n, m, out=matrix(NA,2,0,dimnames = list(c("v","w"),c()))) {
  if ((n+m)==0) { return(out) }
  if (min(n, m) == 0) {
    if (n+m > 0) {
      if (n > 0) {
        out <- cbind(c(v[n], "-"), out)
        return(Print_Align_2Seqs(v, w, b, n-1, m, out=out))
      } else {
        out <- cbind(c("-", w[m]), out)
        return(Print_Align_2Seqs(v, w, b, n, m-1, out=out))
      }
    } 
  }
  if (min(n, m) > 0) {
    if (b[n, m] == "↖") {
      out <- cbind(c(v[n], w[m]), out)
      return(Print_Align_2Seqs(v, w, b, n-1, m-1, out))
    } else {
      if (b[n, m] == "↑") {
        out <- cbind(c(v[n], "-"), out)
        return(Print_Align_2Seqs(v, w, b, n-1, m, out))
      } else {
        out <- cbind(c("-", w[m]), out)
        return(Print_Align_2Seqs(v, w, b, n, m-1, out))
      }
    }
  }
}