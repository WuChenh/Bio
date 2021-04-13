PrintLCS <- function(b, v, n, m, lcs.out) {
  if (n == 0 || m == 0) {
    return(lcs.out)
  }
  #print(b[n,m])
  if (b[n, m] == "↖") {
    lcs.out <- c(v[n], lcs.out)
    PrintLCS(b, v, n-1, m-1, lcs.out)
  } else {
    if (b[n, m] == "↑") {
      PrintLCS(b, v, n-1, m, lcs.out)
    } else {
      PrintLCS(b, v, n, m-1, lcs.out)
    }
  }
}