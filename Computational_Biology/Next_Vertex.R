NextVertex <- function(a,i,k) {
  L <- length(a)
  if (i > L) { return('Error! ') }
  if (i<L) {
    a[i+1] = 1
    return(list(a,i+1))
  }
  else {
    for (j in L:1) {
      if (a[j] < k) {
        a[j] = a[j] + 1
        return(list(a,j))
      }
    }
  }
  return(list(a,0))
}