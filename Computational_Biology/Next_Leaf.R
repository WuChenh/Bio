NextLeaf <- function(a,k){
  L <- length(a)
  for (i in L:1){
    if (a[i] < k){
      a[i] = a[i] + 1
      return(a)
    }
    a[i] = 1
  }
  return(a)
}