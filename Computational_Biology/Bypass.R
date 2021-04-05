Bypass <- function(a,i,k){
  L <- length(a)
  for (j in i:1){
    if(a[j] < k){
      a[j] = a[j] + 1
      return(list(a,j))
    }
  }
  return(list(a,0))
}