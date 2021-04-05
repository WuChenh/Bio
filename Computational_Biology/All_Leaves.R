AllLeaves <- function(L,k){
  a <- rep(1,L)
  outMatrix <- matrix(1, 1, L)
  repeat{
    #print(a)
    a = NextLeaf(a,k)
    if (all(a==rep(1,L))){
      break
    }
    outMatrix <- rbind(outMatrix,a)
  }
  return(outMatrix)
}
