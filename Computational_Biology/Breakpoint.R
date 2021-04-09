Breakpoint <- function(pi) {
  len.pi <- length(pi)
  if (len.pi < 3) { return('Error! ') }
  bpt.marker <- matrix(NA, 3, 0)
  # 1st row: bpt posit, 2nd row: incr/decr strip, 3rd row: strips' start num. 
  for (i in 2:len.pi) {
    prior.tmp <- pi[i] - pi[i-1]
    if (prior.tmp == 1) {
      if (i == 2) {
        bpt.marker <- cbind(bpt.marker, c(1, 1, pi[1]))
        prior <- 1
        next
      }
      if (prior == 1) {
        next
      } else {
        prior <- 1
        bpt.marker <- cbind(bpt.marker, c(i-1, 1, pi[i-1]))
        next
      }
    }
    if (prior.tmp == -1) {
      if (i == 2) {
        bpt.marker <- cbind(bpt.marker, c(1, -1, pi[1]))
        prior <- -1
      }
      if (prior == -1) {
        next
      } else {
        bpt.marker <- cbind(bpt.marker, c(i-1, -1, pi[i-1]))
        prior <- -1
      }
    }
    else {
      if (i == 2) {
        bpt.marker <- cbind(bpt.marker, c(1, -1, pi[1]))
        prior <- prior.tmp
        next
      }
      if (abs(prior) == 1) {
        if (i == len.pi) {
          bpt.marker <- cbind(bpt.marker, c(i, -1, pi[i]))
          break
        }
        prior <- prior.tmp
        next
      } else{
      bpt.marker <- cbind(bpt.marker, c(i-1, -1, pi[i-1]))
      }
    }
  }
  if (ncol(bpt.marker) > 1) {
    if (bpt.marker[1, 2] == 2 && bpt.marker[3, 1] == 1) {
      bpt.marker[2 ,1] <- 1
    }
  }
  return(bpt.marker)
}