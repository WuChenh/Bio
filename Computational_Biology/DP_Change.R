DPChange <- function(money, face.value) {
  if (min(face.value)<=0 || money<=0 || money<min(face.value)) { return('Error!') }
  face.value <- sort(face.value)
  d <- length(face.value)
  slt.mx <- matrix(0, money+1, length(face.value))
  
  for (m in 2:(money+1)) {
    slt.mx[m, ] <- .Machine$integer.max
    for (i in 1:d) {
      if ((m-1) >= face.value[i]) {
        if ((sum(slt.mx[m-face.value[i], ])+1) < sum(slt.mx[m, ])) {
          slt.mx[m, ] <- slt.mx[m-face.value[i], ]
          slt.mx[m,i] <- slt.mx[m-face.value[i],i]+1
        }
      }
    }
  }
  print(paste(' Smallest number of coins:  ', sum(slt.mx[m, ]), sep = ' '))
  print(paste(' Available denominations:   ', paste(face.value, collapse = ' '), sep = ' '))
  print(paste(' (One of) The best solution:', paste(slt.mx[m, ], collapse = ' '), sep = ' '))
  return(slt.mx[m, ])
}
#Test Input: money=14, face.value=c(1,5,7,10)