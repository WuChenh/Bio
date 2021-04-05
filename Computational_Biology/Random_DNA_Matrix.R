Random_DNA_Matrix <- function(row,col){
  DNA <- matrix(0, row, col)
  snp <- c('A','T','G','C')
  for (i in 1:nrow(DNA)){
    for (j in 1:ncol(DNA)){
      DNA[i,j] <- sample(snp, 1, replace = TRUE)
    }
  }
  return(DNA)
}