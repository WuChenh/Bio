Split_each_char <- function(seq1) {
  len_seq <- nchar(seq1)
  seq1 <- substring(toupper(seq1), 1:len_seq, 1:len_seq)
  return(seq1)
}