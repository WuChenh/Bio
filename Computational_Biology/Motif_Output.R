MotifOutput <- function(motif.mx) {
  # This function takes into account multiple motifs due to equal maxima. #
  NBase <- c('A','T','G','C')
  bind.tmp <- rep(NA, ncol(motif.mx))
  for (mx.c in 1:ncol(motif.mx)) {
    NBase.tmp <- c(0,0,0,0)
    for (nc in 1:4) {
      NBase.tmp[nc] = length(which(motif.mx[ , mx.c] == NBase[nc]))
    }
    max.position <- which(NBase.tmp == max(NBase.tmp))
    for (n.max in 1:length(max.position)) {
      motif.single <- rep(NA, ncol(motif.mx))
      motif.single[mx.c] <- NBase[max.position[n.max]]
      bind.tmp <- rbind(bind.tmp, motif.single)
    }
  }
  bind.tmp <- bind.tmp[-1, ]
  exist.count <- rep(0,ncol(bind.tmp))
  for (b.c in 1:ncol(bind.tmp)) {
    exist.count[b.c] <- length(which(!is.na(bind.tmp[ , b.c])))
  }
  motif <- matrix(NA, max(exist.count), ncol(motif.mx))
  for (m.c in 1:ncol(motif.mx)) {
    for (ec in 1:exist.count[m.c]) {
      motif[ec, m.c] <- bind.tmp[which(!is.na(bind.tmp[ , m.c]))[ec], m.c]
    }
  }
  return(motif)
}
