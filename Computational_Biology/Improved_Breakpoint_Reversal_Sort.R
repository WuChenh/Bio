ImprovedBreakpointReversalSort <- function(pi) {
  len.pi <- length(pi)
  if (len.pi < 3) { return('Error! ') }
  pi.sort <- sort(pi)
  for (p.s in 1:len.pi) {
    if (pi.sort[p.s] != p.s) { return('Error! ')}
  }
  b <- Breakpoint(pi)
  if (ncol(b) == 1 && b[2, 1] == -1) {
    print(rev(pi))
    return(rev(pi))
  }
  out.mx <- matrix(NA, 0, len.pi)
  while (ncol(b) > 1) {
    if (min(b[2, ]) < 0) {
      incr.s.max <- 0
      decr.s.min <- len.pi
      for (b.c in 1:ncol(b)) {
        if (b[2, b.c] == -1) {
          if (b.c < ncol(b)) {
            decr.s.tmp <- b[3, b.c] - (b[1, b.c + 1] - b[1, b.c] - 1)
            if (decr.s.min > decr.s.tmp) {
              decr.s.min <- decr.s.tmp
              len.decr.s <- b[1, b.c + 1] - b[1, b.c]
              posi.sta.decr.min <- b[1, b.c]
            }
          } else {
            decr.s.tmp <- b[3, b.c] - (len.pi - b[1, b.c])
            if (decr.s.min > decr.s.tmp) {
              decr.s.min <- decr.s.tmp
              posi.sta.decr.min <- b[1, b.c]
              len.decr.s <- len.pi - b[1, b.c] + 1
            }
          }
        }
      }
      for (b.c in 1:ncol(b)) {
        if (decr.s.min == 1) { break }
        if (b[2, b.c] == 1){
          if (b.c < ncol(b)) {
            incr.s.tmp <- b[3, b.c] + b[1, b.c + 1] - b[1, b.c] - 1
            if (incr.s.max < incr.s.tmp) {
              incr.s.max <- incr.s.tmp
              posi.sta.incr.max <- b[1, b.c]
              len.incr.s <- b[1, b.c + 1] - b[1, b.c]
            }
          } else {
            incr.s.tmp <- b[3, b.c] + len.pi - b[1, b.c]
            if (incr.s.max < incr.s.tmp) {
              incr.s.max <- incr.s.tmp
              posi.sta.incr.max <- b[1, b.c]
              len.incr.s <- len.pi - b[1, b.c] + 1
            }
          }
        }
        if (incr.s.max == decr.s.min - 1) { break }
      }
      if (decr.s.min == 1) {
        rev.sta.posi <- 1
        rev.end.posi <- posi.sta.decr.min + len.decr.s - 1
      } else {
        if (posi.sta.incr.max < posi.sta.decr.min) {
          rev.sta.posi <- posi.sta.incr.max + len.incr.s
          rev.end.posi <- posi.sta.decr.min + len.decr.s - 1
        } else {
          rev.sta.posi <- posi.sta.decr.min + len.decr.s
          rev.end.posi <- posi.sta.incr.max + len.incr.s - 1
        }
      }
    } else {
      rev.sta.posi <- b[1, 2]
      if (2 < ncol(b)) {
        len.2nd.s <- b[1, 3] - b[1, 2]
      } else {
        len.2nd.s <- len.pi - b[1, 2] + 1
      }
      rev.end.posi <- b[1, 2] + len.2nd.s - 1
    }
    pi[rev.sta.posi : rev.end.posi] <- rev(pi[rev.sta.posi : rev.end.posi])
    print(pi)
    out.mx <- rbind(out.mx, pi)
    b <- Breakpoint(pi)
  }
  return(out.mx)
}