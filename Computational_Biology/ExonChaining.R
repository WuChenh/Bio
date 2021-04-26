# G
G = matrix(c(1,5,5,
             2,3,3,
             4,8,6,
             6,12,10,
             7,17,12,
             9,10,1,
             11,15,7,
             13,14,0,
             16,18,4), nrow=9, byrow=TRUE)

ExonChaining <- function(G) {
  n = nrow(G)
  G_v = sort(as.vector(G[ ,1:2]))
  #G = G[order(G[ ,2], decreasing = FALSE), ]
  s = rep(0, n*2)
  for (i in 1:(n*2)) {
    if (G_v[i] %in% G[ ,2]){
      posi = which(G[ ,2]==G_v[i])
      left  = G[posi,1]
      s[i] = max(s[i-1], s[left]+G[posi,3])
      #if (s[i]==s[left]+G[posi,3]) {print(G[posi,])}
    } else {
      if (i > 1) {
        s[i] = s[i-1]
      }
    }
    print(s[i])
  }
  return(s[n*2])
}

ExonChaining(G)