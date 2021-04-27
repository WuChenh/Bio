ExonChaining <- function(G) {
  out = c()
  n = nrow(G)
  G_v = sort(as.vector(G[ , 1:2]))
  slt.mx = matrix(0, sum(G[ , 3])+1, n+2)
  plus_mx = matrix(0, 0, n+2)
  s = rep(0, (n*2))
  for (i in 1:(n*2)) {
    if (G_v[i] %in% G[ , 2]){
      r_G = which(G[ , 2] == G_v[i])
      left  = G[r_G, 1]
      right = G[r_G, 2]
      w = G[r_G, 3]
      s[i] = max(s[i-1], (s[left] + w))
      if (s[i] == (s[left] + w)) {
        if (sum(slt.mx[s[i]+1 , 3:(n+2)]) > 0) {
          plus_tmp = slt.mx[(s[left]+1), ]
          plus_tmp[1] = i
          plus_tmp[2] = s[i]
          plus_tmp[r_G+2] = 1
          plus_mx = rbind(plus_mx, plus_tmp)
          tmp_rbind = rbind(plus_mx, slt.mx)
          tmp_rbind = matrix(tmp_rbind[which(tmp_rbind[,2]==s[i]), ], ncol=n+2)
          for (tmp in left:1) {
            if (tmp %in% tmp_rbind[ , 1]) {
              slt.mx[s[i]+1, ] = tmp_rbind[which(tmp_rbind[,1]==tmp), ]
              slt.mx[s[i]+1, 1] = i
              slt.mx[s[i]+1, (r_G+2)] = 1
              break
            }
          }
        } else {
          slt.mx[s[i]+1, ] = slt.mx[s[left]+1, ]
          slt.mx[s[i]+1, 1] = i
          slt.mx[s[i]+1, 2] = s[i]
          slt.mx[s[i]+1, r_G+2] = 1
        }
      }
    } else {
      if (i > 1) {
        s[i] = s[i-1]
      }
    }
    out = c(out, s[i])
  }
  ss = max(out)
  out_slt = matrix(0, 0, 3)
  for (c in 3:(n+2)) {
    if (slt.mx[ss+1, c] == 1) {
      out_slt = rbind(out_slt, G[c-2, ])
    }
  }
  print(out_slt)
  print(paste("Score =", s[n*2]))
  return(out_slt)
}

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
#ExonChaining(G)