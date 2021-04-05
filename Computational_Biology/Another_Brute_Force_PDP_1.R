AnotherBruteForcePDP <- function(L,n){
  L <- sort(L)
  L1 <- sort(unique(L))
  if (L1[1]<1){return('Input Error!')}
  M <- max(L1)
  L_No_Max <- sort(unique(L1[-which(L1==M)]))
  L_Combinations = t(combn(L_No_Max, n-2))
  L_Combinations = cbind(0, L_Combinations, M)
  for (L_row in 1:nrow(L_Combinations)){
    X <- L_Combinations[L_row,]
    X_Combinations = t(combn(X, 2))
    Delta_X <- c()
    for (X_row in 1:nrow(X_Combinations)){
      X_row_minus = abs(X_Combinations[X_row,1] - X_Combinations[X_row,2])
      Delta_X <- c(Delta_X, X_row_minus)
    }
    Delta_X = sort(Delta_X)
    if (all(L==Delta_X)){
      return(X)
    }
  }
  return('Error!')
}
