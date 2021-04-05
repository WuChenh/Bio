AnotherBruteForcePDP <- function(L,n){
  L <- sort(L)
  L1 <- sort(unique(L))
  if (L1[1]<1){return('Input Error!')}
  M <- max(L1)
  L_No_Max <- sort(unique(L1[-which(L1==M)]))
  L_Combinations = combn(L_No_Max, n-2)
  for (C_col in 1:ncol(L_Combinations)){
    X <- c(0, L_Combinations[,C_col], M)
    X_Combinations = combn(X, 2)
    Delta_X <- c()
    for (X_col in 1:ncol(X_Combinations)){
      X_col_minus = abs(X_Combinations[1,X_col] - X_Combinations[2,X_col])
      Delta_X <- c(Delta_X, X_col_minus)
    }
    Delta_X = sort(Delta_X)
    if (all(L==Delta_X)){
      return(X)
    }
  }
  return('Error!')
}
