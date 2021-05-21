Additive_Phylogeny <- function(DistMx) {
  n_o <- dim(DistMx)[1]
  if (n_o != dim(DistMx)[2]) { return("Distance Matrix Error.") }
  name_sp <- colnames(DistMx)
  T <- AP_Recursive(DistMx, n_o)
  edge <- T$edge
  distance <- T$distance
  name_edge <- names(edge)
  distance_unique <- matrix(NA, 1, 3, TRUE, list(NULL, c('dist', 'n1', 'n2')))
  # Print unique
  for (i in 1:length(edge)) {
    for (j in 1:length(edge[[i]])) {
      w_ij <- intersect(which(distance[,2]==name_edge[i]),
                        which(distance[,3]==edge[[i]][j]))
      if (length(w_ij) > 0) {
        uniq <- 0
        for (r in 1:nrow(distance_unique)) {
          if (sum(c(name_edge[i], edge[[i]][j]) %in% distance_unique[r, 2:3]) == 2) {
            uniq <- uniq + 1
          }
        }
        if (uniq == 0) {
          print(paste(c(name_edge[i], '<-->', edge[[i]][j], ":", distance[w_ij, 1]), collapse = " "))
          distance_unique <- rbind(distance_unique, distance[w_ij, ])
        }
      }
    }
  }
  return(list(distance=distance_unique[-1, ], edge=edge))
}

Calc_delta <- function(DistMx) {
  n <- dim(DistMx)[1]
  name_sp <- colnames(DistMx)
  delta <- .Machine$integer.max
  ii <- NA
  jj <- NA
  kk <- NA
  for (k in 1:n) {
    for (i in 1:n) {
      if (i == k) { next }
      for (j in 1:n) {
        if (j == k) { next }
        if (j == i) { next }
        delta <- min(delta, abs(DistMx[i, j] + DistMx[j, k] - DistMx[i, k]))
        if (delta == 0 && is.na(ii) && is.na(jj)) {
          ii <- name_sp[i]
          jj <- name_sp[j]
          kk <- name_sp[k]
        }
      }
    }
  }
  return(list(delta=delta%/%2, i=ii, j=jj, k=kk))
}

Del_link_2_nodes <- function(x1, x2, edge, distance, delEdge=TRUE, delDist=TRUE) {
  if (delEdge) {
    e_12 <- which(edge[[x1]] == x2)
    e_21 <- which(edge[[x2]] == x1)
    if (length(e_12) > 0) {
      edge[[x1]] <- edge[[x1]][-e_12] }
    if (length(e_21) > 0) {
      edge[[x2]] <- edge[[x2]][-e_21] }
  }
  if (delDist) {
    w_12 <- intersect(which(distance[,2]==x1), which(distance[,3]==x2))
    w_21 <- intersect(which(distance[,2]==x2), which(distance[,3]==x1))
    if (length(w_12) > 0) { distance <- distance[-w_12, ] }
    if (length(w_21) > 0) { distance <- distance[-w_21, ] }
  }
  return(list(edge=edge, distance=distance))
}

AP_Recursive <- function(DistMx, n_o) {
  n <- dim(DistMx)[1]
  name_sp <- colnames(DistMx)
  # 2x2 ?
  if (n == 2) {
    edge <- list() # Names represent each node, their vectors are linking node(s).
    #distance <- matrix(NA, 0, 3) # Col 1 is distance, others are nodes.
    edge[[1]] <- name_sp[2]
    edge[[2]] <- name_sp[1]
    names(edge) <- name_sp
    distance <- c(DistMx[name_sp[1], name_sp[2]], name_sp[1], name_sp[2])
    distance <- rbind(distance, c(DistMx[name_sp[2], name_sp[1]], name_sp[2], name_sp[1]))
    return(list(edge=edge, distance=distance))
  }
  # Degeneracy ?  <-  delta = 0 ?
  degeneracy <- FALSE
  delta_o <- Calc_delta(DistMx)
  delta <- delta_o$delta
  ii <- delta_o$i
  jj <- delta_o$j
  kk <- delta_o$k
  if (delta == 0) {
    degeneracy <- TRUE
  }
  # If degeneracy is F
  if (!degeneracy) {
    for (i in 1:n) {
      for (j in 1:n) {
        if (j == i) { next }
        DistMx[i, j] <- DistMx[i, j] - delta*2
      }
    }
  }
  if (is.na(ii)) {
    ij <- Calc_delta(DistMx)
    ii <- ij$i
    jj <- ij$j
    kk <- ij$k
  }
  if (!is.na(ii)) {
    x_ij <- DistMx[ii, jj]
    x_jk <- DistMx[jj, kk]
    DistMx <- DistMx[-which(name_sp==jj), -which(name_sp==jj)]
    name_sp <- colnames(DistMx)
  }
  # Recursion
  n_o <- n_o + 1
  T <- AP_Recursive(DistMx, n_o)
  edge <- T$edge
  distance <- T$distance
  # Add new node:
  name_new_node <- paste(jj, n_o, sep = '')
  edge[[name_new_node]] <- c(ii, jj, kk)
  edge[[ii]] <- c(edge[[ii]], name_new_node)
  edge[[kk]] <- c(edge[[kk]], name_new_node)
  distance <- rbind(distance, c(x_ij, ii, name_new_node))
  distance <- rbind(distance, c(x_ij, name_new_node, ii))
  distance <- rbind(distance, c(x_jk, name_new_node, kk))
  distance <- rbind(distance, c(x_jk, kk, name_new_node))
  distance <- rbind(distance, c(0, name_new_node, jj))
  distance <- rbind(distance, c(0, jj, name_new_node))
  # Del i-k direct link
  deleted <- Del_link_2_nodes(ii, kk, edge, distance)
  edge <- deleted[[1]]
  distance <- deleted[[2]]
  # Add link between two new nodes
  name_edge <- names(edge)
  for (e in 1:length(edge)) {
    if (name_edge[e] == name_new_node) { next }
    if (sum(c(ii, kk) %in% edge[[e]]) == 2) {
      r_in <- intersect(which(distance[,2] == name_edge[e]),
                        which(distance[,3] == ii))
      r_nk <- intersect(which(distance[,2] == name_edge[e]),
                        which(distance[,3] == kk))
      if (length(r_in) > 0 && length(r_nk) > 0) {
        if ((as.numeric(distance[r_in, 1]) + as.numeric(distance[r_nk, 1])) == (x_ij + x_jk)) {
          edge[[name_new_node]] <- c(edge[[name_new_node]],
                                     name_edge[e])
          edge[[name_edge[e]]] <- c(edge[[name_edge[e]]],
                                    name_new_node)
          distance <- rbind(distance, c(abs(as.numeric(distance[r_in, 1]) - x_ij) - delta,
                                        name_edge[e], name_new_node))
          distance <- rbind(distance, c(abs(as.numeric(distance[r_nk, 1]) - x_jk) - delta,
                                        name_new_node, name_edge[e]))
          # Del link of i-new_node or new_node-k
          # Is the new node on the left?
          if (distance[r_in, 1] < x_ij) {
            # right, so delete i-new_node
            deleted <- Del_link_2_nodes(ii, name_new_node, edge, distance)
            deleted <- Del_link_2_nodes(name_edge[e], kk,
                                        deleted[[1]], deleted[[2]],
                                        delEdge = TRUE, delDist = FALSE)
            edge <- deleted[[1]]
            distance <- deleted[[2]]
            edge[[name_new_node]] <- c(edge[[name_new_node]], kk)
          } else {
            # left, so delete new_node-k
            deleted <- Del_link_2_nodes(name_new_node, kk, edge, distance)
            deleted <- Del_link_2_nodes(ii, name_edge[e],
                                        deleted[[1]], deleted[[2]],
                                        delEdge = TRUE, delDist = FALSE)
            edge <- deleted[[1]]
            distance <- deleted[[2]]
            edge[[ii]] <- c(edge[[ii]], name_new_node)
          }
        }
      }
    }
  }
  # Add delta
  distance[ , 1] <- as.numeric(distance[ , 1]) + delta
  return(list(edge=edge, distance=distance))
}