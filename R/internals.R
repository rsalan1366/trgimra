.prep_X <- function(X) {
  for (n in names(X)) {
    if (is.character(X[[n]])) X[[n]] <- factor(X[[n]])
  }
  X
}

.gower_dist_matrix <- function(X) {
  as.matrix(cluster::daisy(X, metric = "gower"))
}

.knn_from_dist <- function(D, k) {
  lapply(seq_len(nrow(D)), function(i) {
    order(D[i, ])[2:(k + 1)]
  })
}

.choose_sigma <- function(D, nn, q) {
  d <- unlist(mapply(function(i, j) D[i, j], seq_along(nn), nn))
  as.numeric(quantile(d, q))
}

.topo_smooth <- function(v, D, nn, sigma) {
  s2 <- sigma^2
  sapply(seq_along(v), function(i) {
    w <- exp(-D[i, nn[[i]]]^2 / s2)
    sum(w * v[nn[[i]]]) / sum(w)
  })
}

.build_gam_formula <- function(X, y, k) {
  terms <- sapply(names(X), function(n) {
    if (is.numeric(X[[n]])) sprintf("s(%s,k=%d)", n, k) else n
  })
  as.formula(paste(y, "~", paste(terms, collapse = "+")))
}

.orthogonalize <- function(gam, r) {
  Xp <- predict(gam, type = "lpmatrix")
  beta <- solve(crossprod(Xp), crossprod(Xp, r))
  r - Xp %*% beta
}
