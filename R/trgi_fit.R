#' Fit TRGI-MRA++ model
#'
#' @param X data.frame of predictors (mixed numeric/categorical)
#' @param y numeric response
#' @param cv_folds number of CV folds
#' @param micro_candidates micro learners to consider
#' @param seed random seed
#' @return object of class 'trgi_mra'
#' @export
trgi_fit <- function(
    X, y,
    cv_folds = 5,
    micro_candidates = c("rf", "gam", "xgb"),
    seed = 123
) {
  stopifnot(is.data.frame(X), length(y) == nrow(X))
  X <- .prep_X(X)
  set.seed(seed)

  n <- nrow(X)
  k_nn <- min(max(10, ceiling(log(n) * 10)), n - 1)

  D <- .gower_dist_matrix(X)
  nn <- .knn_from_dist(D, k_nn)

  sigma0 <- .choose_sigma(D, nn, 0.85)
  sigma1 <- .choose_sigma(D, nn, 0.35)

  z <- .topo_smooth(y, D, nn, sigma0)

  fml <- .build_gam_formula(X, "z", k = 8)
  macro <- mgcv::gam(fml, data = data.frame(z = z, X), method = "REML")

  yhat0 <- as.numeric(predict(macro, data.frame(X)))
  r <- y - yhat0
  u <- .orthogonalize(macro, r)

  Tu <- .topo_smooth(u, D, nn, sigma1)
  SR <- sum((u - Tu)^2) / sum(u^2)

  micro <- NULL
  best <- NULL

  if (SR > 0.05) {
    if ("rf" %in% micro_candidates) {
      micro <- ranger::ranger(u ~ ., data = data.frame(u = u, X))
      best <- "rf"
    }
  }

  structure(list(
    macro = macro,
    micro = micro,
    best_micro = best,
    SR = SR
  ), class = "trgi_mra")
}

#' @export
predict.trgi_mra <- function(object, newdata, ...) {
  X <- .prep_X(newdata)
  yhat <- as.numeric(predict(object$macro, data.frame(X)))
  if (!is.null(object$micro)) {
    yhat <- yhat + predict(object$micro, data.frame(X))$predictions
  }
  yhat
}
