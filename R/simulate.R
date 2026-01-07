#' Simulate mixed-type data with global and local nonlinearity
#'
#' @param n Sample size
#' @param seed Random seed
#'
#' @return A list with elements:
#'   \describe{
#'     \item{X}{data.frame of predictors}
#'     \item{y}{numeric response}
#'   }
#'
#' @export
simulate_trgi_data <- function(n = 400, seed = 42) {
  set.seed(seed)

  X1 <- runif(n, -2, 2)
  X2 <- rnorm(n)
  X3 <- runif(n, 0, 1)
  X4 <- rnorm(n)

  C1 <- factor(sample(c("A", "B", "C"), n, replace = TRUE,
                      prob = c(0.45, 0.35, 0.20)))
  C2 <- factor(sample(c("No", "Yes"), n, replace = TRUE,
                      prob = c(0.6, 0.4)))

  f_global <- 2.0 * sin(2 * pi * X1) +
    1.5 * log(1 + X2^2) +
    1.2 * (X3 - 0.5)^2 +
    0.8 * cos(1.5 * X4)

  bump <- ifelse(X1 > 0.8 & X3 < 0.25, 3.0, 0.0) +
    ifelse(X2 < -1 & X4 > 0.5, -2.5, 0.0) +
    ifelse(C1 == "C" & X1 < -1, 2.0, 0.0)

  interaction_jagged <- 2.0 * (X1 * X2) * (X3 > 0.6) +
    1.5 * (abs(X2) > 1.2) * sin(6 * X1)

  cat_shift <- ifelse(C2 == "Yes", 1.2, 0) +
    ifelse(C1 == "B", -0.8, 0)

  y <- f_global + bump + interaction_jagged + cat_shift +
    rnorm(n, sd = 0.8)

  list(
    X = data.frame(X1 = X1, X2 = X2, X3 = X3, X4 = X4, C1 = C1, C2 = C2),
    y = y
  )
}
