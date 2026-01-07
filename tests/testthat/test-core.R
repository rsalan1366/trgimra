test_that("micro gate returns a valid SR statistic and a micro selection label", {
  skip_if_not_installed("mgcv")

  dat <- simulate_trgi_data(n = 180, seed = 4)
  fit <- trgi_fit(dat$X, dat$y)

  # SR gating statistic must exist and be well-formed
  expect_true("SR" %in% names(fit))
  expect_true(is.numeric(fit$SR))
  expect_length(fit$SR, 1)
  expect_true(is.finite(fit$SR))

  # Micro selection label (if present) should be a single string
  if ("best_micro" %in% names(fit) && !is.null(fit$best_micro)) {
    expect_true(is.character(fit$best_micro))
    expect_length(fit$best_micro, 1)
    expect_true(nchar(fit$best_micro) >= 1)
  }

  # Predictions should still be valid irrespective of gating outcome
  yhat <- predict(fit, newdata = dat$X)
  expect_true(is.numeric(yhat))
  expect_equal(length(yhat), nrow(dat$X))
  expect_false(anyNA(yhat))
})
