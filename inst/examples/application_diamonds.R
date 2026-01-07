# inst/examples/application_diamonds.R
# End-to-end application test: diamonds price prediction

suppressPackageStartupMessages({
  library(trgimra)
  library(mgcv)
})

has_ranger <- requireNamespace("ranger", quietly = TRUE)
has_ggplot2 <- requireNamespace("ggplot2", quietly = TRUE)

if (!has_ggplot2) stop("Please install ggplot2: install.packages('ggplot2')")

diamonds <- ggplot2::diamonds

# Subsample for speed
set.seed(1)
n <- 12000
diamonds <- diamonds[sample.int(nrow(diamonds), n), ]

y <- diamonds$price
X <- diamonds[, c("carat", "depth", "table", "x", "y", "z", "cut", "color", "clarity")]

# ensure factors
X$cut <- factor(X$cut)
X$color <- factor(X$color)
X$clarity <- factor(X$clarity)

set.seed(2)
idx <- sample.int(nrow(X), size = floor(0.75 * nrow(X)))
Xtr <- X[idx, , drop = FALSE]; ytr <- y[idx]
Xte <- X[-idx, , drop = FALSE]; yte <- y[-idx]

rmse <- function(y, yhat) sqrt(mean((y - yhat)^2))
mae  <- function(y, yhat) mean(abs(y - yhat))

# --- TRGI-MRA ---
fit_trgi <- trgi_fit(Xtr, ytr)
pred_trgi <- predict(fit_trgi, Xte)

# --- GAM baseline ---
fit_gam <- mgcv::gam(
  price ~ s(carat, k = 10) + s(depth, k = 10) + s(table, k = 10) +
    s(x, k = 10) + s(y, k = 10) + s(z, k = 10) + cut + color + clarity,
  data = data.frame(price = ytr, Xtr),
  method = "REML"
)
pred_gam <- predict(fit_gam, newdata = Xte)

# --- RF baseline (optional) ---
if (has_ranger) {
  fit_rf <- ranger::ranger(
    price ~ .,
    data = data.frame(price = ytr, Xtr),
    num.trees = 500,
    respect.unordered.factors = "order"
  )
  pred_rf <- predict(fit_rf, data = Xte)$predictions
} else {
  pred_rf <- rep(mean(ytr), length(yte))
}

results <- data.frame(
  Model = c("TRGI-MRA", "GAM (macro-only)", if (has_ranger) "RF (ranger)" else "RF (mean baseline)"),
  RMSE  = c(rmse(yte, pred_trgi), rmse(yte, pred_gam), rmse(yte, pred_rf)),
  MAE   = c(mae(yte, pred_trgi),  mae(yte, pred_gam),  mae(yte, pred_rf))
)
results$Delta_RMSE_vs_GAM <- results$RMSE - results$RMSE[results$Model == "GAM (macro-only)"]

print(results)

cat("\n--- TRGI-MRA diagnostics ---\n")
cat("best_micro:", if (!is.null(fit_trgi$best_micro)) fit_trgi$best_micro else "NA", "\n")
cat("SR:", if (!is.null(fit_trgi$SR)) fit_trgi$SR else "NA", "\n")

# Diagnostics plots (base R)
plot(yte, pred_trgi,
     xlab = "Observed price", ylab = "Predicted price",
     main = "TRGI-MRA: Observed vs Predicted")
abline(0, 1, lty = 2)

resid_trgi <- yte - pred_trgi
plot(resid_trgi, type = "p",
     xlab = "Test index", ylab = "Residual",
     main = "TRGI-MRA: Test residuals")
abline(h = 0, lty = 2)
