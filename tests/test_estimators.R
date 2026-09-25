# tests/test_estimators.R -- checks that the hand-coded DerSimonian-Laird
# meta-regression used in RUN_ANALYSIS / 12 / 13 / 14 equals metafor's
# rma(method = "DL", test = "t"), on random data. Run: Rscript tests/test_estimators.R
suppressPackageStartupMessages(library(metafor))
metareg <- function(y, se, X) {                  # body copied from RUN_ANALYSIS.R
  X <- cbind(1, as.matrix(X)); v <- se^2; k <- length(y); p <- ncol(X)
  W <- diag(1/v); XtWX <- t(X) %*% W %*% X
  b <- solve(XtWX, t(X) %*% W %*% y); r <- y - X %*% b
  Qe <- as.numeric(t(r) %*% W %*% r)
  P  <- W - W %*% X %*% solve(XtWX, t(X) %*% W)
  tau2 <- max(0, (Qe - (k - p)) / sum(diag(P)))
  W2 <- diag(1/(v + tau2)); XtW2X <- t(X) %*% W2 %*% X
  b2 <- solve(XtW2X, t(X) %*% W2 %*% y); V <- solve(XtW2X)
  sb <- sqrt(diag(V)); list(b = drop(b2), se = sb, tau2 = tau2,
                            p = 2 * pt(-abs(drop(b2) / sb), k - p))
}
set.seed(7); fails <- 0
for (i in 1:200) {
  k <- sample(8:24, 1); x <- runif(k, 8, 30); z <- rbinom(k, 1, .4)
  se <- runif(k, .02, .08); y <- .3 - .01 * x + rnorm(k, 0, .08) + rnorm(k, 0, se)
  X <- if (i %% 2) cbind(x) else cbind(x, z)
  h <- metareg(y, se, X)
  m <- rma(yi = y, sei = se, mods = X, method = "DL", test = "t")
  d <- max(abs(h$b - drop(m$beta)), abs(h$se - m$se), abs(h$tau2 - m$tau2), abs(h$p - m$pval))
  if (d > 1e-8) { fails <- fails + 1; cat("mismatch", i, d, "\n") }
}
cat(sprintf("hand-coded DL vs metafor DL+t: %d / 200 mismatches\n", fails))
if (fails) quit(status = 1)
