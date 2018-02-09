print.rwe <-
function(x, digits = 4, ...){
  results <- data.frame(Estimate = c(x$fe.est, x$swe.est))
  results$"Std error" <- c(sqrt(x$fe.var), sqrt(x$swe.var))
  results$"t stat" <- results$Estimate / results$"Std error"
  results$"p-value" <- 2 * pnorm(abs(results$"t stat"), lower.tail = FALSE)

  rownames(results) <- c("FE", "SWE")

  results <- as.matrix(results)

  cat("\n")
  cat("Regression weighted estimator results: \n\n")
  print(round(results, digits = digits))
  diff.pct <- (x$fe.est - x$swe.est) / x$fe.est * 100
  cat("\n")
  cat("Percent difference:", round(diff.pct, 2), "\n\n")
  cat("Observations:", x$N, "\n")
  cat("Groups:      ", x$M, "\n\n")
}
