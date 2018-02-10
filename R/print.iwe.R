print.iwe <- function(x, digits = 4, ...){
  ## Identify interaction variables
  t.int <- grepl(paste0("^", x$treatment, "$|^", x$treatment, ":"),
    names(x$reg.int$coefficients))

  ## Identify groups interacted
  t.int.groups <- gsub(paste0(x$treatment, "|:", x$group), "",
    names(x$reg.int$coefficients)[t.int])
  t.int.groups <- t.int.groups[t.int.groups != ""]

  if(!is.null(x$cluster.var)){
    var.mat <- "clustervcv"
  } else if(x$is.robust){
    var.mat <- "robustvcv"
  } else {
    var.mat <- "vcv"
  }

  fe.est  <- x$reg.fe$coefficients[x$treatment]
  fe.var  <- x$reg.fe[[var.mat]][x$treatment, x$treatment]

  swe.weights <- x$f.weights
  swe.weights[!names(swe.weights) %in% t.int.groups] <- 1
  stopifnot(length(swe.weights) - 1 == length(t.int.groups))
  swe.est <- sum(swe.weights * x$reg.int$coefficients[t.int])
  swe.var <- crossprod(swe.weights, x$reg.int[[var.mat]][t.int, t.int]) %*%
    swe.weights

  results <- data.frame(Estimate = c(fe.est, swe.est))
  results$"Std error" <- c(sqrt(fe.var), sqrt(swe.var))
  results$"t stat" <- results$Estimate / results$"Std error"
  results$"p-value" <- 2 * pnorm(abs(results$"t stat"), lower.tail = FALSE)

  rownames(results) <- c("FE", "SWE")

  results <- as.matrix(results)

  cat("Interaction weighted estimator results: \n\n")
  print(round(results, digits = digits))
  diff.pct <- (fe.est - swe.est) / fe.est * 100
  cat("\n")
  cat("Percent difference:", round(diff.pct, 2), "\n\n")
  cat("Observations:", x$N, "\n")
  cat("Groups:      ", x$M, "\n\n")
}
