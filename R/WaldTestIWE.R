WaldTestIWE <- function(model){
  stopifnot(class(model) == "iwe")
  ## Create G-1 x K matrix
  r.int <- grepl(paste0("^", model$treatment, ":"), names(model$reg.int$coefficients))
  r.mat <- matrix(0, nrow = sum(r.int), ncol = length(r.int))

  ## Index using matrix of rows and columns that should be 1s
  r.mat[matrix(c(1:nrow(r.mat), which(r.int)), ncol = 2)] <- 1

  ## Calculate variance
  v.r <- r.mat %*% tcrossprod(model$reg.int$vcv, r.mat)

  theta <- model$reg.int$coefficients
  r.theta <- r.mat %*% theta

  label <- "Wald test for treatment effect heterogeneity"
  stat <- crossprod(r.theta, solve(v.r)) %*% r.theta
  results <- list(stat = as.numeric(stat), df = sum(r.int), label = label)
  class(results) <- "chisq.test"
  return(results)
}
