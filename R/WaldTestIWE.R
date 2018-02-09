WaldTestIWE <-
function(model){
  if(!is.null(model$cluster.var)){
    var.mat <- "clustervcv"
  } else if(model$is.robust){
    var.mat <- "robustvcv"
  } else {
    var.mat <- "vcv"
  }

  r.int <- grepl(paste0("^", model$treatment, ":"), names(model$reg.int$coefficients))
  r.mat <- matrix(0, nrow = sum(r.int), ncol = length(r.int))
  r.mat[matrix(c(1:nrow(r.mat), which(r.int)), ncol = 2)] <- 1

  v.r <- r.mat %*% tcrossprod(model$reg.int[[var.mat]], r.mat)

  theta <- model$reg.int$coefficients
  r.theta <- r.mat %*% theta

  stat <- crossprod(solve(v.r, r.theta), r.theta)[1,1]
  results <- c(stat = stat, df = sum(r.int))
  class(results) <- "chisq.test"
  return(results)
}
