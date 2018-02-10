VarRWEcluster <- function(model, cluster.id, K){
  if(is.null(model$model[["(weights)"]])){
    model$model[["(weights)"]] <- 1
  }

  M <- length(unique(cluster.id))
  N <- length(cluster.id)
  ## Small-sample correction cited by Cameron and Miller and used by Stata
  dfc <- (M/(M-1))*((N-1)/(N-K))

  ## estfun returns w^2*x*residual
  s <- estfun(model)
  uj <- by(s, cluster.id, sum)
  var <- sum(uj^2) /
    sum((model$model[["(weights)"]]*model$model[[2]]^2))^2
}
