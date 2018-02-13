VarRWEstandard <- function(model, K){
  if(is.null(model$model[["(weights)"]])){
    model$model[["(weights)"]] <- 1
  }
  u <- model$residuals
  N <- length(u)
  CORRECTION <- N/(N - K) # Small sample correction to match HC1 estimator
  var <- CORRECTION * sum(model$model[["(weights)"]]*u^2) /
    sum((model$model[["(weights)"]]*model$model[[2]]^2)) / N
}
