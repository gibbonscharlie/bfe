VarRWEstandard <- function(model, K){
  if(is.null(model$model[["(weights)"]])){
    model$model[["(weights)"]] <- 1
  }
  u <- model$residuals * model$model[["(weights)"]]
  N <- nrow(u)
  CORRECTION <- N/(N - K) # Small sample correction to match HC1 estimator
  var <- CORRECTION * sum(u^2) /
    sum((model$model[["(weights)"]]*model$model[[2]]^2))^2
}
