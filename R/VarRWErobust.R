VarRWErobust <- function(model){
  if(is.null(model$model[["(weights)"]])){
    model$model[["(weights)"]] <- 1
  }
  ## estfun returns w^2*x*residual
  var <- sum(estfun(model)^2) /
    sum((model$model[["(weights)"]]*model$model[[2]]^2))^2
}
