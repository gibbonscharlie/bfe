VarRWErobust <-
function(model){
  if(is.null(model$model[["(weights)"]])){
    model$model[["(weights)"]] <- 1
  }
  numerator <- sum((estfun(model))^2)
  denominator <- sum(model$model[["(weights)"]] * model$model[[2]]^2)^2
  var <- numerator / denominator
}
