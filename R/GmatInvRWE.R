GmatInvRWE <-
function(model){
  if(is.null(model$model[["(weights)"]])){
    model$model[["(weights)"]] <- 1
  }
  mat <- -1 / mean(model$model[["(weights)"]] * model$model[[2]]^2)
}
