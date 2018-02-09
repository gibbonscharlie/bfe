VarRWEcluster <-
function(model, cluster.id){
  if(is.null(model$model[["(weights)"]])){
    model$model[["(weights)"]] <- 1
  }
  numerator.by.cluster <- by(estfun(model), cluster.id, sum)
  numerator <- sum(numerator.by.cluster^2)
  denominator <- sum(model$model[["(weights)"]] * model$model[[2]]^2)^2
  var <- numerator / denominator
}
