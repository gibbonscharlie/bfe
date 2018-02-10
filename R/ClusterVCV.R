ClusterVCV <- function(model, obs.groups){
  M <- length(unique(obs.groups))
  N <- length(obs.groups)
  K <- model$rank
  ## Small-sample correction cited by Cameron and Miller and used by Stata
  dfc <- (M/(M-1))*((N-1)/(N-K))
  uj <- apply(estfun(model), 2, function(x){
    tapply(x, obs.groups, sum)
  })
  vcv <- dfc * sandwich(model, meat. = crossprod(uj) / N)
}
