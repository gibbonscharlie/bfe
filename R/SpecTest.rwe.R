SpecTest.rwe <-
function(model, data){
  data <- droplevels(data)
  covariates <- model$covariates

  theta <- c(model$reg.fe$coefficients[1], model$reg.w$coefficients[1])
  
  ginv.fe <- GmatInvRWE(model$reg.fe)
  ginv.w  <- GmatInvRWE(model$reg.w)
  ginv <- GmatInv(ginv.fe, ginv.w)
  ginv <- as.matrix(ginv)

  h.fe <- hmatModel(model$reg.fe)
  h.w  <- hmatModel(model$reg.w)
  h <- hmat(h.fe, h.w)

  if(!is.null(model$cluster.var)){
    smat <- SmatCluster(h, model$obs.cluster)
  } else if(model$is.robust){
    ### Equivalent to "HC0"
    smat <- SmatRobust(h)
  } else {
    ### FILL IN
  }

  v <- crossprod(ginv, smat) %*% ginv / nrow(h)

  r <- c(-1, 1)

  v.r <- crossprod(r, v) %*% r

  diff <- sum(r*theta)
  stat <- diff^2 / v.r

  results <- c(diff = diff, stat = stat[1,1], df = 1)
  class(results) <- "chisq.test"
  return(results)
}
