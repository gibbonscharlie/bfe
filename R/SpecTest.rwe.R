SpecTest.rwe <- function(model){
  theta <- c(model$reg.fe$coefficients[1], model$reg.w$coefficients[1])

  ## Get diagonal matrix of N*(X'X)^{-1} for the models (i.e., the gradients)
  ginv.fe <- GmatInvRWE(model$reg.fe)
  ginv.w  <- GmatInvRWE(model$reg.w)
  ginv <- GmatInv(ginv.fe, ginv.w)

  h.fe <- hmatModel(model$reg.fe)
  h.w  <- hmatModel(model$reg.w)
  h <- hmat(h.fe, h.w)

  if(!is.null(model$cluster.var)){
    smat <- SmatCluster(h, model$data[[model$cluster.var]])
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

  label <- "Specification test for equality between FE and RWE"
  results <- list(diff = diff, stat = as.numeric(stat), df = 1, label = label)
  class(results) <- "chisq.test"
  return(results)
}
