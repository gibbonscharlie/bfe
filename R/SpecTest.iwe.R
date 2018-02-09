SpecTest.iwe <-
function(model, data){
  if(!is.null(model$subset)){
    data <- data[eval(model$subset, data), ]
  }
  data <- droplevels(data)
  
  reg.fe  <- model$reg.fe
  reg.int <- model$reg.int

  theta <- c(reg.fe$coefficients, reg.int$coefficients)
  
  ginv.fe  <- GmatInvLM(reg.fe)
  ginv.int <- GmatInvLM(reg.int)
  ginv <- GmatInv(ginv.fe, ginv.int)
  ginv <- as.matrix(ginv)

  h.fe  <- hmatModel(reg.fe)
  h.int <- hmatModel(reg.int)
  h <- hmat(h.fe, h.int)

  if(!is.null(model$cluster.var)){
    obs.cluster <- data[names(reg.int$residuals), model$cluster.var]
    smat <- SmatCluster(h, obs.cluster)
  } else if(model$is.robust){
    ### Equivalent to "HC0"
    smat <- SmatRobust(h)
  } else {
    ### FILL IN
  }

  v <- crossprod(ginv, smat) %*% ginv / nrow(h)

  f.weights <- model$f.weights
  f.weights[1] <- 1

  r.int <- grepl(paste0("^", model$treatment, "$|^", model$treatment, ":"),
    colnames(h.int))
  r.int[r.int] <- f.weights

  r <- c(-grepl(paste0("^", model$treatment, "$"), colnames(h.fe)), r.int)

  v.r <- crossprod(r, v) %*% r

  diff <- sum(r*theta)
  stat <- diff^2 / v.r

  results <- c(diff = diff, stat = stat[1,1], df = 1)
  class(results) <- "chisq.test"
  
  return(results)
}
