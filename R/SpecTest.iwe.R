SpecTest.iwe <- function(model){
  reg.fe  <- model$reg.fe
  reg.int <- model$reg.int

  ## All coefficients from both models
  theta <- c(reg.fe$coefficients, reg.int$coefficients)

  ## Get diagonal matrix of N*(X'X)^{-1} for the models (i.e., the gradients)
  ginv.fe  <- GmatInvLM(reg.fe)
  ginv.int <- GmatInvLM(reg.int)
  ginv <- GmatInv(ginv.fe, ginv.int)

  ## Get the moment conditions and bind the columns
  h.fe  <- hmatModel(reg.fe)
  h.int <- hmatModel(reg.int)
  h <- hmat(h.fe, h.int)

  ## Get meat of calculation
  if(!is.null(model$cluster.var)){
    smat <- SmatCluster(h, model$cluster.obs)
  } else if(model$is.robust){
    ### Equivalent to "HC0"
    smat <- SmatRobust(h)
  } else {
    ### CHECK: FILL IN
  }

  v <- crossprod(ginv, smat) %*% ginv / nrow(h)

  r.int <- grepl(paste0("^", model$treatment, "$|^", model$treatment, ":"),
    colnames(h.int))

  ## Identify groups interacted
  r.int.groups <- gsub(paste0(model$treatment, "|:", model$group), "",
    colnames(h.int)[r.int])
  r.int.groups <- r.int.groups[r.int.groups != ""]

  f.weights <- model$f.weights
  f.weights[!names(f.weights) %in% r.int.groups] <- 1

  r.int[r.int] <- f.weights

  r <- c(-grepl(paste0("^", model$treatment, "$"), colnames(h.fe)), r.int)

  v.r <- crossprod(r, v) %*% r

  diff <- sum(r*theta)
  stat <- diff^2 / v.r

  label <- "Specification test for equality between FE and IWE"
  results <- list(diff = diff, stat = as.numeric(stat), df = 1, label = label)
  class(results) <- "chisq.test"

  return(results)
}
