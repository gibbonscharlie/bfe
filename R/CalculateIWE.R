CalculateIWE <- function(model, treatment, group, f.weights){
  ## Identify interaction variables
  t.int <- grepl(paste0("^", treatment, "$|^", treatment, ":"),
    names(model$coefficients))

  ## Identify groups interacted
  t.int.groups <- gsub(paste0(treatment, "|:", group), "",
    names(model$coefficients)[t.int])
  t.int.groups <- t.int.groups[t.int.groups != ""]

  swe.weights <- f.weights
  swe.weights[!names(swe.weights) %in% t.int.groups] <- 1
  stopifnot(length(swe.weights) - 1 == length(t.int.groups))
  swe.est <- sum(swe.weights * model$coefficients[t.int])
  swe.var <- crossprod(swe.weights, model$vcv[t.int, t.int]) %*%
    swe.weights

  return(list(swe.est = swe.est, swe.var = swe.var))
}
