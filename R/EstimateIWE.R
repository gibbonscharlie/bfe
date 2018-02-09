EstimateIWE <-
function(y, treatment, group, controls, fe.other = NULL, data, subset = NULL,
  cluster.var = NULL, is.robust = TRUE, is.data.returned = FALSE){
  subset.check <- try(class(subset), silent = TRUE)
  if(class(subset.check) == "try-error"){
    subset <- substitute(subset)
    data <- data[eval(subset, data), ]
  } else {
    subset <- NULL
  }

  CheckSweInputs(y, treatment, group, controls, fe.other, data, subset,
    cluster.var, is.robust, is.data.returned)
  stopifnot(is.robust)
  data <- droplevels(data)
  
  covariates <- MakeCovariatesLM(group, treatment, controls, fe.other)
  
  formula.base <- paste(y, "~", treatment, covariates$controls, "+", covariates$fe)
  formula.int  <- paste(y, "~", treatment, "/", group, covariates$controls, "+",
    covariates$fe)
  formula.base <- formula(formula.base)
  formula.int  <- formula(formula.int)

  reg.fe.c <- call("lm", formula = formula.base, data = data)
  reg.fe <- eval(reg.fe.c)
  reg.fe$call <- match.call()

  N <- nrow(reg.fe$model)
  M <- length(levels(data[[group]]))

  f.table <- table(data[names(reg.fe$fitted.values), group])
  if(any(f.table < 2)){
    stop("All groups must contain more than one observation for identification")
  }
  f.weights <- prop.table(f.table)

  if(!is.null(cluster.var)){
    reg.fe$clustervcv <- ClusterVCV(reg.fe, data[names(reg.fe$residuals), cluster.var])
  }
  if(is.robust){
    reg.fe$robustvcv <- vcovHC(reg.fe)
  }

  reg.int.c <- call("lm", formula = formula.int, data = data)
  reg.int <- eval(reg.int.c)
  reg.int$call <- match.call()

  if(!is.null(cluster.var)){
    reg.int$clustervcv <- ClusterVCV(reg.int, data[names(reg.int$residuals), cluster.var])
  }
  if(is.robust){
    reg.int$robustvcv <- vcovHC(reg.int)
  }
    
  results <- list(y = y, group = group, treatment = treatment, controls = controls,
    fe.other = fe.other,  cluster.var = cluster.var, subset = subset,
    is.robust = is.robust, N = N, M = M,
    formula.fe = formula.base, formula.int = formula.int, f.weights = f.weights,
    reg.fe = reg.fe, reg.int = reg.int)
  if(is.data.returned){
    results$data <- data
  }
  class(results) <- "iwe"
  return(results)
}
