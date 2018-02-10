EstimateRWE <- function(y, treatment, group, controls, fe.other = NULL, data, subset = NULL,
  cluster.var = NULL, is.robust = TRUE){
  subset.check <- try(class(subset), silent = TRUE)
  if(class(subset.check) == "try-error"){
    subset <- substitute(subset)
    data <- data[eval(subset, data), ]
  } else {
    subset <- NULL
  }

  ## Check inputs
  CheckSweInputs(y, treatment, group, controls, fe.other, data, subset,
    cluster.var, is.robust)
  data <- droplevels(data)
  
  ## Coerce if necessary
  if(!class(data[[group]]) %in% c("character", "factor")){
    data[[group]] <- as.character(data[[group]])
  }

  covariates <- MakeCovariatesLM(group, treatment, controls, fe.other)

  ## Create annihilator formulas
  formula.annihilate.y <- paste(y,         "~", covariates$fe,
    covariates$controls)
  formula.annihilate.t <- paste(treatment, "~", covariates$fe,
    covariates$controls)
  formula.annihilate.y <- formula(formula.annihilate.y)
  formula.annihilate.t <- formula(formula.annihilate.t)

  ### Use these for subsequent tests
  formula.base <- paste(y, "~", treatment, covariates$controls, "+", covariates$fe)
  formula.int  <- paste(y, "~", treatment, "/", group, covariates$controls, "+",
    covariates$fe)
  formula.base <- formula(formula.base)
  formula.int  <- formula(formula.int)

  if(!is.null(subset)){
    obs.subset <- eval(subset, data) & !is.na(data[[y]]) & ! is.na(data[[treatment]])
  } else {
    obs.subset <- NULL
  }
  ### 'reg.t' is done in 'lm' so that 'group' variable can be grabbed from the
  ### model frame and used in 'by' below
  reg.t.c <- call("lm", formula = formula.annihilate.t, data = data)
  reg.t <- eval(reg.t.c)
  reg.y.c <- call("lm", formula = formula.annihilate.y, data = data)
  reg.y <- eval(reg.y.c)

  N <- nrow(reg.t$model)
  M <- length(unique(data[[group]]))

  ## Check groups in model
  obs.groups <- reg.t$model[[group]]
  if(any(table(obs.groups) < 2)){
    stop("All groups must contain more than one observation for identification")
  }

  ## Calculate weights (lm takes weight defined in paper squared)
  weights <- by(reg.t$residuals, obs.groups, var)
  weights <- 1 / weights[obs.groups]

  ## Base and weighted model coefficients
  reg.fe <- lm(reg.y$residuals ~ reg.t$residuals - 1)
  reg.w  <- lm(reg.y$residuals ~ reg.t$residuals - 1, weights = weights)

  ## Estimates
  fe.est  <- reg.fe$coefficients[1]
  swe.est <- reg.w$coefficients[1]

  if(!is.null(cluster.var)){
    obs.cluster <- reg.t$model[[cluster.var]]
    swe.var <- VarRWEcluster(reg.w,  obs.cluster)
    fe.var  <- VarRWEcluster(reg.fe, obs.cluster)
  } else if(is.robust){
    swe.var <- VarRWErobust(reg.w)
    fe.var  <- VarRWErobust(reg.fe)
  } else {
    swe.var <- 0 ### FILL IN
    fe.var  <- 0 ### FILL IN
  }

  ## Results
  results <- list(y = y, group = group, treatment = treatment,
    controls = controls,  fe.other = fe.other, covariates = covariates,
    cluster.var = cluster.var, subset = subset, is.robust = is.robust, N = N, M = M,
    formula.fe = formula.base, formula.int = formula.int,
    fe.est = fe.est, fe.var = fe.var, swe.est = swe.est, swe.var = swe.var,
    reg.fe = reg.fe, reg.w = reg.w, data = data)
  class(results) <- "rwe"
  return(results)
}
