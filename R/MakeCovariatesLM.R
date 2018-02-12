MakeCovariatesLM <- function(group, treatment, controls){
  if(!is.null(controls)){
    controls <- paste("+", paste(controls, collapse = " + "))
  }
  fe <- group
  return(list(controls = controls, fe = fe))
}
