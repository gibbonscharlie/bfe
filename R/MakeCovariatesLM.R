MakeCovariatesLM <-
function(group, treatment, controls, fe.other){
  if(!is.null(controls)){
    controls <- paste("+", paste(controls, collapse = " + "))
  }
  if(!is.null(fe.other)){
    fe <- paste0(group, " + ", paste(fe.other, collapse = " + "))
  } else {
    fe <- group
  }
  return(list(controls = controls, fe = fe))
}
