MakeCovariatesLFE <-
function(group, treatment, controls, fe.other){
  if(!is.null(controls)){
    controls <- paste("+", paste(controls, collapse = " + "))
  }
  if(!is.null(fe.other)){
    fe.other <- paste0("G(", fe.other, ")")
    fe <- paste0("G(", group, ") + ", paste(fe.other, collapse = " + "))
  } else {
    fe <- paste0("G(", group, ")")
  }
  return(list(controls = controls, fe = fe))
}
