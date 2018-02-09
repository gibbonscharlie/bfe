CheckClusterVar <-
function(formula.text, group, controls, fe.other, cluster.var){
  if(!is.null(cluster.var)){
    if(!cluster.var %in% c(group, controls, fe.other)){
    formula.text <- paste(formula.text, "-", cluster.var)
  }}
  return(formula.text)
}
