CheckSweInputs <-
function(y, treatment, group, controls, fe.other, data, subset,
  cluster.var, is.robust, is.data.returned){
  vars.df <- names(data)
  if(length(y) == 1L & !class(y) == "character" &
     length(treatment) == 1L & !class(treatment) == "character" &
     length(group) == 1L & !class(group) == "character"){
      stop("'y', 'treatment', and 'group' must be variable names")
  }
  if(!(is.null(controls) || class(controls) == "character") &
     !(is.null(fe.other) || class(fe.other) == "character")){
      stop("'controls' and 'fe.other' must be NULL or character vectors")
  }
  if(!group %in% vars.df || class(data[[group]]) != "factor"){
      stop("'group' must be a factor in 'data'")
  }
  if(!is.logical(is.robust) | !is.logical(is.data.returned)){
      stop("'is.logical' and 'is.data.returned' must be TRUE/FALSE")
  }
  if(!gsub(".*\\(([^\\(\\)]+)\\).*", "\\1", y) %in% vars.df){
      stop("'y' must be a variable in 'data'")
  }
  if(!gsub(".*\\(([^\\(\\)]+)\\).*", "\\1", treatment) %in% vars.df){
      stop("'treatment' must be a variable in 'data'")
  }
  if(!is.null(cluster.var) &
     !gsub(".*\\(([^\\(\\)]+)\\).*", "\\1", cluster.var) %in% vars.df){
      stop("'cluster.var' must be a variable in 'data'")
  }
  if(!class(data) == "data.frame"){
      stop("'data' must be a data frame")
  }
}
