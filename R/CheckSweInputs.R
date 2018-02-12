CheckSweInputs <-
function(y, treatment, group, controls, data, subset,
  cluster.var, is.robust){
  if(!class(data) == "data.frame"){
      stop("'data' must be a data frame")
  }
  vars.df <- names(data)
  if(length(y) == 1L & !class(y) == "character" &
     length(treatment) == 1L & !class(treatment) == "character" &
     length(group) == 1L & !class(group) == "character"){
      stop("'y', 'treatment', and 'group' must be variable names")
  }
  if(!(is.null(controls) || class(controls) == "character")){
      stop("'controls' must be NULL or a character vector")
  }
  if(!group %in% vars.df){
      stop("'group' must be a variable in 'data'")
  }
  if(!gsub(".*\\(([^\\(\\)]+)\\).*", "\\1", y) %in% vars.df){
      stop("'y' must be a variable in 'data'")
  }
  if(!gsub(".*\\(([^\\(\\)]+)\\).*", "\\1", treatment) %in% vars.df){
      stop("'treatment' must be a variable in 'data'")
  }
  if(!is.null(cluster.var) && !cluster.var %in% vars.df){
      stop("'cluster.var' must be a variable in 'data'")
  }
  if(!is.logical(is.robust) | length(is.robust) != 1L | !is.robust){
    stop("'is.robust' must be TRUE")
  }
}
