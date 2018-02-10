GmatInv <- function(g.mat.fe, g.mat.int){
  mat <- bdiag(g.mat.fe, g.mat.int)
  mat <- as.matrix(mat)
}
