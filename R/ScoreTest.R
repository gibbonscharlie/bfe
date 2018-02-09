ScoreTest <-
function(model, data){
  reg.fe <- model$reg.fe
  
  if(!is.null(model$subset)){
    data <- data[eval(model$subset, data), ]
  }

  data <- droplevels(data)
  z.mat <- model.matrix(model$formula.int, data = data)
  s.mat <- reg.fe$residuals * z.mat
  s.mat.means <- colMeans(s.mat)

  if(!is.null(model$cluster.var)){
    h.mat <- SmatCluster(s.mat, data[rownames(s.mat), model$cluster.var])
  } else {
    h.mat <- SmatRobust(s.mat)
  }

  r.int <- grepl(paste0("^", model$treatment, ":"), colnames(h.mat))
  c.mat <- matrix(0, nrow = sum(r.int), ncol = length(r.int))
  c.mat[matrix(c(1:nrow(c.mat), which(r.int)), ncol = 2)] <- 1

  c.hinv.mat <- c.mat %*% solve(h.mat)
  meat <-  crossprod(c.hinv.mat,
    solve(c.mat %*% tcrossprod(solve(h.mat), c.mat), c.hinv.mat))

  stat <- nrow(z.mat) * sum(crossprod(s.mat.means, meat) * s.mat.means)
  results <- c(stat = stat, df = sum(r.int))
  class(results) <- "chisq.test"
  return(results)
}
