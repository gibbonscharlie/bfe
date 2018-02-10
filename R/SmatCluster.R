SmatCluster <- function(h, cluster.as.variable){
  h.by.cluster <- apply(h, 2, function(col){
    tapply(col, cluster.as.variable, sum)
  })
  S <- crossprod(h.by.cluster) / nrow(h)
}
