print.chisq.test <-
function(x, ...){
  cat("Chi-squared statistic:", round(x["stat"], 3), "\n")
  cat("Degrees of freedom:   ", x["df"], "\n")
  cat("p-value:              ", round(pchisq(x["stat"], x["df"],
    lower.tail = FALSE), 3), "\n\n")
}
