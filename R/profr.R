#' Sort profr
#'
#' ...
#' 
#' @param pf ...
#' @S3method sort profr
sort.profr <- function(pf){
  res <- data.frame(t = tapply(pf$t, pf$f, sum))
  o <- order(res$t, decreasing = T)
  data.frame(t = res[o, ])
}