#' Sort profr
#'
#' ...
#' 
#' @param x ...
#' @param decreasing A logical
#' @param ... Further arguments (unused)
#' @S3method sort profr
#' @method sort profr
sort.profr <- function(x, decreasing = T, ...){
  res <- data.frame(t = tapply(x$t, x$f, sum))
  o <- order(res$t, decreasing = decreasing)
  data.frame(t = res[o, ])
}