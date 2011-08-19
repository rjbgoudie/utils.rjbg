#' a multiple intersection function
#'
#' ...
#' 
#' @param x ...
#' @param y ...
#' @param ... ...
#' @export
intersection <- function(x, y, ...){
  if (missing(y)){
    unique(unlist(x))
  }
   else {
    if (missing(...)) intersect(x, y)
    else intersect(x, intersection(y, ...))
  }
}