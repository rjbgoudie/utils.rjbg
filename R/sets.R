# a multiple intersection function
#' @export
intersection <- function(x, y, ...){
  if (missing(y)){
    unlist(x)
  }
   else {
    if (missing(...)) intersect(x, y)
    else intersect(x, intersection(y, ...))
  }
}