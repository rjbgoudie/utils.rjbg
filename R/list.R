#' Subset a nested list
#' 
#' Extract a particular element of a each sublist of a list.
#' 
#' @param x a list
#' @param which which element
#' @param size the length of each sublist
#' @param use.names logical, whether to use the names. It is faster not to use names
#' @return a list
#' @export
Rows2 <- function(x, which, size, use.names = F){
  select <- seq.int(
    from = which,
    to = size * length(x),
    by = size
  )
  unlist(x, recursive = F, use.names = use.names)[select]
}