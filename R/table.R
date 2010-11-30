#' @export
table2 <- function(data, whichCols){
  # Must start at 0.
  # 
  # Args:
  #   x: A matrix, as described in as.parental.matrix()
  # 
  # Returns:
  #   An object of class 'bn'
  stopifnot(class(data) == "matrix",
            class(whichCols) == "integer" || class(whichCols) == "numeric",
            all(whichCols > 0),
            all(whichCols < ncol(data)),
            all(apply(data, 2, min) == 0))
  
  dims <- sapply(whichCols, function(col){
    length(.Internal(unique(data[, col], FALSE, FALSE)))
  })
  
  pd <- cumprod(c(1, dims))
  sel <- length(dims) + 1
  
  bin <- tcrossprod(pd[-sel], data[, whichCols]) + 1
  
  N <- .C("R_tabulate",
      as.integer(bin),
      as.integer(length(bin)),
      as.integer(pd[sel]),
      ans = integer(pd[sel]),
      PACKAGE = "base")$ans

  array(N, dim = dims)
}