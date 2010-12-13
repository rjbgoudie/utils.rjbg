#' notdiag
#'
#' ...
#' 
#' @param mx ...
#' @export
notdiag <- function(mx){
  mx[which(upper.tri(mx) + lower.tri(mx) == 1)]
}

#' blockDiag
#'
#' ...
#' 
#' @param nr ...
#' @param nc ...
#' @param sample.size ...
#' @export
blockDiag <- function(nr, nc, sample.size){
  iy <- as.vector(outer(rep(1:nc, each = nr), (0:(sample.size - 
    1)) * nc, "+"))
  ix <- as.vector(outer(rep(1:nr, nc), (0:(sample.size - 1)) * 
    nr, "+"))
  result <- matrix(F, nrow = sample.size * nr, ncol = sample.size * 
    nc)
  result[cbind(ix, iy)] <- T
  result
}