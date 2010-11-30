#' @export
logsumexp <- function(a){
  # returns log(sum(exp(a)))
  # see sach email 19 Mar 2009
  
  m <- max(a)
  b <- a - m * rep(1, times = length(a))
  m + log(sum(exp(b)))
}