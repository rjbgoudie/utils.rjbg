#' Convert \code{data.frame} of factors to integers
#' 
#' Converts a \code{data.frame} that consists columns of factors into a 
#' \code{data.frame} consisting of integers. If useLevelNames is FALSE, 
#' numbering starts at 0.
#' 
#' @param x an object of class \code{data.frame}
#' @param useLevelNames logical, indicating whether the labels of the levels 
#' should be be converted to integers. This only makes sense if the levels 
#' are integers stored as characters. e.g. factor(c("3", "2", "3")).
#' @return The data.frame with columns converted to integers
#' @export
#' @examples
#' lets <- as.factor(c("a", "c", "b"))
#' nums <- as.factor(c("1", "2", "3"))
#' vars <- as.factor(c("3", "2", "1"))
#' x <- data.frame(letters = lets, numbers = nums, vars = vars)
#' fdfAsInt(x, useLevelNames = FALSE)
#' \dontrun{fdfAsInt(x)}
#' fdfAsInt(x[, 2:3])
#' # returns a data.frame
fdfAsInt <- function(x, useLevelNames = T){
  stopifnot(
    class(x) == "data.frame",
    all(sapply(x, class) == "factor")
  )
  data.frame(lapply(x, function(col){
    if (useLevelNames){
      as.integer(levels(col))[as.integer(col)]
    }
    else {
      # the 1L needed to ensure that
      # the output is integer not double!
      as.integer(col) - 1L
    }
  }))
}

#' Convert \code{data.frame} of integers to factors
#' 
#' Converts a \code{data.frame} that consists columns of integers into a 
#' \code{data.frame} consisting of factors
#' 
#' @param x an object of class \code{data.frame}
#' @return The data.frame with columns converted to factors
#' @export
intAsFDF <- function(x){
  stopifnot(
    class(x) == "data.frame",
    all(sapply(x, class) %in% c("integer", "numeric"))
  )
  
  data.frame(lapply(x, as.factor))
}