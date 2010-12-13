#' Use roxygen to make documentation
#'
#' A variant of Hadlay Wickham's document() function in devtools.
#' 
#' @param pkg A package
#' @export
document <- function(pkg) 
{
    pkg <- as.package(pkg)
    require(roxygen)
    if (exists("roxygenise")) {
      roxygenize(pkg$path, pkg$path, F, T, T, use.Rd2 = T)
    }
}