#' Use roxygen to make documentation
#'
#' A variant of Hadlay Wickham's document() function in devtools.
#' 
#' @param pkg A package, see \code{\link[devtools]{as.package}}.
#' @export
document <- function(pkg) 
{
  pkg <- as.package(pkg)
  require(roxygen2)
  roxygenize(package.dir  = pkg$path,
            roxygen.dir   = pkg$path,
            copy.package  = F,
            overwrite     = T,
            unlink.target = T)
}