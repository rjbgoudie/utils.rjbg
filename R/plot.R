#' Crop a pdf using pdfcrop
#'
#' Crops a pdf file using the 'pdfcrop' utility
#'
#' @param x file path of a pdf file
#' @return Nothing, unless there is an error
#' @export
pdfcrop <- function(x){
  available <- nzchar(Sys.which("pdfcrop"))
  if (available){
    x = shQuote(x)
    cmd <- paste('pdfcrop',  x,  x)
    windows <- .Platform$OS.type == "windows"
    os_shell <- if (windows) shell else system
    output <- os_shell(cmd, intern = TRUE)
  }
}
