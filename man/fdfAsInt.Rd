\name{fdfAsInt}
\alias{fdfAsInt}
\title{Convert \code{data.frame} of factors to integers}
\usage{
  fdfAsInt(x, useLevelNames = T)
}
\arguments{
  \item{x}{an object of class \code{data.frame}}

  \item{useLevelNames}{logical, indicating whether the
  labels of the levels should be be converted to integers.
  This only makes sense if the levels are integers stored
  as characters. e.g. factor(c("3", "2", "3")).}
}
\value{
  The data.frame with columns converted to integers
}
\description{
  Converts a \code{data.frame} that consists columns of
  factors into a \code{data.frame} consisting of integers.
  If useLevelNames is FALSE, numbering starts at 0.
}
\examples{
lets <- as.factor(c("a", "c", "b"))
nums <- as.factor(c("1", "2", "3"))
vars <- as.factor(c("3", "2", "1"))
x <- data.frame(letters = lets, numbers = nums, vars = vars)
fdfAsInt(x, useLevelNames = FALSE)
\dontrun{fdfAsInt(x)}
fdfAsInt(x[, 2:3])
# returns a data.frame
}

