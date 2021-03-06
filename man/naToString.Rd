\name{naToString}
\alias{naToString}
\title{Convert NA to string "NA"}
\usage{
  naToString(x)
}
\arguments{
  \item{x}{A data.frame, containing columns of factors}
}
\value{
  The data frame, with the NAs converted to strings "NA"
}
\description{
  When renaming the levels of a factor, it is often helpful
  to allow the renaming of the NAs themselves. This allows
  that, in combination with \code{\link{changeLevels}}.
  This function converts the NAs to a string "NA", which
  then becomes a level of the factor.
}

