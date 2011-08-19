\name{missingnessSummary}
\alias{missingnessSummary}
\title{Summary of missingness}
\usage{
  missingnessSummary(x, missingValues)
}
\arguments{
  \item{x}{a data frame}

  \item{missingValues}{A character vector, listing the
  values of the factors that correspond to missing values.}
}
\value{
  A data frame including information about the missingness.
}
\description{
  Returns a summary of what is removed through missingness,
  by column.
}
\examples{
dat <- esoph[, 1:3]
# done
}

