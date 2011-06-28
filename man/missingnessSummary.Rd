\name{missingnessSummary}
\alias{missingnessSummary}
\title{Summary of missingness...}
\usage{missingnessSummary(x, missingValues)}

\description{
  Summary of missingness
}

\details{
  Returns a summary of what is removed through missingness,
  by column.
}
\value{A data frame including information about the missingness.}
\arguments{
  \item{x}{a data frame}
  \item{missingValues}{A character vector, listing the values of the factors
that correspond to missing values.}
}
\examples{dat <- esoph[, 1:3]
# done}
