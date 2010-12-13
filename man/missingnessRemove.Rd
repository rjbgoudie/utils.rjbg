\name{missingnessRemove}
\alias{missingnessRemove}
\title{Remove missing with summary...}
\usage{missingnessRemove(x, missingValues)}
\description{Remove missing with summary}
\details{Removes the missing values from a data frame of factors, and returns a 
summary of what is removed, by column.

Each column is releveled after the removal, so that 
the factor contains only values that exist after the missing values have 
been removed. Note that this could mean that levels that are NOT missing 
values (ie supplied in \code{missingValues}) could be removed. This 
happens when a level is only present when another variable is missing.}
\value{The supplied data frame with rows that include a missing value
removed.}
\arguments{\item{x}{a data frame}
\item{missingValues}{A character vector, listing the values of the factors
that correspond to missing values.}}
\examples{dat <- esoph[, 1:3]
# done}
