\name{levelsStringForColName}
\alias{levelsStringForColName}
\title{Generate factor renamer for column...}
\usage{levelsStringForColName(colName, x, newlines="level",
    sort=F)}

\description{
  Generate factor renamer for column
}

\details{
  Generate a factor renamer for an individual column of a
  data.frame
}
\value{A character vector. This is an individual component of the list 
described in \code{\link{generateFactorRenamer}}.}
\arguments{
  \item{colName}{A character vector of length 1, indicating which column 
of \code{x} the string should be created for.}
  \item{x}{a data frame}
  \item{newlines}{A character variable. Either \code{"variable"} or
\code{"level"}. In the former case, each variable is one a separate
line in the result. In the latter case, newlines are additionally
placed after each level.}
  \item{sort}{A logical indicating whether the levels should be sorted, 
according to their numeric order}
}
