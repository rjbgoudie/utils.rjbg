\name{generateFactorRenamer}
\alias{generateFactorRenamer}
\title{Generate a factor renamer}
\usage{
  generateFactorRenamer(x, newlines = "level", sort = F)
}
\arguments{
  \item{x}{a data frame}

  \item{newlines}{A character variable. Either
  \code{"variable"} or \code{"level"}. In the former case,
  each variable is one a separate line in the result. In
  the latter case, newlines are additionally placed after
  each level.}

  \item{sort}{A logical indicating whether the levels
  should be sorted, according to their numeric order}
}
\value{
  The R code to generate the list is output to the
  terminal.
}
\description{
  Creates a template from which a specification of the
  renaming of the levels of factor variables in a
  data.frame can be.
}
\details{
  Suppose we have a data frame \code{x}, composed of a
  number of factor variables (in columns). These factor
  variables are poorly named. For example, they may just be
  named by by integers. We would like to rename them so
  that their names are more helpful and intuitive.

  To do this in a clear and repeatable manner, we will
  create an R list. Each component of the list is given the
  name of a column of the data.frame \code{x}. Each of
  these components is a character vector, each component of
  which refers to a level of the corresponding factor
  variable in \code{x}. Each of these component of the
  vector is a character, taking the default value of the
  current name of the factor level.

  This function output such a list. The list can then be
  modified to specify the new level names, and the changes
  made using function \code{\link{changeLevels}}.
}
\examples{
dat <- esoph[, 1:3]
generateFactorRenamer(dat)

# we can then change this into the following
levelChanges <- list(
agegp = c(
  "25-34" = "Young",
  "35-44" = "Young",
  "45-54" = "Middle-aged",
  "55-64" = "Middle-aged",
  "65-74" = "Old",
  "75+" = "Old"),
alcgp = c(
  "0-39g/day" = "0-39g/day",
  "40-79" = "40-79",
  "80-119" = "80-119",
  "120+" = "120+"),
tobgp = c(
  "0-9g/day" = "Light",
  "10-19" = "Medium",
  "20-29" = "Heavy",
  "30+" = "Heavy")
)
changeLevels(dat, levelChanges)
# this return the new data frame
}

