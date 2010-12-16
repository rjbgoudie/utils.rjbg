
#' Remove missing with summary
#' 
#' Removes the missing values from a data frame of factors, and returns a 
#' summary of what is removed, by column.
#'
#' Each column is releveled after the removal, so that 
#' the factor contains only values that exist after the missing values have 
#' been removed. Note that this could mean that levels that are NOT missing 
#' values (ie supplied in \code{missingValues}) could be removed. This 
#' happens when a level is only present when another variable is missing.
#' 
#' @param x a data frame
#' @param missingValues A character vector, listing the values of the factors
#'   that correspond to missing values.
#' @return The supplied data frame with rows that include a missing value
#'   removed.
#' @examples
#' dat <- esoph[, 1:3]
#' # done
#' @export
missingnessRemove <- function(x, missingValues){
  colNames <- names(x)
  
  missingRowsByColumn <- unlist(sapply(x, whichMissing, missingValues))
  
  allMissing <- unlist(missingRowsByColumn)
  x <- x[-allMissing, ]
  x <- data.frame(lapply(x, factor))
  x
}

#' Summary of missingness
#' 
#' Returns a summary of what is removed through missingness, by column.
#' 
#' @param x a data frame
#' @param missingValues A character vector, listing the values of the factors
#'   that correspond to missing values.
#' @return A data frame including information about the missingness.
#' @examples
#' dat <- esoph[, 1:3]
#' # done
#' @export
missingnessSummary <- function(x, missingValues){
  missingRowsByColumn <- sapply(x, whichMissing, missingValues)
  numberMissing <- sapply(missingRowsByColumn, length)
  
  data.frame(`Missing`   = numberMissing)
}

whichMissing <- function(x, missingValues){
  which(x %in% missingValues)
}

#' Convert NA to string "NA"
#' 
#' When renaming the levels of a factor, it is often helpful to allow the 
#' renaming of the NAs themselves. This allows that, in combination with 
#' \code{\link{changeLevels}}. This function converts the NAs to a string 
#' "NA", which then becomes a level of the factor.
#' 
#' @param x A data.frame, containing columns of factors
#' @return The data frame, with the NAs converted to strings "NA"
#' @export
naToString <- function(x){
  stopifnot(class(x) == "data.frame",
            all(sapply(x, class) == "factor"))
  
  data.frame(lapply(x, function(col) {
    col <- as.character(col)
    col[is.na(col)] <- "NA"
    factor(col)
  }))
}