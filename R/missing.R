
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
#' @return ...
#' @examples
#' dat <- esoph[, 1:3]
#' # done
#' @export
removeMissingWithSummary <- function(x, missingValues){
  incomplete <- list()
  
  for (col in names(x)){
    incomplete[[col]] <- which(x[, col] %in% missingValues)
  }
  
  sort(unlist(lapply(incomplete, length)), dec = T)
  
  x <- x[-unlist(incomplete), ]
  
  # relevel
  for (i in seq_along(names(x))){
    x[, names(x)[i]] <- factor(x[, names(x)[i]])
  }
  
  ignoreCols <- which(names(v1) %in% c("AID"))
  v1[, -ignoreCols] <- lapply(names(v1[, -ignoreCols]), changeLevels, v1, 
                relevelChanges)
  dat <- v1
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
  
  data.frame(lapply(wave1, function(col) {
    col <- as.character(col)
    col[is.na(col)] <- "NA"
    factor(col)
  }))
}