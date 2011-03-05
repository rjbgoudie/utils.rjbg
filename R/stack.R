#' Most-recently used stack
#'
#' Stack that is aware of which items have been used most recently.
#' 
#' Based upon Hadley Wickham's memoise (MIT License)
#' http://cran.r-project.org/web/packages/memoise/
#' 
#' @param size An integer. The stack size
#' @export
new_stack <- function(size = 1000L)
{
  keys_used <- vector("character", size)
  cache <- NULL
  cache_reset <- function(){
    cache <<- new.env(hash = TRUE, emptyenv(), size = size)
  }
  cache_get <- function(key){
    keys_used <<- keys_used[-match(key, keys_used)]
    keys_used <<- c(key, keys_used)
    get(key, env = cache, inherits = FALSE)
  }
  cache_has_key <- function(key){
    exists(key, env = cache, inherits = FALSE)
  }
  cache_rm <- function(key){
    if (cache_has_key(key)){
      rm(list = key, envir = cache)
    }
  }
  cache_set <- function(key, value){
    last <- keys_used[size]
    if (last != ""){
      cache_rm(last)
    }
    keys_used <<- keys_used[1:(size-1)]
    keys_used <<- c(key, keys_used)
    assign(key, value, env = cache)
  }
  cache_reset()
  list(reset = cache_reset,
       set = cache_set,
       get = cache_get,
       has_key = cache_has_key,
       keys = function() ls(cache),
       keys_used = function() keys_used)
}