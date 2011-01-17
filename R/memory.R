#' function title
#' 
#' description
#' 
#' @param f ...
mtrace <- function(f){
  temp_file <- tempfile("gcinfo")
  temp_file_con <- file(temp_file, open = "wt")
  sink(file = temp_file_con, type = "message")
  gcinfo(verbose = TRUE)
  f()
  gcinfo(verbose = FALSE)
  sink(type = "message")
  close(temp_file_con)
  
  temp_file_con <- file(temp_file, open = "r")
  head <- readLines(con = temp_file_con, n = 5)
  # tail <- readLines(con = z, n = -1)
  # tail_len <- length(tail)
  # tail <- tail[(tail_len-5):tail_len]
  # 
  # head <- paste(head, collapse = "\n")
  # tail <- paste(tail, collapse = "\n")
  # cat(paste(head, "===", tail, sep = "\n"))
  #
  result <- parse_gcinfos(readLines(con = temp_file_con))
  close(temp_file_con)
  result
}


#' Parse a gcinfo() output
#' 
#' gcinfo() outputs text of the form
#' 
#' Garbage collection 208 = 99+51+58 (level 1) ... 
#' 20.1 Mbytes of cons cells used (56\%)
#' 10.7 Mbytes of vectors used (63\%)
#' 
#' @param str a gcinfo string
#' @return a numeric vector, with two components of the gcinfo string
parse_gcinfo <- function(str){
  cons <- as.numeric(strsplit(str[2], " ")[[1]][1])
  vecs <- as.numeric(strsplit(str[3], " ")[[1]][1])
  c(cons = cons,
    vecs = vecs)
}

#' function title
#' 
#' description
#' 
#' @param str ...
parse_gcinfos <- function(str){
  reportsSeq <- seq_len(length(str)/3)
  str <- split(str, rep(reportsSeq, each = 3))
  
  result <- sapply(str, parse_gcinfo)
  result <- t(result)
  class(result) <- "gcinfo"
  result
}

#' function title
#' 
#' description
#' 
#' @param x ...
#' #S3method xyplot gcinfo
xyplot.gcinfo <- function(x){
  x <- cbind(t = 1:nrow(x), x)
  x <- as.data.frame(x)
  xyplot(cons + vecs ~ t, data = x, type = "l")
}
