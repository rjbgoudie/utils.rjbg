\name{parse_gcinfo}
\alias{parse_gcinfo}
\title{Parse a gcinfo() output}
\usage{
  parse_gcinfo(str)
}
\arguments{
  \item{str}{a gcinfo string}
}
\value{
  a numeric vector, with two components of the gcinfo
  string
}
\description{
  gcinfo() outputs text of the form
}
\details{
  Garbage collection 208 = 99+51+58 (level 1) ... 20.1
  Mbytes of cons cells used (56\%) 10.7 Mbytes of vectors
  used (63\%)
}

