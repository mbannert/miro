\name{zooLikeDateconvert}
\alias{zooLikeDateconvert}
\title{Zoo like Date Conversion}
\usage{
  .zoolike.Date.convert(x, offset = 0, ...)
}
\arguments{
  \item{x}{object of class ts}
}
\description{
  This function is taken from the zoo package. It is
  basically the S3 method as.Date.numeric of the package
  zoo. It is used to turn 2005.75 (3rd quarter of 2005)
  like date formats into dates like 2005-07-01.
}
\author{
  Achim Zeileis, Gabor Grothendieck, Jeffrey A. Ryan, Felix
  Andrews
}

