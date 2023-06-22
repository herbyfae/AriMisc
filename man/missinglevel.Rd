\name{missinglevel}
\alias{missinglevel}
\title{Checking two factors for non matching levels}
\usage{
missinglevel(factor1, factor2)
}
\description{
Checks any two lists to find concordant and discordant elements
}
\examples{
missinglevel(c(1,2,5), c(3,4,5))
}
\arguments{
  \item{factor1}{
  First vector(can be anything really, but continuous variables will obviously not give you anything relevant)
  }
  \item{factor2}{
  Comparator, same lack of limitations, way too simple of a function for that
  }
}
