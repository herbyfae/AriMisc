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

# Basic use case(just numbers to make it easier(no "" everywhere))
missinglevel(c(1,2,5,2,5,2,3), c(3,4,5,3,4,2,3,5))

# Warnings(mostly for when you have ridiculous amounts of possible matches)

missinglevel(c(1,2,2,5), c(3,4,5))

missinglevel(c(1,2,2,5), c(3,4,5,2))

# A little stop to not show a massive list of nothing
set.seed(1) # Just in case
missinglevel(rnorm(50, 1, 25), rnorm(50, 1, 25))

}
\arguments{
  \item{factor1}{
  First vector(can be anything really, but continuous variables will obviously not give you anything relevant)
  }
  \item{factor2}{
  Comparator, same lack of limitations, way too simple of a function for that
  }
}
