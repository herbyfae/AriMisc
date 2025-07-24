\name{missinglevel}
\alias{missinglevel}
\title{Checking two factors for non matching levels}
\usage{
missinglevel(factor1, factor2)
}
\description{
Checks any two lists to find concordant and discordant elements.
Can be supplied in any order but the first is treated as a baseline.
}
\arguments{
  \item{factor1}{
  Treated as a baseline. Can be anything that survives as.factor().
  }
  \item{factor2}{
  Comparator, same lack of limitations, way too simple of a function for that.
  }
}
\value{
List of matching along with non-matching levels and their indexes.
}
\examples{

# Basic use case(just numbers to make it easier(no "" everywhere))
missinglevel(c(1:10), c(13:7))

# Warnings(mostly for when you have ridiculous amounts of possible matches)

missinglevel(c("a","b","b","e"), c("c","d","e"))

missinglevel(c("a","b","b","e","g","h"), c("c","d","e","f"))

# A little stop to not show a massive list of nothing
set.seed(1) # Just in case
missinglevel(rnorm(50, 1, 25), rnorm(50, 1, 25))

}
