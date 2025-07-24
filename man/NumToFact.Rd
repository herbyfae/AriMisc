\name{NumToFact}
\alias{NumToFact}
\title{
Converting numerical columns to factors.
}
\description{
Converts any given number of numerical columns/vectors to factors(all by the same breaks/quantiles though).
Touched up recycled code from breaking up numerical "by"s in aggregate and company.

}
\usage{
NumToFact(data = NULL,
          cols = NULL,
          quantiles = NULL,
          breaks = NULL,
          break.names = NULL,
          return.full = F)
}

\arguments{
  \item{data}{
Either a numeric vector or a dataframe from which numeric columns can be extracted.

}
  \item{cols}{
By default all numerical columns are converted at once, by the same criteria.
This serves as a place to specify which columns to convert if necessary(by index or name).
}
  \item{quantiles}{
Quantiles with which to subset the vectors/columns. Defaults to quartiles if nothing else is given.
Given as a scale of 0-1 or 0-100. These are automatically ordered.

}
  \item{breaks}{
Same idea, just numbers instead.
}
  \item{break.names}{
Custom names for each new level. Length = length(breaks or quantiles) + 1.
Accepts duplicate names, \bold{but they have to be ordered}.
}
  \item{return.full}{
Tries to put everything back in place and return the data just with the decided upon numericals turned to factors.
}
}

\examples{

data("iris")
iris$Sepal.Length[c(1,3,34,56,122)] = NA
str(iris)

# Automatic detection of numeric columns + default quartiles
NumToFact(data = iris)

# Choosing columns beforehand
# Index
NumToFact(data = iris, cols = c(2,4,5))
# Name
NumToFact(data = iris, cols = "Sepal.Length")

# Forcing a factor
 # Used to be possible, opted to can it for safety, just as.numeric yourself if it's ever relevant
iris$Species = as.numeric(iris$Species)
NumToFact(data = iris, cols = c(2,5))
iris$Species = factor(iris$Species, labels = c("setosa", "versicolor", "virginica"))

# Custom quantiles and names

NumToFact(data = iris[,c(1,2)], quantiles = c(0.05,0.95),
          break.names = c("Outlier", "Normal", "Outlier"))

# Custom breaks

hist(iris$Petal.Length)
NumToFact(data = iris$Petal.Length, breaks = c(2,5), break.names = c("S", "M", "L"))

# Error if break names length is different from length(breaks or quantiles) + 1

NumToFact(data = iris$Petal.Length, breaks = c(2,5), break.names = c("S", "L"))

# Full return
NumToFact(iris, return.full = T)

NumToFact(iris, return.full = T, cols = c(1,3))

}
