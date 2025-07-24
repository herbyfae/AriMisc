\name{RankDiv}
\alias{RankDiv}
\title{
Ordering by size of its elements and cutting any column into a factor.
}
\description{

Converts any given number of columns/vectors to factors(all by the same ranks/quantiles though).
An easier experiment on the bones of NumToFact on how to implement this feature in Aggregate that might be useful in general.

}
\usage{
RankDiv(data = NULL,
        cols = NULL,
        quantiles = NULL,
        ranks = NULL,
        break.names = NULL,
        return.full = F)
}

\arguments{
  \item{data}{
Either a numeric vector or a dataframe from which columns can be extracted.

}
  \item{cols}{
By default all columns are converted at once, by the same criteria.
This serves as a place to specify which columns to convert if necessary(by index or name).
}
  \item{quantiles}{

Quantiles with which to subset the vectors/columns. Defaults to quartiles if nothing else is given.
Given as a scale of 0-1 or 0-100. These are automatically ordered.

Additionally you can set the behaviour when an quantile is crossed.
Say a break is set at 50\%, the biggest groups have 30\% and 25\%, should they stay together("incl", default) or not("excl").

}
  \item{ranks}{
Divides the columns by their k most common elements. Given by a group of numbers along with "top"(default) or "bottom" tags.
So c(10, 50, "top") would divide a column into a level with their top 10 most common elements, from there to the 50th,
and the rest.
}
  \item{break.names}{
Custom names for each new level. Length = length(ranks or quantiles) + 1.
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
RankDiv(data = iris)

# Choosing columns beforehand
# Index
RankDiv(data = iris, cols = c(2,4,5))
# Name
RankDiv(data = iris, cols = "Sepal.Length")

# Forcing a factor
 # Used to be possible, opted to can it for safety, just as.numeric yourself if it's ever relevant
iris$Species = as.numeric(iris$Species)
RankDiv(data = iris, cols = c(2,5))
iris$Species = factor(iris$Species, labels = c("setosa", "versicolor", "virginica"))

# Custom quantiles and names

RankDiv(data = iris[,c(1,2)], quantiles = c(0.05,0.95),
          break.names = c("Outlier", "Normal", "Outlier"))

# Custom names

hist(iris$Petal.Length)
RankDiv(data = iris$Petal.Length, ranks = c(2,5), break.names = c("S", "M", "L"))

# Error if break names length is different from length(ranks or quantiles) + 1

RankDiv(data = iris$Petal.Length, ranks = c(2,5), break.names = c("S", "L"))

# Full return
RankDiv(iris, return.full = T)

RankDiv(iris, return.full = T, cols = c(1,3))

}
