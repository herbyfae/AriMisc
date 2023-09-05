\name{NumToFact}
\alias{NumToFact}
\title{
Converting numerical columns to factors
}
\description{
In the name really, just converts any given number of numerical columns/vectors to factors(all by the same breaks/quantiles though). Recycled code from breaking up numerical "by"s in aggregate and company. Still supports NAs.
}
\usage{
NumToFact = function(data = NULL,
                     num = NULL,
                     quantiles = NULL,
                     breaks = NULL,
                     return.full = F)
}

\arguments{
  \item{data}{
Any given dataframe, not necessarily all numeric, from which numeric columns can be extracted.
}
  \item{num}{
Role dependant on if data was given.

If it was, this serves as a place to specify which columns to convert(by index or name). Can be NULL, if you want all numerical columns to be converted at once, by the same criteria.

If data isn't given, this serves as a repository for the numerical vectors/columns you want to convert. As so, data is really only relevant if you want to get everything back in place(return.full) with no extra work.
}
  \item{quantiles}{
Quantiles with which to subset the vectors/columns. Defaults to quartiles if nothing else is given. Pay attention to keep them all between 0-1 or 1-100(tecnically 0-100 but why) as any of them being over 1 divides the vector by 100.
}
  \item{breaks}{
Same idea, concrete numbers instead.
}
  \item{return.full}{
Tries to put everything back in place and return the data just with the decided upon numericals turned to factors.
}
}

\examples{

data("iris")

# Stress test

iris$Sepal.Length[c(1,3,34,56,122)] = NA
set.seed(1)
iris$Petal.Width = sample(1:3, 150, replace = T)
iris$Petal.W.Fact = as.factor(iris$Petal.Width)

str(iris)

# Automatic detection of numeric columns + default quartiles
NumToFact(data = iris)

# Indexation via num(only in the presence of data)
# Index
NumToFact(data = iris, num = c(2,4))
# Name
NumToFact(data = iris, num = "Sepal.Length")

# Hardcoded error to not waste your time
NumToFact(num = "Sepal.Length")

# Forcing a factor by indexation
NumToFact(data = iris, num = c(2,6))

# Custom quantiles
NumToFact(num = iris[,c(1,2)], quantiles = c(0.1,0.5,0.9))

# Custom breaks

hist(iris$Petal.Length)
NumToFact(num = iris$Petal.Length, breaks = c(2,5))

# Full return
NumToFact(iris, return.full = T)
