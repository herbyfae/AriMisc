\name{Aggregate}
\alias{Aggregate}
\title{
Aggregate with multiple basic summary statistics at once
}
\description{
Given a factor to separate directly or a numeric vector to break up, returns summary measures for each column of each partition of the data
}
\usage{
Aggregate = function(data,
                     by = NULL,
                     quantiles = NULL,
                     breaks.num = NULL,
                     measures = "Core"
)
}

\arguments{
  \item{data}{
Pretty self explanatory. Unless you name by, exclude the vector from here. If it's named, exact matches for the column name will be automatically removed.
}
  \item{by}{
Vector to separate the data with. Factors/character vectors don't require anything else, numeric ones do.
}
  \item{quantiles}{
Quantiles used to separate a numeric by. Defaults to quartiles.

Not to confuse with getting summary quantiles, that's in measures.
}
  \item{breaks.num}{
Same rationale, just giving it more specific numbers
}
  \item{measures}{
What summary statistics should be given(for now only apply to numeric columns).

"core" computes the mean, quartiles, and, when present a tally of the column's NAs;

"mean" and "median" can be given separately;

"iqr" gives you the interquantile range;

"na" forces every initial column to return a tally of their NAs(The rationale stays for character/factor columns);

Quantiles can be specified, just keep it consistent from 0-1 or 0-100(if there's any value over 1, it will divide the list by 100)

With all this said a custom measures string could be, for example: c("core", 0.1, 0.9, "na") giving you the mean, median 10,25,50,75 and 90\% quantiles and forcing a tally of the NAs for every column

By default, character/factor columns compute the mode and 1-3 most common elements of the string(mode as a string, the rest appear as percentages of the name given to the column(most frequent elements determined globally))

}
}

\examples{

data("iris")

summary(iris)

# By a factor
Aggregate(iris, by = iris$Species, measures = "core")

# Naming by and specifying measures
Aggregate(iris, by = "Species", measures = c("median", 5, 25, 75, 95, "iqr"))

# Default numeric by
Aggregate(iris[,-1], by = iris$Sepal.Length)

# Messing with by's quantiles
Aggregate(iris[,-2], by = iris$Sepal.Width, quantiles = c(.35,.65))

# By a specific break + specific case of core + na forcing every column vs only when relevant
hist(iris$Petal.Length)
Aggregate(iris[,-3], by = iris$Petal.Length, breaks.num = c(2), measures = c("core", "na"))
}
