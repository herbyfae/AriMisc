\name{Poisson}
\alias{Poisson}
\title{
Systematic Poisson regressions
}
\description{
Given a dependent variable and a dataset of factors it applies a normal or Quasipoisson regression depending on the results of an AER::dispersiontest(). The results of these are compared with a negative binomial regression.

Factors are nested for visibility, using the default as a reference(1st level against the rest) along with their count, mean and sd for a visual check.
}
\usage{
Poisson(data = NULL, dependent = NULL)
}
\arguments{
  \item{data}{
What factors you want to study. Or the full data, this detects the factors anyways(just factors, not characters).
}
  \item{dependent}{
What is your dependent/response variable. You can give the column, or its index or name if it's included in the data.
}
}

\examples{
data("CO2")
CO2 = as.data.frame(CO2)
Summary(CO2)
str(CO2)
CO2$uptake = floor(CO2$uptake)

# Obviously not poisson distributions, but getting datasets for this is annoying
hist(CO2$uptake)

a = Poisson(data = CO2, dependent = "uptake")
# A dispersion test p value over 0.95 would've called for a quasipoisson too
a
}
