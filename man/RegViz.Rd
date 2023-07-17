\name{RegVis}
\alias{RegVis}
\title{Visualization of numerical dependent variable predictions by different factor levels}
\usage{
RegVis(model = NULL,
       target = NULL,
       data = NULL,
       by = NULL,
       quantiles = NULL,
       breaks.num = NULL,
       map = F,
       map.data = NULL,
       interactive.map = F,
       ask = T)
}
\description{
Given a model, their target and independent variables, acts as a wrapper of predict to separate their input by a given factor's levels. Returns basic summary statistics for each level. Transplanted the by processing from MatrixViz(no longer supports getting a name for a group of dummies, just use the original column)

If the separating factor is spatial you can provide additional data to automatically plot summary statistics.

}
\arguments{
  \item{model}{
Model file
  }
  \item{target}{
True values of the predicted variable
  }
  \item{data}{
Set of predictor variables
  }
  \item{by}{
Vector establishing separations for the data.
  }
  \item{quantiles}{
Through which quantiles should a numeric by be separated. Defaults to quartiles
  }
  \item{breaks.num}{
Vector of specific breaks for a numeric by.
  }
  \item{map}{
If by is spatial, would you like to plot the summary statistics
  }
  \item{map.data}{
Required spatial data for plotting
  }
  \item{ask}{
Only for non interactive plots - Should ask be turned on
  }
  \item{interactive.map}{
Should the map be converted to a leaflet(doesn't work with knitting for some reason)
  }
}
  \examples{
data("iris")
test = lm(data = iris, Petal.Length ~ Sepal.Length)
summary(test)

# If by is not given just returns a standard summary
RegVis(data = iris, model = test, target = iris$Petal.Length)

# Factor by
RegVis(data = iris, model = test, target = iris$Petal.Length, by = iris$Species)

# Numeric by
 # Quantiles
RegVis(data = iris, model = test, target = iris$Petal.Length,
       by = iris$Sepal.Width)

RegVis(data = iris, model = test, target = iris$Petal.Length,
       by = iris$Sepal.Width, quantiles = c(0.1, 0.25,0.5,0.75,0.9))

 # Specific breaks
hist(iris$Sepal.Width)
RegVis(data = iris, model = test, target = iris$Petal.Length,
       by = iris$Sepal.Width, breaks.num = c(2.5, 3, 3.5))

  }
