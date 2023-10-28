\name{RegVis}
\alias{RegVis}
\title{Visualization of numerical dependent variable predictions by different factor levels}
\usage{
RegVis(model = NULL,
       target = NULL,
       data = NULL,
       by = NULL,
       quantiles = NULL,
       breaks.num = NULL
       )
}
\description{
Given a model, their target and independent variables, acts as a wrapper of predict to separate their input by a given factor's levels. Returns basic summary statistics for each level. Transplanted the by processing from MatrixViz(no longer supports getting a name for a group of dummies, just use the original column)

\bold{Removed in-house mapping to limit package dependencies}. I recomend using sf::merge(make sure the column and strings all match) + tmap(just calling plot() or using ggplot also works, I just find tmap's functions easier to use for the results)

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
Vector establishing separations for the data. Numeric ones will be automatically divided into quartiles(customizable via "quantiles" and "breaks.num"). Can have NAs, if it's not required for the model's predictions(They'll just form their own group).
  }
  \item{quantiles}{
Through which quantiles should a numeric by be separated. Defaults to quartiles
  }
  \item{breaks.num}{
Vector of specific breaks for a numeric by.
  }
}
  \examples{

data("iris")

iris$Species = as.factor(iris$Species)
iris$Sepal.Width[c(1,10,59,70,120)] = NA

test = lm(data = iris, Petal.Length ~ Sepal.Length)
summary(test)

# If by is not given just returns a standard summary
RegVis(data = iris, model = test, target = iris$Petal.Length)

# Factor by
RegVis(data = iris, model = test, target = iris$Petal.Length, by = iris$Species)

# Numeric by
# Quantiles + by NA support
RegVis(data = iris, model = test, target = iris$Petal.Length,
       by = iris$Sepal.Width)

RegVis(data = iris, model = test, target = iris$Petal.Length,
       by = iris$Sepal.Width,
       quantiles = c(0.1,0.25,0.5,0.75, 0.9))

# Specific breaks(can be out of order)
hist(iris$Sepal.Width)
RegVis(data = iris, model = test, target = iris$Petal.Length,
       by = iris$Sepal.Width, breaks.num = c(2.5, 3.5, 3))

  }
