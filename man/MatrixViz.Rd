\name{MatrixVis}
\alias{MatrixVis}
\title{Visualization of categorical dependent variable predictions by different factor levels}
\usage{
MatrixVis(prediction = NULL,
          target = NULL,
          model = NULL,
          data = NULL,
          by = NULL,
          quantiles = NULL,
          breaks.num = NULL,
          ext.summary = F,
          detailed = F)
}
\description{
Given a model, their target and independent variables, it separates a model's observations by their level of a given factor, returning summary statistics for each of their levels. Alternatively one could just input the predictions from the beggining, to get a sligtly scuffed version of caret's confusionMatrix.

\bold{Removed in-house mapping to limit package dependencies}. I recomend using sf::merge(make sure the column and strings all match) + tmap(just calling plot() or using ggplot also works, I just find tmap's functions easier to use for the results)
}
\arguments{
  \item{prediction}{
Predicted values from a model
}
  \item{target}{
True values of the predicted variable
  }
  \item{model}{
Fitted model
  }
  \item{data}{
Set of predictor variables. If by is given as a name, this must include the column.
  }
  \item{by}{
Factor for later separation of predictions. Can be a column name, or a vector. If numeric it will be automatically divided by its quartiles. Can have NAs, if it's not required for the model's predictions(They'll just form their own group).
  }
  \item{quantiles}{
Divisions for a numeric by(0-1).

These will be translated into breakpoints, so take care to have them be distinct. If two divisions translate to the same value, it will result in an error
  }
  \item{breaks.num}{
Alternative to setting quantiles. Supply the exact values you would like to divide the data by.

Take care to not set them outside the values of the variable, as it will result in an error(yes, all these warnings result from my own blindness).
  }
  \item{ext.summary}{
Intermediate between the basic summary table and just spitting out all the tables for each level of by.
Adds the prevalence of each level of the target and the range of each fit measure.
  }
  \item{detailed}{
Should summary tables be returned for every level of a factor.
  }
}
  \examples{

library(nnet)
library(AriMisc)

data("iris")
iris$Species = as.factor(iris$Species)
iris$Sepal.Width[c(1,10,59,70,120)] = NA

set.seed(101)
test = multinom(Species ~ Sepal.Length + Petal.Length, data = iris)

summary(test)

# Basic use case(more or less equivalent to caret::confusionMatrix)
MatrixVis(prediction = predict(test, iris), target = iris$Species)

# Separating by quartiles
MatrixVis(model = test, target = iris$Species, data = iris,
          by = iris$Sepal.Length,
          ext.summary = T)

# By custom quantiles + by NA support
MatrixVis(model = test, target = iris$Species, data = iris,
          by = iris$Sepal.Width, quantiles = c(.1,.25,.5,.75,.9),
          detailed = T)

# By custom breaks(can be out of order)
hist(iris$Petal.Length)
MatrixVis(model = test, target = iris$Species, data = iris,
          by = iris$Petal.Length, breaks.num = c(2,6,4))

# By levels of a factor
hist(iris$Petal.Width)
vector = ifelse(iris$Petal.Width < 1,"<1", ">=1")

MatrixVis(model = test, target = iris$Species, data = iris, by = vector)

}
