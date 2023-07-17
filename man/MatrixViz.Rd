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
          detailed = F,
          map = F,
          map.data = NULL,
          breaks = NULL,
          ask = F,
          interactive.map = F)
}
\description{
Given a model, their target and independent variables, it separates a model's observations by their level of a given factor, returning summary statistics for each of their levels. Alternatively one could just input the predictions from the beggining, to get a sligtly scuffed version of caret's confusionMatrix.

If the separating factor is spatial you can provide additional data to automatically plot summary statistics.
The ability to separate results by a numeric variable's quantiles has been added.
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
Factor for later separation of predictions. Can be a column name, or a vector. If numeric it will be automatically divided by its quartiles.
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
  \item{map}{
If by is spatial, would you like to plot the summary statistics
  }
  \item{map.data}{
Required spatial data for plotting
  }
  \item{breaks}{
Where would you like to separate the color gradients(for counts only(not many left honestly...))
  }
  \item{ask}{
Only for non interactive plots - Should ask be turned on, aka, shoud plots wait until your input, or should they run all at once.
  }
  \item{interactive.map}{
Should the map be converted to a leaflet(doesn't work with knitting for some reason, only on script or markdown, probably quarto too(untested))
  }
}
  \examples{

library(nnet)
library(AriMisc)

data("iris")
iris$Species = as.factor(iris$Species)

test = multinom(Species ~., data = iris)

summary(test)

# Basic use case(more or less equivalent to caret::confusionMatrix)
MatrixVis(prediction = predict(test, iris), target = iris$Species)

# Separating by quartiles
MatrixVis(model = test, target = iris$Species, data = iris,
          by = iris$Sepal.Length,
          ext.summary = T)

# By costum quantiles
MatrixVis(model = test, target = iris$Species, data = iris,
          by = iris$Sepal.Width, quantiles = c(.1,.25,.5,.75,.9),
          detailed = T)

# By custom breaks
hist(iris$Petal.Length)
MatrixVis(model = test, target = iris$Species, data = iris,
          by = iris$Petal.Length, breaks.num = c(2,4,6))

# By levels of a factor
hist(iris$Petal.Width)
vector = ifelse(iris$Petal.Width < 1,"<1", ">=1")

MatrixVis(model = test, target = iris$Species, data = iris, by = vector)

}
