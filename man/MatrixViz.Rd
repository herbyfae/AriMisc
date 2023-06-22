\name{MatrixVis}
\alias{MatrixVis}
\title{Visualization of categorical dependent variable predictions by different factor levels}
\usage{
MatrixVis(prediction = NULL,
          target = NULL,
          model = NULL,
          data = NULL,
          by = NULL,
          detailed = F,
          map = F,
          map.data = NULL,
          breaks = NULL,
          ask = F,
          interactive.map = F)
}
\description{
Given a model, their target and independent variables, it separates a model's observations by their level of a given factor, returning summary statistics for each of their levels. Alternatively one could just input the predictions from the beggining, to get a scuffed version of caret's confusionMatrix.
If the separating factor is spatial you can provide additional data to automatically plot summary statistics.
The ability to separate results by a numeric variable's quantiles is WIP.
}
\arguments{
  \item{prediction}{
Predicted values from a model
}
  \item{target}{
True values of the predicted variable
  }
  \item{model}{
Model file
  }
  \item{data}{
Set of predictor variables
  }
  \item{by}{
Factor for later separation of predictions
  }
  \item{detailed}{
Should summary tables be returned for every level of a factor
  }
  \item{map}{
If by is spatial, would you like to plot the summary statistics
  }
  \item{map.data}{
Required spatial data for plotting
  }
  \item{breaks}{
Where would you like to separate the color gradients(for counts only)
  }
  \item{ask}{
Only for non interactive plots - Should ask be turned on
  }
  \item{interactive.map}{
Should the map be converted to a leaflet(doesn't work with knitting for some reason)
  }
}
