\name{RegViz}
\alias{RegViz}
\title{Visualization of numerical dependent variable predictions by different factor levels}
\usage{
RegViz(model = NULL,
       target = NULL,
       by = NULL,
       data = NULL,
       missing.level = NULL,
       map = F,
       map.data = NULL,
       interactive = F,
       ask = T)
}
\description{
Given a model, their target and independent variables, acts as a wrapper of predict to separate their input by a given factor's levels. Returns basic summary statistics for each level.
If the separating factor is spatial you can provide additional data to automatically plot summary statistics.
Still very much a work in progress.
}
\arguments{
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
Factor for later separation of predictions(for now only named(on a positive note, supports dummy and one hot encoding if given a common name for the columns))
  }
  \item{missing.level}{
If using a dummy by variable, to specify the name of the factor not in the given data
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
  \item{interactive}{
Should the map be converted to a leaflet(doesn't work with knitting for some reason)
  }
}
