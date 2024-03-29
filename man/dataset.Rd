% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/class_dataset.R
\name{dataset}
\alias{dataset}
\title{Dataset constructor.}
\usage{
dataset(
  Y,
  var = data.frame(varName = factor(rep(names(as.data.frame(Y))[1], NROW(Y)))),
  X = NULL,
  iDim = NULL,
  cType = rep(0, NROW(Y)),
  cWidth = rep(0, NROW(Y)),
  name = "STooDs_dataset",
  file = NULL
)
}
\arguments{
\item{Y}{Data frame (numeric, single column), predictand data.}

\item{var}{Data frame (factor, single column), variable in cases
where the predictand is multivariate.}

\item{X}{Data frame (numeric), covariates (predictors) data.}

\item{iDim}{Data frame (integer), dimension indices. Column names should
correspond to (or at least contain) the names of the dimension objects.
For instance, if 2 dimensions objects with names 'time' and 'space' are
used, the column of iDim should be named 'time'/'space', or possibly
something like 'time_index'/'space_index'.}

\item{cType}{Data frame (integer), censoring type for each data.
<0: 'less than' censoring, >0: 'more than' censoring, 0: interval censoring (see cWidth)}

\item{cWidth}{Data frame (numeric), width of the censoring interval for each data.
True value is supposed to be in Y[i]+/- cWidth[i] (=> 0 leads to no censoring)}

\item{name}{Character, name of the dataset.}

\item{file}{Character, file where the dataset is stored (full path). If NULL, datafile
will be stored in the workspace with an automatic filename}
}
\value{
An object of class 'dataset'.
}
\description{
Creates a new instance of a 'dataset' object
}
\examples{
d <- dataset(Y=data.frame(annualDischarge=rlnorm(50)),
             X=data.frame(year=1960+(1:50)),
             iDim=data.frame(time=1:50))
}
