% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/class_dimension.R
\name{dimension}
\alias{dimension}
\title{Dimension constructor.}
\usage{
dimension(name, coord, d = distance(), z = NULL, file = NULL)
}
\arguments{
\item{name}{Character, name of the dimension.}

\item{coord}{Data frame (numeric), coordinates.}

\item{d}{Distance object.}

\item{z}{Data frame (numeric), covariates.}

\item{file}{Character, file where the dimension data are stored (full path).
If NULL, it will be stored in the workspace with an automatic filename.}
}
\value{
An object of class 'dimension'.
}
\description{
Creates a new instance of a 'dimension' object
}
\examples{
# Space dimension
coord <- data.frame(x=runif(50),y=runif(50))
d <- distance(funk='Euclidean')
space <- dimension(name='space',coord=coord,d=d)
# Time dimension
coord <- data.frame(t=1:50)
d <- distance(funk='Euclidean')
z <- data.frame(year=1960+(1:50))
time <- dimension(name='time',coord=coord,d=d,z=z)
}
