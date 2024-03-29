% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/class_parameter.R
\name{parameter}
\alias{parameter}
\title{Parameter constructor.}
\usage{
parameter(name, init, priorDist = "FlatPrior", priorPar = numeric())
}
\arguments{
\item{name}{Character, name of the parameter.}

\item{init}{Numeric, initial value of the parameter.}

\item{priorDist}{Character, prior distributions.
getCatalogue(printOnly=TRUE) for a list of available distributions.}

\item{priorPar}{Numeric, vector of prior parameters.}
}
\value{
An object of class 'parameter'.
}
\description{
Creates a new instance of a 'parameter' object
}
\examples{
p1 <- parameter(name='par1',init=0)
p2 <- parameter(name='par2',init=5,priorDist='FlatPrior+')
p3 <- parameter(name='par3',init=4,priorDist='Gaussian',priorPar=c(5,2.3))
}
