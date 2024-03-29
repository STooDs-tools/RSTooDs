% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/class_parameter.R
\name{toString.parameter}
\alias{toString.parameter}
\title{Parameter to string}
\usage{
\method{toString}{parameter}(x, ...)
}
\arguments{
\item{x}{Parameter to be converted.}

\item{...}{Optional arguments.}
}
\value{
A string ready to be printed or written.
}
\description{
Convert an object of class 'parameter' into a ready-to-write vector of string
}
\examples{
toString(parameter(name='par1',init=0))
toString(parameter(name='par3',init=4,priorDist='Gaussian',priorPar=c(5,2.3)))
}
