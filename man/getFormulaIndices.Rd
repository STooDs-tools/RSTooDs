% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/class_model.R
\name{getFormulaIndices}
\alias{getFormulaIndices}
\title{Get formula indices}
\usage{
getFormulaIndices(x)
}
\arguments{
\item{x}{Model}
}
\value{
A list of lenth nVar; the kth element gives the indices of the formulas
   associated with the kth variable
}
\description{
Indices of formulas corresponding to each variable
}
\examples{
# Generate a fake dataset
dat <- dataset(Y=c(rlnorm(50),rpois(40,lambda=1)),var=c(rep('V1',50),rep('V2',40)))
# Define parameters to be estimated: mu, sigma and lambda
param <- list(parameter(name='mu',init=0),parameter(name='sigma',init=1),parameter(name='lambda',init=1))
# Define model
mod <- model(dataset=dat,parentDist=c('LogNormal','Poisson'),varName=c('V1','V2'),
             formula=c('meanlog=mu','sdlog=sigma','lambda=lambda'),par=param)
# Get formula indices
fIndx <- getFormulaIndices(mod)
mod$formula[fIndx[[1]]]
mod$formula[fIndx[[2]]]
}
