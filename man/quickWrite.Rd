% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utilities.R
\name{quickWrite}
\alias{quickWrite}
\title{Write a character vector (typically output of toString() to file)}
\usage{
quickWrite(txt, dir, fname)
}
\arguments{
\item{txt}{Character vector, text to be written}

\item{dir}{Character, directory where file is written. The directory is
created if needed. dir should end with a path seperator (not checked here).}

\item{fname}{Character, file name}
}
\value{
Nothing
}
\description{
Write a character vector (typically output of toString() to file)
}
\keyword{internal}
