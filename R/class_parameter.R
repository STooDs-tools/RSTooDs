#***************************************************************************----
# Constructor ----
#' Parameter constructor.
#'
#' Creates a new instance of a 'parameter' object
#'
#' @param name Character, name of the parameter.
#' @param init Numeric, initial value of the parameter.
#' @param priorDist Character, prior distributions.
#'   getCatalogue(printOnly=TRUE) for a list of available distributions.
#' @param priorPar Numeric, vector of prior parameters.
#' @return An object of class 'parameter'.
#' @examples
#' p1 <- parameter(name='par1',init=0)
#' p2 <- parameter(name='par2',init=5,priorDist='FlatPrior+')
#' p3 <- parameter(name='par3',init=4,priorDist='Gaussian',priorPar=c(5,2.3))
#' @export
parameter<-function(name,init,priorDist='FlatPrior',priorPar=numeric()){
  o<-new_parameter(name,init,priorDist,priorPar)
  return(validate_parameter(o))
}

#***************************************************************************----
# toString function ----
#' Parameter to string
#'
#' Convert an object of class 'parameter' into a ready-to-write vector of string
#'
#' @param x Parameter to be converted.
#' @param ... Optional arguments.
#' @return A string ready to be printed or written.
#' @examples
#' toString(parameter(name='par1',init=0))
#' toString(parameter(name='par3',init=4,priorDist='Gaussian',priorPar=c(5,2.3)))
#' @export
toString.parameter<-function(x,...){
  comment=c(
    'Parameter name',
    'Initial guess',
    paste0('Prior distribution. Type getCatalogue(printOnly=TRUE) ',
           'for a list of available distributions'),
    'Prior parameters'
  )
  return(toString_engine(x,comment))
}

#***************************************************************************----
# is function ----
#' Parameter tester
#'
#' Is an object of class 'parameter'?
#'
#' @param o Object, an object.
#' @return A logical equal to TRUE if class(o)== 'parameter', FALSE otherwise.
#' @keywords internal
is.parameter<-function(o){
  return(class(o)=='parameter')
}

#***************************************************************************----
# internal constructor ----
new_parameter<-function(name,init,priorDist,priorPar){
  stopifnot(is.character(name))
  stopifnot(is.numeric(init))
  stopifnot(is.character(priorDist))
  if(!is.null(priorPar)) stopifnot(is.numeric(priorPar))
  o <- list(name=name,init=init,priorDist=priorDist,priorPar=priorPar)
  class(o) <- 'parameter'
  return(o)
}

#***************************************************************************----
# validator ----
validate_parameter<-function(par){
  if(all(trimws(par$priorDist)!=getCatalogue()$distribution)){
    mess=paste0("Unknown `priorDist`: ",trimws(par$priorDist),
                ". Type getCatalogue(printOnly=TRUE) ",
                "for a list of available distributions.")
    stop(mess,call.=FALSE)
  }
  return(par)
}
