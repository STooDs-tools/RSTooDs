#***************************************************************************----
# Constructor ----
#' Distance constructor.
#'
#' Creates a new instance of a 'distance' object
#'
#' @param funk Character, distance function.
#'   getCatalogue(printOnly=TRUE) for a list of available distance functions
#' @param par Parameter, a list of parameter objects.
#' @return An object of class 'distance'.
#' @examples
#' d1 <- distance(funk='Euclidean')
#' d2 <- distance(funk='Haversine')
#' @export
distance<-function(funk='Euclidean',par=NULL){
  if(is.parameter(par)){ # single-parameter, user forgot to put it in a list
    p <- list(par)
  } else {p <- par}
  o<-new_distance(funk,p)
  return(validate_distance(o))
}

#***************************************************************************----
# toString function ----
#' Distance to string
#'
#' Convert an object of class 'distance' into a ready-to-write vector of string
#'
#' @param x Distance to be converted.
#' @return A string ready to be printed or written.
#' @param ... Optional arguments.
#' @examples
#' toString(distance(funk='Euclidean'))
#' @export
toString.distance<-function(x,...){
  n<-length(x$par)
  comment=c(
    paste0('Distance function. Type getCatalogue(printOnly=TRUE) ',
           'for a list of available functions'),
    'Number of parameters for distance function'
  )
  txt<-toString_engine(list(x$funk,n),comment)
  if(n>0){
    for(i in 1:n){
      txt=c(txt,toString(x$par[[i]]))
    }
  }
  return(txt)
}

#***************************************************************************----
# is function ----
#' Distance tester.
#'
#' Is an object of class 'distance'?
#'
#' @param o Object, an object.
#' @return A logical equal to TRUE if class(o)== 'distance', FALSE otherwise.
#' @keywords internal
is.distance<-function(o){
  return(class(o)=='distance')
}

#***************************************************************************----
# internal constructor ----
new_distance<-function(funk,par){
  stopifnot(is.character(funk))
  if(!is.null(par)) {
    stopifnot(is.list(par))
    for(i in 1:length(par)){stopifnot(is.parameter(par[[i]]))}
  }
  o <- list(funk=funk,par=par)
  class(o) <- 'distance'
  return(o)
}

#***************************************************************************----
# validator ----
validate_distance<-function(d){
  if(all(trimws(d$funk)!=getCatalogue()$distance)){
    mess=paste0("Unknown `funk`: ",trimws(d$funk),
                ". Type getCatalogue(printOnly=TRUE) ",
                "for a list of available distance functions.")
    stop(mess,call.=FALSE)
  }
  if(!is.null(d$par)){
    names=getNames(d$par)
    mask=(duplicated(names))
    if(any(mask)){ # duplicated parameter names
      mess=paste0("Duplicated parameter names:",
                  paste0(names[mask],collapse=','))
      stop(mess,call.=FALSE)
    }
  }
  return(d)
}
