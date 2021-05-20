#***************************************************************************----
# Constructor ----
#' Process constructor.
#'
#' Creates a new instance of a 'process' object
#'
#' @param name Character, name of the process.
#' @param dim Dimension object, on which the process is defined.
#' @param dist Character, name of the process (multivariate) distribution.
#' @param par Parameter, a list of parameter objects.
#' @param iScalarPar Integer vector, indices of scalar parameters in par.
#' @param fMean Character, formula to compute the mean vector.
#' @param fCovar Character, formula to compute the covariance matrix.
#' @param init Numeric or Character. If numeric, initial value of the process;
#'   if Character, name of the dim-covariate used to initialise the process.
#' @param constraint Numeric, constraint index. 0 = none,
#'   1 = process is centred, 2 = process is centred & scaled.
#' @return An object of class 'process'.
#' @examples
#' # create a time dimension
#' time <- dimension(name='time',coord=data.frame(t=1:50))
#' # unconstrained time process
#' pro <- process(name='unconstrained',dim=time)
#' # IID Gaussian time process, flat priors on hyperparameters
#' param <- list(parameter(name='mean',init=0),
#'               parameter(name='sdev',init=1))
#' pro <- process(name='IID',dim=time,dist='Gaussian_IID',
#'                par=param,iScalarPar=c(1,2))
#' # AR(1) Gaussian time process, informative priors on hyperparameters
#' param<-list(parameter(name='cte',init=0,priorDist='Gaussian',priorPar=c(0,5)),
#'             parameter(name='sdev.innov',init=1,priorDist='FlatPrior+'),
#'             parameter(name='rho',init=0.5,priorDist='Uniform',priorPar=c(0,1)))
#' pro <- process(name='AR1',dim=time,dist='Gaussian_AR1',
#'                par=param,iScalarPar=c(1,2,3))
#' # Gaussian spatial process with zero mean
#' space <- dimension(name='space',coord=data.frame(x=runif(50),y=runif(50)))
#' param<-list(parameter(name='mean',init=0,priorDist='FIX'),
#'             parameter(name='range',init=5,priorDist='Exponential',priorPar=c(0,5)),
#'             parameter(name='sill',init=0.5,priorDist='FlatPrior+'))
#' pro <- process(name='spatialGP',dim=space,dist='Gaussian',
#'                par=param,fMean='mean',fCovar='(sill^2)*exp(-Dspace/range)')
#' # NNGP (Nearest-Neighbor Gaussian Process) with 5 neighbors max
#' space <- dimension(name='space',coord=data.frame(x=runif(50),y=runif(50)))
#' param<-list(parameter(name='mean',init=0,priorDist='FIX'),
#'             parameter(name='range',init=5,priorDist='Exponential',priorPar=c(0,5)),
#'             parameter(name='sill',init=0.5,priorDist='FlatPrior+'),
#'             parameter(name='maxNN',init=5,priorDist='FIX'))
#' pro <- process(name='NNGP',dim=space,dist='NNGP',par=param,iScalarPar=4,
#'                fMean='mean',fCovar='(sill^2)*exp(-Dspace/range)')
#' @export
process<-function(name,dim,dist='Flat',par=NULL,iScalarPar=NULL,
                  fMean=NULL,fCovar=NULL,init=NULL,constraint=0){
  if(is.parameter(par)){ # single-parameter, user forgot to put it in a list
    p <- list(par)
  } else {p <- par}
  o <- new_process(name,dim,dist,p,iScalarPar,fMean,fCovar,init,constraint)
  return(validate_process(o))
}

#***************************************************************************----
# toString function ----
#' Process to string
#'
#' Convert an object of class 'process' into a ready-to-write vector of string
#'
#' @param x process to be converted
#' @param ... Optional arguments.
#' @return A string ready to be printed or written.
#' @examples
#' time <- dimension(name='time',coord=data.frame(t=1:50))
#' toString(process('unconstrained',time))
#' @export
toString.process<-function(x,...){
  if(is.null(x$par)){
    npar <- 0; namepar <- ''
  } else {
    npar <- length(x$par)
    namepar=vector("character",npar)
    for(i in 1:npar){namepar[i]=x$par[[i]]$name}
  }
  if(is.null(x$iScalarPar)){
    nSpar <- 0;iSpar <- 0
  } else {
    nSpar <- length(x$iScalarPar)
    iSpar <- x$iScalarPar
  }
  if(is.null(x$fMean)){fm <- ''} else {fm <- x$fMean}
  if(is.null(x$fCovar)){fc <- ''} else {fc <- x$fCovar}

  value=list(
    x$name,x$dim$name, # process and dimension names
    x$constraint,x$init, # constraint and initial value
    x$dist,npar,namepar, # properties of the distribution
    nSpar,iSpar, # properties of scalar parameters
    fm,fc # formulae for mean/covariance
  )
  comment=c(
    'Name of the process',
    'Name of the dimension the process is defined on',
    'Constraint index. 0 = none, 1 = centred, 2 = centred & scaled',
    'Initial value OR name of the dimension-covariate giving initial values',
    paste0('Distribution. Type getCatalogue(printOnly=TRUE) ',
           'for a list of available multivariate distributions'),
    'nPar (total number of parameters)',
    'Parameter names (size nPar)',
    'nSpar (number of scalar parameters)',
    'indices of scalar parameters in full parameter list (size nSpar)',
    paste0('Formula for distribution mean vector - may depend on ',
           'parameters above and covariates of the dimension'),
    paste0('Formula for distribution covariance matrix  - may depend on ',
           'parameters above, covariates of the dimension and pairwise ',
           'distances of the dimension, noted Ddimname (e.g. Dspace, Dtime)')
  )
  txt<-toString_engine(value,comment)
  if(npar>0 ){# add parameters properties
    for(i in 1:npar){
      txt=c(txt,toString(x$par[[i]]))
    }
  }
  return(txt)
}

#***************************************************************************----
# is function ----
#' Process tester.
#'
#' Is an object of class 'process'?
#'
#' @param o Object, an object.
#' @return A logical equal to TRUE if class(o)== 'process', FALSE otherwise.
#' @keywords internal
is.process<-function(o){
  return(class(o)=='process')
}

#***************************************************************************----
# internal constructor ----
new_process<-function(name,dim,dist,par,iScalarPar,fMean,fCovar,init,constraint){
  # check types are ok
  stopifnot(is.character(name))
  stopifnot(is.dimension(dim))
  stopifnot(is.character(dist))
  if(!is.null(par)) {
    stopifnot(is.list(par))
    for(i in 1:length(par)){stopifnot(is.parameter(par[[i]]))}
  }
  if(!is.null(iScalarPar)){
    stopifnot(all(iScalarPar%%1==0)) # iScalarPar should be integer
    iScal=as.integer(iScalarPar)
  } else{
    if(!is.null(par) & is.null(fMean) & is.null(fCovar)){
      # there are parameters and they are neither for fMean nor for fCovar
      # => by default they are scalar parameters
      iScal <- 1:length(par)
    } else {
      iScal <- NULL
    }
  }
  if(!is.null(fMean)){stopifnot(is.character(fMean))}
  if(!is.null(fCovar)){stopifnot(is.character(fCovar))}
  stopifnot(constraint%%1==0) # constraint should be integer
  if(!is.null(init)){ # use provided initial value
    stopifnot(is.numeric(init) | is.character(init))
    initial <- init
    dime <- dim
  } else {# need to work out default init value
    if(constraint==0){ # use 0 as init value
      initial=0
      dime <- dim
    } else {
      # # try a -1/+1 starting vector
      # starting <- data.frame(starting=(-1)^seq(NROW(dim$coord)))
      # try a random starting vector
      nr <- NROW(dim$coord)
      foo <- 1:nr #rnorm(nr)
      starting <- data.frame(starting=(foo-mean(foo))/sd(foo)) #(sd(foo)*((nr-1)/nr)))
      initial='starting'
      dime <- dim
      if(is.null(dime$z)){dime$z=starting} else {dime$z=cbind(dime$z,starting)}
    }
  }
  # return process
  o <- list(name=name,dim=dime,dist=dist,par=par,iScalarPar=iScal,fMean=fMean,
            fCovar=fCovar,init=initial,constraint=as.integer(constraint))
  class(o) <- 'process'
  return(o)
}

#***************************************************************************----
# validator ----
validate_process<-function(p){
  # Check distribution is available
  if(all(trimws(p$dist)!=getCatalogue()$mvtdistribution)){
    mess=paste0("Unknown `dist`: ",trimws(p$dist),
                ". Type getCatalogue(printOnly=TRUE) ",
                "for a list of available multivariate distributions.")
    stop(mess,call.=FALSE)
  }
  # Check constraint makes sense
  if( p$constraint<0 | p$constraint>2){
    mess="`constraint` can only be 0,1 or 2."
    stop(mess,call.=FALSE)
  }
  # Check there is no duplicated parameter name
  if(!is.null(p$par)){
    names=getNames(p$par)
    mask=(duplicated(names))
    if(any(mask)){ # duplicated parameter names
      mess=paste0("Duplicated parameter names:",
                  paste0(names[mask],collapse=','))
      stop(mess,call.=FALSE)
    }
  }
  # Get full namespace and check for no duplication
  namespace <- getNames(p)
  mask=(duplicated(namespace))
  if(any(mask)){ # duplicated names
    mess=paste0("Duplicated names in process components:",
                paste0(namespace[mask],collapse=','))
    stop(mess,call.=FALSE)
  }
  # check formulas make sense
  if(!is.null(p$fMean)){
    # Note that namespace[1] is the distance and should not be used in fMean
    check=checkFormula(p$fMean,namespace[-1])
    if(!check$ok){
      mess=paste0("fMean: ",check$mess)
      stop(mess,call.=FALSE)
    }
  }
  if(!is.null(p$fCovar)){
    check=checkFormula(p$fCovar,namespace)
    if(!check$ok){
      mess=paste0("fCovar: ",check$mess)
      stop(mess,call.=FALSE)
    }
  }
  return(p)
}

#***************************************************************************----
# Get names ----
# Get namespace of a process, i.e. names of
# parameters, covariates and distances
getNames.process <- function(p){
  # name of the distance
  ns <- paste0("D",trimws(p$dim$name))
  # add parameter names
  if(!is.null(p$par)){
    ns <- c(ns,getNames(p$par))
  }
  # add covariates names
  if(!is.null(p$dim$z)){
    ns <- c(ns,names(p$dim$z))
  }
  return(ns)
}
