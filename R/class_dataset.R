#***************************************************************************----
# Constructor ----
#' Dataset constructor.
#'
#' Creates a new instance of a 'dataset' object
#'
#' @param Y Data frame (numeric, single column), predictand data.
#' @param var Data frame (factor, single column), variable in cases
#'   where the predictand is multivariate.
#' @param X Data frame (numeric), covariates (predictors) data.
#' @param iDim Data frame (integer), dimension indices. Column names should
#'   correspond to (or at least contain) the names of the dimension objects.
#'   For instance, if 2 dimensions objects with names 'time' and 'space' are
#'   used, the column of iDim should be named 'time'/'space', or possibly
#'   something like 'time_index'/'space_index'.
#' @param cType Data frame (integer), censoring type for each data.
#'   <0: 'less than' censoring, >0: 'more than' censoring, 0: interval censoring (see cWidth)
#' @param cWidth Data frame (numeric), width of the censoring interval for each data.
#'   True value is supposed to be in Y[i]+/- cWidth[i] (=> 0 leads to no censoring)
#' @param name Character, name of the dataset.
#' @param file Character, file where the dataset is stored (full path). If NULL, datafile
#'   will be stored in the workspace with an automatic filename
#' @return An object of class 'dataset'.
#' @examples
#' d <- dataset(Y=data.frame(annualDischarge=rlnorm(50)),
#'              X=data.frame(year=1960+(1:50)),
#'              iDim=data.frame(time=1:50))
#' @export
dataset<-function(Y,
                  var=data.frame(varName=factor(rep(names(as.data.frame(Y))[1],NROW(Y)))),
                  X=NULL,iDim=NULL,cType=rep(0,NROW(Y)),cWidth=rep(0,NROW(Y)),
                  name='STooDs_dataset',file=NULL){
  # Convert inputs into data frames
  Y.df <- as.data.frame(Y)
  var.df <- as.data.frame(var)
  if(!is.null(X)){X.df <- as.data.frame(X)} else {X.df <- NULL}
  if(!is.null(iDim)){iDim.df <- as.data.frame(iDim)} else {iDim.df <- NULL}
  cType.df <- as.data.frame(cType)
  cWidth.df <- as.data.frame(cWidth)
  o<-new_dataset(Y.df,var.df,X.df,iDim.df,cType.df,cWidth.df,name,file)
  return(validate_dataset(o))
}

#***************************************************************************----
# toString function ----
#' Dataset to string
#'
#' Convert an object of class 'dataset' into a ready-to-write vector of string
#'
#' @param x Dataset object, dataset to be converted.
#' @param ... Optional arguments.
#' @return A string ready to be printed or written.
#' @examples
#' toString(dataset(data.frame(Y=rlnorm(50))))
#' @export
toString.dataset<-function(x,...){
  nc=2
  if(!is.null(x$X)){
    iX=nc+(1:NCOL(x$X));nc=nc+NCOL(x$X)
  } else {iX=0}
  if(!is.null(x$iDim)){
    iDim=nc+(1:NCOL(x$iDim));nc=nc+NCOL(x$iDim)
  }else {iDim=0}
  icType=nc+1
  icWidth=nc+2
  nc=nc+2
  if(!is.null(x$file)){fname=x$file} else {fname=''}
  value=list(
    x$name,# dataset name
    fname, # datafile
    1,NROW(x$Y),nc, # dimensions
    1,2,iX,iDim,icType,icWidth # indices
  )

  comment=c(
    'Name / description of the dataset',
    'File where the dataset is stored',
    'Number of header lines',
    'nObs, number of rows in data file (excluding header lines)',
    'Number of columns in data file',
    'Column for y (predictand) in data file',
    'Column for ivar (variable index) in data file',
    'Columns for x (covariates aka predictors) in data file - comma-separated if several',
    'Columns for idims (dimension index) in data file - comma-separated if several',
    'Columns for censoring type in data file - 0 for no censoring',
    'Columns for censoring interval width in data file - 0 for no censoring'
  )

  txt<-toString_engine(value,comment)
  return(txt)
}

#***************************************************************************----
# as.data.frame function ----
#' Dataset to data frame
#'
#' Convert an object of class 'dataset' into a data frame
#'
#' @param x Dataset object, dataset to be converted.
#' @param row.names NULL or a character vector giving the row names for the
#'     data frame. Missing values are not allowed.
#' @param optional logical, see ?as.data.frame. Unused here.
#' @return A data frame merging all components of dataset x.
#' @param ... Optional arguments.
#' @examples
#' as.data.frame(dataset(data.frame(Y=rlnorm(50))))
#' @export
as.data.frame.dataset<-function(x,row.names=NULL,optional=FALSE,...){
  # Start with Y
  DF <- x$Y
  # Add variable ID
  DF <- cbind(DF,var=x$var[,1])
  # Add X and iDim if not null
  if(!is.null(x$X)){DF <- cbind(DF,x$X)}
  if(!is.null(x$iDim)){DF <- cbind(DF,x$iDim)}
  DF <- cbind(DF,x$cType)
  DF <- cbind(DF,x$cWidth)
  if(!is.null(row.names)) {row.names(DF) <- row.names}
  return(DF)
}

#***************************************************************************----
# is function ----
#' Dataset tester
#'
#' Is an object of class 'dataset'?
#'
#' @param o Object, an object.
#' @return A logical equal to TRUE if class(o)== 'dataset', FALSE otherwise.
#' @keywords internal
is.dataset<-function(o){
  return(class(o)=='dataset')
}

#***************************************************************************----
# internal constructor ----
new_dataset<-function(Y,var,X,iDim,cType,cWidth,name,file){
  stopifnot(is.data.frame(Y))
  stopifnot(is.data.frame(var))
  if(!is.null(X)) {stopifnot(is.data.frame(X))}
  if(!is.null(iDim)) {stopifnot(is.data.frame(iDim))}
  stopifnot(is.data.frame(cType))
  stopifnot(is.data.frame(cWidth))
  stopifnot(is.character(name))
  if(!is.null(file)){stopifnot(is.character(file))}
  o <- list(Y=Y,var=var,X=X,iDim=iDim,cType=cType,cWidth=cWidth,name=name,file=file)
  class(o) <- 'dataset'
  return(o)
}

#***************************************************************************----
# validator ----
validate_dataset<-function(dat){
  # Y: single-column
  if(NCOL(dat$Y)!=1){stop("`Y` should have a single column",call.=FALSE)}
  n=NROW(dat$Y)
  # var: single-column, Y-compatible, factor-valued
  if(NCOL(dat$var)!=1){stop("`var` should have a single column",call.=FALSE)}
  if(NROW(dat$var)!=n){
    stop("`var` and `Y` should have the same number of rows",call.=FALSE)
  }
  if(!is.factor(dat$var[,1])){
    stop("`var` should be a factor",call.=FALSE)
  }
  if(nlevels(dat$var[,1])>n){
    stop("There cannot be more variables than data points",call.=FALSE)
  }
  # X: Y-compatible
  if(!is.null(dat$X)){
    if(NROW(dat$X)!=n){
      stop("`X` and `Y` should have the same number of rows",call.=FALSE)
    }
  }
  # iDim: Y-compatible, integer-valued
  if(!is.null(dat$iDim)){
    if(NROW(dat$iDim)!=n){
      stop("`iDim` and `Y` should have the same number of rows",call.=FALSE)
    }
    if(!all(dat$iDim%%1==0)){
      stop("`iDim` should only take integer values",call.=FALSE)
    }
  }
  # cType: single-column, Y-compatible, integer-valued
  if(NCOL(dat$cType)!=1){stop("`cType` should have a single column",call.=FALSE)}
  if(NROW(dat$cType)!=n){
    stop("`cType` and `Y` should have the same number of rows",call.=FALSE)
  }
  if(!all(dat$cType%%1==0,na.rm=TRUE)){
    stop("`cType` should only take integer values",call.=FALSE)
  }
  # cWidth: single-column, Y-compatible, positive
  if(NCOL(dat$cWidth)!=1){stop("`cWidth` should have a single column",call.=FALSE)}
  if(NROW(dat$cWidth)!=n){
    stop("`cWidth` and `Y` should have the same number of rows",call.=FALSE)
  }
  if(!all(dat$cWidth>=0,na.rm=TRUE)){
    stop("`cWidth` should only take values larger or equal to zero",call.=FALSE)
  }
  # Check there is no duplicate namespace
  namespace<-getNames.dataset(dat)
  mask=(duplicated(namespace))
  if(any(mask)){ # duplicated names
    mess=paste0("Duplicated names in dataset:",
                paste0(namespace[mask],collapse=','))
    stop(mess,call.=FALSE)
  }
  # Remove missing values
  mask <- !is.na(dat$Y)
  dat2 <- dat
  dat2$Y <- data.frame(dat$Y[mask]);names(dat2$Y) <- names(dat$Y)
  dat2$var <- data.frame(dat$var[mask]);names(dat2$var) <- names(dat$var)
  if(!is.null(dat$X)){
    dat2$X <- data.frame(dat$X[mask,]);names(dat2$X) <- names(dat$X)
  }
  if(!is.null(dat$iDim)){
    dat2$iDim <- data.frame(dat$iDim[mask,]);names(dat2$iDim) <- names(dat$iDim)
  }
  dat2$cType <- data.frame(dat$cType[mask]);names(dat2$cType) <- names(dat$cType)
  dat2$cWidth <- data.frame(dat$cWidth[mask]);names(dat2$cWidth) <- names(dat$cWidth)
  return(dat2)
}

#***************************************************************************----
# Get names ----
# Get namespace of a dataset, i.e. names of all columns of data frames for
# Y, var, X and iDim
getNames.dataset <- function(dat){
  # name of Y and var
  ns <- c(names(dat$Y),names(dat$var))
  # add covariate names
  if(!is.null(dat$X)){
    ns <- c(ns,names(dat$X))
  }
  # add dimension indices names
  if(!is.null(dat$iDim)){
    ns <- c(ns,names(dat$iDim))
  }
  return(ns)
}
