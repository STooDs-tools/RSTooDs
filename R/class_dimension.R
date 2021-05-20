#***************************************************************************----
# Constructor ----
#' Dimension constructor.
#'
#' Creates a new instance of a 'dimension' object
#'
#' @param name Character, name of the dimension.
#' @param coord Data frame (numeric), coordinates.
#' @param d Distance object.
#' @param z Data frame (numeric), covariates.
#' @param file Character, file where the dimension data are stored (full path).
#'   If NULL, it will be stored in the workspace with an automatic filename.
#' @return An object of class 'dimension'.
#' @examples
#' # Space dimension
#' coord <- data.frame(x=runif(50),y=runif(50))
#' d <- distance(funk='Euclidean')
#' space <- dimension(name='space',coord=coord,d=d)
#' # Time dimension
#' coord <- data.frame(t=1:50)
#' d <- distance(funk='Euclidean')
#' z <- data.frame(year=1960+(1:50))
#' time <- dimension(name='time',coord=coord,d=d,z=z)
#' @export
dimension<-function(name,coord,d=distance(),z=NULL,file=NULL){
  o <- new_dimension(name,coord,d,z,file)
  return(validate_dimension(o))
}

#***************************************************************************----
# toString function ----
#' Dimension to string
#'
#' Convert an object of class 'dimension' into a ready-to-write vector of string
#'
#' @param x Dimension object, dimension to be converted
#' @param ... Optional arguments.
#' @return A string ready to be printed or written.
#' @examples
#' toString(dimension(name='time',coord=data.frame(t=1:50)))
#' @export
toString.dimension<-function(x,...){
  p <- NCOL(x$coord)
  n <- NROW(x$coord)
  if(is.null(x$z)){
    pcov <- 0; icov <- 0; namecov <- ''
  } else {
    pcov <- NCOL(x$z); icov <- p+(1:pcov); namecov <- names(x$z)
  }
  if(!is.null(x$file)){fname=x$file} else {fname=''}
  value=list(
    x$name, # dimension name
    fname, # dimension datafile
    1,n,p+pcov, # properties of the datafile
    p,1:p, # properties of coordinates
    pcov,namecov,icov # properties of covariates
  )
  comment=c(
    'Name of the dimension',
    'File where the dimension dataset is stored',
    'Number of header lines',
    'nObs, number of rows in data file (excluding header lines)',
    'Number of columns in data file',
    'How many coordinates? (typically 1 for time, 2 for space, etc.)',
    'Column(s) for coordinates',
    'Number of covariates',
    'Covariates names',
    'Columns for covariates'
  )
  txt<-toString_engine(value,comment)
  txt=c(txt,toString(x$d)) # add distance properties
  return(txt)
}

#***************************************************************************----
# plot ----
#
#' plot a Dimension
#'
#' Plot a Dimension object (coordinates and covariates)
#'
#' @param x Dimension object.
#' @param col Color. color of the points or lines
#' @param alpha Numeric. transparency
#' @param size Numeric. point size
#' @param type character. 'l' (line), 'p' (points) or 'b' (both)
#' @param palette character. Name of the color distiller palette for coloring points on a map
#' @param colMax Integer.  Max number of columns in plot matrix
#' @param addWorld Logical. Add world contours?
#' @param ... Optional arguments.
#' @return nothing - plotting function.
#' @examples
#' # Define dimensions
#' time=dimension(name='time',coord=data.frame(t=1:50),z=data.frame(ft=rnorm(50)))
#' space=dimension(name='space',coord=data.frame(lon=runif(50),lat=runif(50)),
#'       z=data.frame(elev=rnorm(50)))
#' space3D=dimension(name='space',coord=data.frame(x=runif(50),y=runif(50),z=runif(50)),
#'         z=data.frame(z=rnorm(50)))
#' # plot them
#' plot(time)
#' plot(space)
#' plot(space3D)
#' @export
#' @import ggplot2
plot.dimension <- function(x,col='red',alpha=0.7,size=3,type='l',
                           palette= 'Spectral',colMax=4,addWorld=FALSE,...){
  p <- NCOL(x$coord)
  # More than two-dimensional: not supported, get out
  if(p>2){
    message('Plotting dimensions with more than 2 coordinates is not supported')
    return()
  }
  if (!requireNamespace("gridExtra", quietly = TRUE)) {
    message('Package gridExtra is required to create this plot.')
    return()
  }
  if(is.null(x$z)){n <- 1} else {n <- NCOL(x$z)}
  gList <- vector("list",n)
  if(is.null(x$z)){ # Coordinates plot only
    if(p==1){ # One-dimensional (e.g. time)
      DF <- data.frame(x=x$coord[,1],y=rep(0,NROW(x$coord)))
    }
    if(p==2){ # Two-dimensional (e.g. space)
      DF <- data.frame(x=x$coord[,1],y=x$coord[,2])
    }
    g <- ggplot(data=DF,aes(x=x,y=y))+geom_point(col=col,alpha=alpha,size=size)+
      scale_x_continuous(name=names(x$coord)[1])
    if(p==1){
      g <- g+ scale_y_continuous(name=NULL,breaks=NULL)
    }
    if(p==2){
      g <- g+ scale_y_continuous(name=names(x$coord)[2])
    }
    g <- g + theme_bw()
    gList[[1]] <- g
  } else { # covariate plot(s)
    for(i in 1:NCOL(x$z)){
      if(p==1){ # One-dimensional (e.g. time)
        DF <- data.frame(x=x$coord[,1],y=x$z[,i])
        g <- ggplot(data=DF,aes(x=x,y=y))+
          scale_x_continuous(name=names(x$coord)[1])+
          scale_y_continuous(name=names(x$z)[i])
        if(type %in% c('l','b')){g <- g+geom_line(col=col)}
        if(type %in% c('p','b')){g <- g+geom_point(col=col,alpha=alpha,size=size)}
      }
      if(p==2){ # Two-dimensional (e.g. space)
        DF <- data.frame(x=x$coord[,1],y=x$coord[,2],z=x$z[,i])
        g <- ggplot(data=DF,aes(x=x,y=y))+
          scale_x_continuous(name=names(x$coord)[1])+
          scale_y_continuous(name=names(x$coord)[2])+
          geom_point(aes(fill=z),alpha=alpha,size=size,pch=21)+
          scale_fill_distiller(names(x$z)[i],palette = palette)
      }
      g <- g + theme_bw()
      gList[[i]] <- g
    }
  }
  if(p==2 & addWorld){
    if (requireNamespace("maps", quietly = TRUE)) {
      world = map_data("world")
      for(i in 1:length(gList)){
        gList[[i]] <-gList[[i]] +
          geom_polygon(data=world,aes(long,lat,group=group),
                       fill="transparent",color="black",size=0.3)+
          coord_fixed(xlim=c(min(x$coord[,1]),max(x$coord[,1])),
                      ylim=c(min(x$coord[,2]),max(x$coord[,2])))
      }
    } else {
      message('Package maps is required to add world countours. Request is ignored')
    }
  }
  gridExtra::grid.arrange(grobs=gList,ncol=min(colMax,n))
}

#***************************************************************************----
# as.data.frame function ----
#' Dimension to data frame
#'
#' Convert an object of class 'dimension' into a data frame
#'
#' @param x Dimension object, dataset to be converted.
#' @param row.names NULL or a character vector giving the row names for the
#'     data frame. Missing values are not allowed.
#' @param optional logical, see ?as.data.frame. Unused here.
#' @param ... Optional arguments.
#' @return A data frame merging coordinates and covariates of the dimension.
#' @examples
#' as.data.frame(dimension(name='time',coord=data.frame(t=1:50)))
#' @export
as.data.frame.dimension<-function(x,row.names=NULL,optional=FALSE,...){
  DF=x$coord
  if(!is.null(x$z)){DF=cbind(DF,x$z)}
  if(!is.null(row.names)) {row.names(DF) <- row.names}
  return(DF)
}

#***************************************************************************----
# is function ----
#' Dimension tester.
#'
#' Is an object of class 'dimension'?
#'
#' @param o Object, an object.
#' @return A logical equal to TRUE if class(o)== 'dimension', FALSE otherwise.
#' @keywords internal
is.dimension<-function(o){
  return(class(o)=='dimension')
}

#***************************************************************************----
# internal constructor ----
new_dimension<-function(name,coord,d,z,file){
  stopifnot(is.character(name))
  stopifnot(is.data.frame(coord))
  stopifnot(is.distance(d))
  if(!is.null(z)){stopifnot(is.data.frame(z))}
  if(!is.null(file)){stopifnot(is.character(file))}
  o <- list(name=name,coord=coord,d=d,z=z,file=file)
  class(o) <- 'dimension'
  return(o)
}

#***************************************************************************----
# validator ----
validate_dimension<-function(d){
  if(!is.null(d$z)){
    n <- NROW(d$z)
    if(n!=NROW(d$coord)){
      mess="`coord` and `z` should have the same number of rows."
      stop(mess,call.=FALSE)
    }
    names=names(d$z)
    mask=(duplicated(names))
    if(any(mask)){ # duplicated names in z
      mess=paste0("Duplicated covariate names:",
                  paste0(names[mask],collapse=','))
      stop(mess,call.=FALSE)
    }
  }
  return(d)
}
