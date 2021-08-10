#***************************************************************************----
# Main functions  ----

#*******************************************************************************
#' Run STooDs
#'
#' Run STooDs.exe
#'
#' @param model Model object, the model to be run.
#' @param workspace Character, directory where config and result files are stored.
#' @param dir.exe Character, directory where STooDs.exe stands.
#' @param name.exe Character, name of the executable without extension ('STooDs' by default).
#' @param run Logical, run STooDs.exe? if FALSE, just write config files.
#' @param mcmcOptions MCMC object, MCMC options
#' @return Nothing: just write config files and runs the executable.
#' @export
STooDs <- function(model,
                   workspace=file.path(getwd(),'STooDs_workspace'),
                   dir.exe=file.path(path.package('RSTooDs'),'bin'),
                   name.exe='STooDs',
                   run=TRUE,mcmcOptions=mcmc()){
  #oooooooooooooooooooooooooooooooooooooooooo
  # Write config files
  # Model
  txt <- toString(model)
  quickWrite(txt,workspace,'model.config')
  # Dataset
  dat=model$dataset
  if(is.null(dat$file)){ # store in workspace under an automatic filename
    dat$file=file.path(workspace,paste0(trimws(dat$name),'.data'))
  }
  txt <- toString(dat)
  quickWrite(txt,workspace,'dataset.config')
  df <- as.data.frame(dat)
  # transform factor column df$var into integer columns df$iVar
  iVar <- sapply(df$var,function(x){which(x==model$varName)})
  df$var <- iVar;names(df)[2] <- 'iVar'
  quickWriteDF(df,dat$file)
  # Parameters
  if(!is.null(model$par)){
    txt <- c()
    for (i in 1:length(model$par)){txt <- c(txt,toString(model$par[[i]]))}
    quickWrite(txt,workspace,'parameters.config')
  }
  # Processes
  if(!is.null(model$process)){
    pronames=getNames(model$process)
    for (i in 1:length(model$process)){
      txt <- toString(model$process[[i]])
      quickWrite(txt,workspace,paste0(pronames[i],'.config'))
    }
    # Dimensions
    alldims=packDims(model$process)
    for (i in 1:length(alldims)){
      dim=alldims[[i]]
      if(is.null(dim$file)){ # store in workspace under an automatic filename
        dim$file=file.path(workspace,paste0(trimws(dim$name),'.data'))
      }
      txt <- toString(dim)
      quickWrite(txt,workspace,paste0(dim$name,'.config'))
      df <- as.data.frame(dim)
      quickWriteDF(df,dim$file)
    }
  }
  # MCMC
  txt <- toString(mcmcOptions)
  quickWrite(txt,workspace,'mcmc.config')
  #oooooooooooooooooooooooooooooooooooooooooo
  # Run exe
  quickWrite(txt=addQuotes(paste0(workspace,.Platform$file.sep)),
             dir=dir.exe,fname='Config.txt')
  if(run){runExe(exedir=dir.exe,exename=name.exe)}
}

#*******************************************************************************
#' MCMC Reader
#'
#' Read raw MCMC samples, return cooked (burnt & sliced) ones
#'
#' @param file Character, full path to MCMC file.
#' @param burnFactor Numeric, burn factor. 0.1 means the first 10 percent iterations
#'    are discarded.
#' @param slimFactor Integer, slim factor. 10 means that only one iteration
#'    every 10 is kept.
#' @param sep Character, separator used in MCMC file.
#' @param reportFile Character, full path to pdf report file, not created if NULL
#' @param model Model, model object for smart coloring (black if NULL)
#' @param panelPerCol Integer, max number of panels per column
#' @param panelHeight Numeric, height of each panel
#' @param panelWidth Numeric, width of each panel
#' @return A data frame containing the cooked mcmc samples.
#' @export
readMCMC <- function(file='MCMC.txt',burnFactor=0.1,slimFactor=10,sep=';',
                     reportFile=NULL,model=NULL,
                     panelPerCol=10,panelHeight=3,panelWidth=23/panelPerCol){
  # read file
  raw <- utils::read.table(file,header=T,sep=sep)
  n <- NROW(raw);p <- NCOL(raw)
  keep <- seq(max(floor(n*burnFactor),1),n,slimFactor)
  cooked <- raw[keep,]
  if(!is.null(reportFile)){
    if (requireNamespace("gridExtra", quietly = TRUE)) {
      ncol <- ifelse(p>=panelPerCol,panelPerCol,p)
      grDevices::pdf(file=reportFile,width=ncol*panelWidth,
          height=ceiling(p/panelPerCol)*panelHeight,useDingbats=FALSE)
      gList <- vector("list",p)
      if(!is.null(model)){ # smart colorscheme
        colors <- getMCMCColumnType(model,p)$color
      } else { # back in black
        colors <- rep('black',p)
      }
      for(i in 1:p){
        gList[[i]] <- tracePlot(sim=raw[,i],ylab=names(raw)[i],
                                keep=keep,col=colors[i])
      }
      gridExtra::grid.arrange(grobs=gList,ncol=ncol)
      grDevices::dev.off()
    } else {
      warning(paste('Package gridExtra is required for reporting.',
              'Report file is not created'))
    }
  }
  return(cooked)
}

#*******************************************************************************
#' MCMC trace plots
#'
#' Generate a trace of MCMC samples for all variables
#'
#' @param mcmc data frame, MCMC samples (typically from a call to readMCMC())
#' @param model Model, model object for smart coloring (black if NULL)
#' @param panelPerCol Integer, max number of panels per column
#' @export
plotMCMC.trace <- function(mcmc,model=NULL,panelPerCol=10){
  if (requireNamespace("gridExtra", quietly = TRUE)) {
    p <- NCOL(mcmc)
    ncol <- ifelse(p>=panelPerCol,panelPerCol,p)
    if(!is.null(model)){ # smart colorscheme
      colors <- getMCMCColumnType(model,p)$color
    } else { # back in black
      colors <- rep('black',p)
    }
    gList <- vector("list",p)
    for(i in 1:p){
      gList[[i]] <- tracePlot(sim=mcmc[,i],ylab=names(mcmc)[i],col=colors[i])
    }
    gridExtra::grid.arrange(grobs=gList,ncol=ncol)
  } else {
    warning('Package gridExtra is required to create this plot.')
  }
}

#*******************************************************************************
#' MCMC parameter plots
#'
#' MCMC-based estimation of marginal posterior pdf for each parameter
#'
#' @param mcmc data frame, MCMC samples (typically from a call to readMCMC())
#' @param model Model, model object
#' @param panelPerCol Integer, max number of panels per column
#' @export
plotMCMC.par <- function(mcmc,model,panelPerCol=10){
  if (requireNamespace("gridExtra", quietly = TRUE)) {
    p <- NCOL(mcmc)
    ncol <- ifelse(p>=panelPerCol,panelPerCol,p)
    foo <- getMCMCColumnType(model,p)
    colType <- foo$type
    colorType <- foo$color
    indx <- which(colType %in% c('par','hyperpar','distancepar'))
    m <- length(indx)
    if(m==0){return()}
    ncol <- ifelse(m>=panelPerCol,panelPerCol,m)
    gList <- vector("list",m)
    for(i in 1:m){
      gList[[i]] <- densityPlot(sim=mcmc[,indx[i]],xlab=names(mcmc)[indx[i]],
                                col=colorType[indx[i]])
    }
    gridExtra::grid.arrange(grobs=gList,ncol=ncol)
  } else {
    warning('Package gridExtra is required to create this plot.')
  }
}

#*******************************************************************************
#' MCMC process plots
#'
#' MCMC-based estimation of marginal posterior pdf for each process
#'
#' @param mcmc data frame, MCMC samples (typically from a call to readMCMC())
#' @param model Model, model object
#' @export
plotMCMC.process <- function(mcmc,model){
  if (requireNamespace("gridExtra", quietly = TRUE)) {
    npro <- length(model$process)
    if(npro==0){return()}
    # Violin plot of all processes on a single page
    gList <- vector("list",npro)
    for(i in 1:npro){
      # determine columns for each process by process name
      cnames=paste0(model$process[[i]]$name,'_',1:NROW(model$process[[i]]$dim$coord))
      indx=which(names(mcmc) %in% cnames)
      gList[[i]] <- violinPlot(sim=mcmc[,indx],ylab=model$process[[i]]$name,
                               col=getModelColor('processvalue'))
    }
    gridExtra::grid.arrange(grobs=gList,ncol=1)
  } else {
    warning('Package gridExtra is required to create this plot.')
  }
}

#*******************************************************************************
#' MCMC reporting
#'
#' Generate pdf report files summarizing mcmc samples
#'
#' @param mcmc data frame, MCMC samples (typically from a call to readMCMC())
#' @param model Model object, the model that has been MCMC-sampled.
#' @param traceFile Character, full path to traceplots pdf file, not created if NULL
#' @param parFile Character, full path to parplots pdf file, not created if NULL
#' @param processFile1 Character, full path to 1st processplot pdf file, not created if NULL
#' @param processFile2 Character, full path to 2nd processplot pdf file, not created if NULL
#' @param panelPerCol Integer, max number of panels per column
#' @param panelHeight Numeric, height of each panel
#' @param panelWidth Numeric, width of each panel
#' @param addWorld Logical, add world contours for spatial processes?
#' @export
MCMCreport <- function(mcmc,model,traceFile='MCMCtrace.pdf',
                       parFile='MCMCpar.pdf',processFile1='MCMCprocess.pdf',
                       processFile2='MCMCprocess2.pdf',
                       panelPerCol=10,panelHeight=3,panelWidth=23/panelPerCol,
                       addWorld=FALSE){

  p <- NCOL(mcmc)
  colType <- getMCMCColumnType(model,p)$type

    # Trace plot
  if(!is.null(traceFile)){
    ncol <- ifelse(p>=panelPerCol,panelPerCol,p)
    grDevices::pdf(file=traceFile,width=ncol*panelWidth,
        height=ceiling(p/panelPerCol)*panelHeight,useDingbats=FALSE)
    plotMCMC.trace(mcmc,model,panelPerCol)
    grDevices::dev.off()
  }

  # parameters
  if(!is.null(parFile)){
    m <- sum(colType %in% c('par','hyperpar','distancepar'))
    if(m>0){
      ncol <- ifelse(m>=panelPerCol,panelPerCol,m)
      grDevices::pdf(file=parFile,width=ncol*panelWidth,
          height=ceiling(m/panelPerCol)*panelHeight,useDingbats=FALSE)
      plotMCMC.par(mcmc,model,panelPerCol)
      grDevices::dev.off()
    }
  }

  # processes
  npro <- length(model$process)
  if(npro>0 & !is.null(processFile1)){
    grDevices::pdf(file=processFile1,width=23,height=npro*panelHeight,useDingbats=FALSE)
    plotMCMC.process(mcmc,model)
    grDevices::dev.off()
  }
  if(npro>0 & !is.null(processFile2)){
    # process-specific pages
    grDevices::pdf(file=processFile2,useDingbats=FALSE)
    for(i in 1:npro){
      # get posterior median
      cnames=paste0(model$process[[i]]$name,'_',1:NROW(model$process[[i]]$dim$coord))
      indx=which(names(mcmc) %in% cnames)
      pmed <- apply(mcmc[,indx],2,stats::median)
      # use the plot.dimension function
      z <-data.frame(pmed);names(z) <- model$process[[i]]$name
      dim <- dimension(name=model$process[[i]]$dim$name,
                       coord=model$process[[i]]$dim$coord,
                       d=model$process[[i]]$dim$d,
                       z=z)
      plot.dimension(dim,addWorld=addWorld)
    }
    grDevices::dev.off()
  }
}

#*******************************************************************************
#' tracePlot
#'
#' returns a trace plot ggplot (or a list thereof if several columns in sim)
#'
#' @param sim vector or matrix or data frame, MCMC simulations
#' @param ylab Character, label of y-axis
#' @param keep Integer vector, indices of samples to be kept in cooked MCMC sample
#' @param col Color
#' @param psize Numeric, point size
#' @return A ggplot (or a list thereof if several columns in sim)
#' @export
#' @import ggplot2
tracePlot <- function(sim,ylab='values',keep=NULL,col='black',psize=0.5){
  p <- NCOL(sim)
  g <- vector("list",p)
  for(i in 1:p){
    DF <- data.frame(x=1:NROW(sim),y=as.data.frame(sim)[,i])
    if(is.null(keep)){lcol <- col} else {lcol <- 'lightgray'}
    g[[i]] <- ggplot()+
      scale_x_continuous('Iteration')+
      scale_y_continuous(ylab)+
      geom_line(data=DF,aes(x=x,y=y),col=lcol)
    if(!is.null(keep)){
      g[[i]] <- g[[i]] + geom_point(data=DF[keep,],aes(x=x,y=y),col=col,size=psize) +
        geom_vline(xintercept=keep[1],col='red')
    }
    g[[i]] <- g[[i]] + theme_bw()
  }
  if(p==1){return(g[[1]])} else {return(g)}
}

#*******************************************************************************
#' densityPlot
#'
#' returns a histogram+density ggplot (or a list thereof if several columns in sim)
#'
#' @param sim vector or matrix or data frame, MCMC simulations
#' @param xlab Character, label of x-axis
#' @param col Color
#' @return A ggplot (or a list thereof if several columns in sim)
#' @export
#' @import ggplot2
densityPlot <- function(sim,xlab='values',col='black'){
  p <- NCOL(sim)
  g <- vector("list",p)
  for(i in 1:p){
    DF <- data.frame(val=as.data.frame(sim)[,i])
    g[[i]] <- ggplot(DF,aes(val))+
      scale_x_continuous(xlab)+
      scale_y_continuous('posterior pdf')+
      geom_histogram(aes(y=..density..),fill=NA,col='black',
                     binwidth=(max(DF$val)-min(DF$val))/30)+
      geom_density(fill=col,alpha=0.5,col=NA)

    g[[i]] <- g[[i]] + theme_bw()
  }
  if(p==1){return(g[[1]])} else {return(g)}
}

#*******************************************************************************
#' violinPlot
#'
#' returns a violinplot ggplot
#'
#' @param sim vector or matrix or data frame, MCMC simulations
#' @param ylab Character, label of y-axis
#' @param col Color
#' @return A ggplot
#' @export
#' @import ggplot2
violinPlot <- function(sim,ylab='values',col='black'){
  if (requireNamespace("tidyr",quietly=TRUE)) {
    DF=tidyr::gather(as.data.frame(sim))
    DF$key2 <- reorder(DF$key, 1:NROW(DF)) # avoids violins being re-ordered alphabetically
    g <- ggplot(DF,aes(x=key2,y=value))+
      geom_violin(fill=col)+
      scale_x_discrete('Index',labels=1:NCOL(sim))+
      scale_y_continuous(ylab)
    g <- g + theme_bw()
    return(g)
  } else {
    warning('Package tidyr is required to create this plot.')
  }
}

#***************************************************************************----
# Catalogue ----

#*******************************************************************************
#' STooDs catalogue
#'
#' Distributions, multivariate distributions and distances available in STooDs
#'
#' @param printOnly Logical, should the catalogue be returned or only printed?
#' @return If \code{printOnly==FALSE}, a list with the following fields:
#' \describe{
#'   \item{distribution}{available univariate distributions.}
#'   \item{mvtdistribution}{available multivariate distributions.}
#'   \item{distance}{available distances.}
#' }
#' @examples
#' catalogue <- getCatalogue()
#' getCatalogue(printOnly=TRUE)
#' @export
getCatalogue<-function(printOnly=FALSE){
  # available distributions
  dist=c('Gaussian','Uniform','Triangle','LogNormal','LogNormal3',
         'Exponential','GPD','Gumbel','GEV','GEV_min','Inverse_Chi2','PearsonIII',
         'Beta','Kumaraswamy',
         'Geometric','Poisson','Bernoulli','Binomial','NegBinomial',
         'FlatPrior','FlatPrior+','FlatPrior-','FIX')
  # available multivariate distributions
  mvtdist=c('Gaussian','Gaussian_IID','Gaussian_AR1','Gaussian_AR1_vmean','NNGP','Uniform','Flat')
  # available dstances
  distance=c('Euclidean','Haversine')
  if(printOnly){
    message('DISTRIBUTIONS:')
    print(dist)
    message('MULTIVARIATE DISTRIBUTIONS:')
    print(mvtdist)
    message('DISTANCES:')
    print(distance)
  } else{
    return(list(distribution=dist,mvtdistribution=mvtdist,distance=distance))
  }
}

#*******************************************************************************
#' Get parameter names
#'
#' Get parameter names for a distribution d
#'
#' @param d Character (possibly vector), distribution (possibly distributions)
#' @return A character vector with parameter names.
#' @examples
#' parnames <- getParNames('GEV')
#' npar <- length(getParNames('Gumbel'))
#' @export
getParNames<-function(d){
  names=c()
  for(i in 1:length(d)){
    name=switch(d[i],
                'Gaussian'=c('mean','sd'),
                'Uniform'=c('lowBound','highBound'),
                'Triangle'=c('peak','lowBound','highBound'),
                'LogNormal'=c('meanlog','sdlog'),
                'LogNormal3'=c('threshold','meanlogexcess','sdlogexcess'),
                'Exponential'=c('threshold','scale'),
                'GPD'=c('threshold','scale','shape'),
                'Gumbel'=c('location','scale'),
                'GEV'=c('location','scale','shape'),
                'GEV_min'=c('location','scale','shape'),
                'Inverse_Chi2'=c('dof','scale'),
                'PearsonIII'=c('location','scale','shape'),
                'Beta'=c('shape1','shape2'),
                'Kumaraswamy'=c('shape1','shape2'),
                'Geometric'=c('prob'),
                'Poisson'=c('rate'),
                'Bernoulli'=c('prob'),
                'Binomial'=c('prob','ntrials'),
                'NegBinomial'=c('prob','nfails'),
                'FlatPrior'=c(),
                'FlatPrior+'=c(),
                'FlatPrior-'=c(),
                'FIX'=c(),
                NA)
    names=c(names,name)
  }
  return(names)
}

#***************************************************************************----
# Generics ----

#*******************************************************************************
#' Generic getNames function
#' @param o Object
#' @param ... Optional arguments.
#' @return A character vector containg names
#' @export
getNames<-function(o,...){UseMethod("getNames")}

#*******************************************************************************
#' Default getNames from an object or a list of objects having a $name field
#' (e.g. parameters,processes)
#' @param loo List Of Objects
#' @return A character vector containg names
#' @export
getNames.default<-function(loo){
  if(is.null(loo)) {return(NULL)}
  if(!is.null(loo$name)){return(loo$name)}
  n=length(loo)
  if(n==0){return(NULL)}
  txt=vector("character",n)
  for(i in 1:n){
    foo=tryCatch(loo[[i]]$name,error=function(e){NaN})
    if(is.null(foo)){txt[i]=NaN} else {txt[i]=foo}
  }
  return(txt)
}

#***************************************************************************----
# Utilities to manage formulas & parent distributions ----

#' Get environment associated with a row in the dataset
#'
#' Get environment (i.e. parameter/process/covariate values) associated with
#' a row in the dataset, using values contained in a named vector 'estimate'
#' (typically: posterior max or median).
#' For instance if 'estimate' contains columns named 'p_1',...'p_50' corresponding
#' to process values at 50 sites, this function retrieves the site associated with
#' 'row' and extract the corresponding 'p' value.
#' @param model Model
#' @param estimate Named vector
#' @param row Integer, row in the dataset
#' @return A list containing the environment
#' @examples
#' # Generate a fake dataset
#' dat <- dataset(Y=rlnorm(50),X=1:50)
#' # Define parameters to be estimated: mu0, mu1 and sigma
#' param <- list(parameter(name='mu0',init=0),parameter(name='mu1',init=0),
#'               parameter(name='sigma',init=1))
#' # Define model
#' mod <- model(dataset=dat,parentDist='LogNormal',
#'              formula=c('meanlog=mu0+mu1*X','sdlog=sigma'),par=param)
#' # Get environment
#' getRowEnv(mod,estimate=list(mu0=0,mu1=0.1,sigma=1),row=1)
#' getRowEnv(mod,estimate=list(mu0=0,mu1=0.1,sigma=1),row=10)
#' @export
getRowEnv <- function(model,estimate,row){
  if(row > NROW(model$dataset$Y)){
    mess=paste0('row is larger than number of rows in model$dataset$Y')
    stop(mess,call.=FALSE)
  }
  env <- vector(mode='list',length=0)
  # get predictors
  if(!is.null(model$dataset$X)){
    env <- c(env,model$dataset$X[row,,drop=FALSE])
  }
  # get parameters
  if(length(model$par)>0){
    for (i in 1:length(model$par)){
      parName <- model$par[[i]]$name # parameter name
      # determine the corresponding column in vector estimate
      col <- which(names(estimate)==parName)
      if(length(col)==0){
        mess=paste0('Cannot find value named ',parName, ' in vector estimate')
        stop(mess,call.=FALSE)
      }
      # add value to environment
      env <- c(env,estimate[col,drop=FALSE])
    }
  }
  # get processes
  if(length(model$process)>0){
    for (i in 1:length(model$process)){
      pro <- model$process[[i]] # process
      proName <- pro$name # process name
      dimName <- pro$dim$name # name of associated dimension
      # determine the corresponding column in iDim
      col <- which(grepl(dimName,names(model$dataset$iDim),fixed=TRUE))
      if(length(col)==0){
        mess=paste0('Cannot find column ',dimName, ' in model$dataset$iDim')
        stop(mess,call.=FALSE)
      }
      # get index
      indx <- model$dataset$iDim[row,col]
      if(indx>0){ # this dimension is active for this data
        # build name to be found mcmc header
        head <- paste0(proName,'_',indx)
        # determine the corresponding column
        col <- which(names(estimate)==head)
        if(length(col)==0){
          mess=paste0('Cannot find value named ',head, ' in vector estimate')
          stop(mess,call.=FALSE)
        }
        # add value to environment
        env <- c(env,estimate[col])
        names(env)[length(env)] <- proName
      }
    }
  }
  # return
  return(env)
}

#' Get parent distribution associated with row(s) in the dataset
#'
#' Get parent distribution (name + parameters) associated with
#' rows in the dataset, using values contained in a named vector 'estimate'
#' (typically: posterior max or median).
#' @param model Model
#' @param estimate Named vector
#' @param rows Integer vector, rows in the dataset
#' @return A list of size nRows; each element is itself a list with fields
#'    $var (variable name), $dist (character, distribution name)
#'    and $par (numeric vector, parameters)
#' @examples
#' # Generate a fake dataset
#' dat <- dataset(Y=rlnorm(50),X=1:50)
#' # Define parameters to be estimated: mu0, mu1 and sigma
#' param <- list(parameter(name='mu0',init=0),parameter(name='mu1',init=0),
#'               parameter(name='sigma',init=1))
#' # Define model
#' mod <- model(dataset=dat,parentDist='LogNormal',
#'              formula=c('meanlog=mu0+mu1*X','sdlog=sigma'),par=param)
#' # Get environment
#' res <- getParentDist(mod,estimate=list(mu0=0,mu1=1,sigma=1))
#' i=1;paste0('Distribution: ',res[[i]]$dist,' - parameters: ',paste(res[[i]]$par,collapse=','))
#' i=10;paste0('Distribution: ',res[[i]]$dist,' - parameters: ',paste(res[[i]]$par,collapse=','))
#' i=50;paste0('Distribution: ',res[[i]]$dist,' - parameters: ',paste(res[[i]]$par,collapse=','))
#' @export
getParentDist <- function(model,estimate,rows=1:NROW(model$dataset$Y)){
  out <- vector(mode='list',length=length(rows))
  for(i in 1:length(rows)){
    row <- rows[i]
    env <- getRowEnv(model,estimate,row)
    f <- applyFormula(model,env)
    indx=which(model$varName == model$dataset$var[row,1])
    if(length(indx)==0){
      mess=paste0('Cannot find variable named ',model$dataset$var[row,1],
                  ' in list of variable names(model$varName)')
      stop(mess,call.=FALSE)
    }
    v <- model$varName[indx]
    d <- model$parentDist[indx]
    fIndx <- getFormulaIndices(model)[[indx]]
    out[[i]]=list(var=v,dist=d,par=as.numeric(f[fIndx]))
  }
  return(out)
}
#***************************************************************************----
