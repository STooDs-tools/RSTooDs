#***************************************************************************----
# Constructor ----
#' Model constructor.
#'
#' Creates a new instance of a 'model' object
#'
#' @param dataset Dataset object.
#' @param parentDist Character vector, name of the parent distribution(s).
#'   In cases where the predictand is multivariate, should be of size nVar.
#' @param varName Character vector, variable name(s), size nVar.
#'   Compulsory for a multivariate predictand (nVar > 1)
#' @param formula Character vector, formula(s) to retrieve each parameter
#'   of the parent distribution(s). Size sum(nParentPar).
#' @param par List of parameter objects, containing the parameters usable
#'   in the formulas.
#' @param process List of Process objects, containing the processes usable
#'   in the formulas.
#' @param name Character, name / description of the model
#' @return An object of class 'model'.
#' @examples
#' # Example of a very simple model, see case studies for more varied examples.
#' # Generate a fake dataset
#' dat <- dataset(Y=data.frame(Y=rlnorm(50)))
#' # Define parameters to be estimated: mu and sigma
#' param <- list(parameter(name='mu',init=0),parameter(name='sigma',init=1))
#' # Define model
#' mod <- model(dataset=dat,parentDist='LogNormal',
#'              formula=c('meanlog=mu','sdlog=sigma'),par=param)
#' @export
#'
model<-function(dataset,parentDist,
                varName=ifelse(length(parentDist)==1,as.character(dataset$var[1,1]),NA),
                formula=NULL,par=NULL,process=NULL,name='STooDs_model'){
  # Replace single-objects by a list of length 1
  # (user is likely to forgot to put a single process/par in a list)
  if(!is.null(par)){
    if(is.parameter(par)){safepar <- list(par)} else {safepar <- par}
  } else {safepar <- NULL}
  if(!is.null(process)){
    if(is.process(process)){safepro <- list(process)} else {safepro <- process}
  }else {safepro <- NULL}
  # Define default formula
  if(is.null(formula)){
    if(!is.null(safepar)){ # identity formula 'parname=parname'
      safeform <- c()
      for(i in 1:length(safepar)){
        f=paste(safepar[[i]]$name,'=',safepar[[i]]$name)
        safeform <- c(safeform,f)
      }
    } else {
      if(!is.null(safepro)){ # identity formula 'parname=proname'
        safeform <- c()
        for(i in 1:length(safepro)){
          f=paste(safepro[[i]]$name,'=',safepro[[i]]$name)
          safeform <- c(safeform,f)
        }
      } else {
        safeform <- NULL # will trigger an error message by validate_model
      }
    }
  } else {safeform <- formula} # user-provided formula
  o<-new_model(dataset,parentDist,varName,safeform,safepar,safepro,name)
  return(validate_model(o))
}

#***************************************************************************----
# toString function ----
#' Model to string
#'
#' Convert an object of class 'model' into a ready-to-write vector of string
#'
#' @param x Model to be converted.
#' @param ... Optional arguments.
#' @return A string ready to be printed or written.
#' @examples
#' # Generate a fake dataset
#' dat <- dataset(Y=data.frame(Y=rlnorm(50)))
#' # Define parameters to be estimated: mu and sigma
#' param <- list(parameter(name='mu',init=0),parameter(name='sigma',init=1))
#' # Define model
#' mod <- model(dataset=dat,parentDist='LogNormal',
#'              formula=c('meanlog=mu','sdlog=sigma'),par=param)
#' toString(mod)
#' @export
toString.model<-function(x,...){
  # Properties of the predictand Y
  nvar <- length(x$parentDist)
  varnames <- x$varName
  nppar <- vector(mode="integer",length=nvar)
  pparnames=c();k=0
  for(i in 1:nvar){
    # number of parameters for ith variable
    nppar[i]<-length(getParNames(x$parentDist[i]))
    # parameter names
    if(nppar[i]>0){
      for(j in 1:nppar[i]){
        k <- k+1
        pparnames=c(pparnames,
                    trimws(strsplit(x$formula[k],'=',fixed=TRUE)[[1]][1]))
      }
    }
  }
  # Properties of covariates, parameters and processes
  if(is.null(x$dataset$X)){
    ncov=0;covnames=''
  } else{
    ncov=NCOL(x$dataset$X);covnames=names(x$dataset$X)
  }
  if(is.null(x$par)){
    npar=0;parnames=''
  } else{
    npar=length(x$par);parnames=getNames(x$par)
  }
  if(is.null(x$process)){
    ndim=0;dimnames='';npro=0;pronames=''
    dimconfig='';proconfig=''
  } else {
    alldims=packDims(x$process)
    ndim=length(alldims)
    dimnames=getNames(alldims)
    dimconfig=paste0(dimnames,'.config')
    npro=length(x$process)
    pronames=getNames(x$process)
    proconfig=paste0(pronames,'.config')
  }

  value <- list(x$name,
             # Properties of the variables
             nvar,varnames,x$parentDist,nppar,pparnames,
             # Covariates
             ncov,covnames,
             # Parameters
             npar,parnames,'parameters.config',
             # Dimensions
             ndim,dimconfig,
             # Processes
             npro,pronames,proconfig
             )
  # Formulas
  for(i in 1:length(x$formula)){
    f <- trimws(strsplit(x$formula[i],'=',fixed=TRUE)[[1]][2])
    value=append(value,f)
  }
  # dataset configuration
  value=append(value,'dataset.config')

  comments=c(
    'Name / description of the model',
    'nVar (number of variables)',
    'Variable names (size nVar)',
    'Parent distribution for each variable (size nVar)',
    'nParentPar, number of parameters for each parent distribution (size nVar)',
    'parentParName, name of each parent parameter (size sum(nParentPar))',
    'nCov (number of covariates)',
    'Covariate names (size nCov)',
    'nPar (number of parameters)',
    'Parameter names (size nPar)',
    'Configuration file for parameters (priors)',
    'nDim (number of dimensions)',
    'Configuration files for dimensions (size nDim)',
    'nPro (number of processes)',
    'Process names (size nPro)',
    'Configuration files for processes (size nPro)'
  )
  for(i in 1:length(x$formula)){
    comments=c(comments,
               paste0('Formula for parent par number ',i,': ',
                      pparnames[i])
               )
    }
  comments=c(comments,'Configuration file for reading dataset')

  txt<-toString_engine(value,comments)
  return(txt)
}

#***************************************************************************----
# GetDAG ----
#
#' Model DAG
#'
#' Get the Directed Acyclic Graph  of a model
#'
#' @param mod Model
#' @param expandData logical. If true each data point defines a node
#' @param expandProcess logical. If true each process value defines a node
#' @param nmax integer. When expandData and/or expandProcess is true, nmax
#'             defines the maximum number of nodes for individual data
#'             or process values
#' @param dataNodeSize numerix. Size of a data node.
#' @param dparNodeSize logical. Size of parent distribution parameter node.
#' @param parNodeSize logical. Size of a parameter node.
#' @param processNodeSize logical. Size of a process node.
#' @param covNodeSize logical. Size of a covariate node.
#' @param dimNodeSize logical. Size of a dimension node.
#' @param minNodeSize logical. Minimum node size. Used for dataset/process
#'                    node when expandData/expandProcess is TRUE.
#' @return A DAG, i.e. a list with 2 data frames: nodes and links.
#' @examples
#' # Generate a fake dataset
#' dat <- dataset(Y=data.frame(Y=rlnorm(50)))
#' # Define parameters to be estimated: mu and sigma
#' param <- list(parameter(name='mu',init=0),parameter(name='sigma',init=1))
#' # Define model
#' mod <- model(dataset=dat,parentDist='LogNormal',
#'              formula=c('meanlog=mu','sdlog=sigma'),par=param)
#' # get its DAG
#' dag<-getDAG(mod)
#' @export
getDAG <- function(mod,expandData=TRUE,expandProcess=TRUE,nmax=100,
                   dataNodeSize=5,dparNodeSize=12,parNodeSize=dparNodeSize,
                   processNodeSize=dataNodeSize,covNodeSize=dparNodeSize,
                   dimNodeSize=25,minNodeSize=3){
  # initialize
  nodes <- data.frame(id=character(0),indx=integer(0),type=character(0),
                      dist=character(0),size=numeric(0),stringsAsFactors=FALSE)
  links <- data.frame(source=integer(0),target=integer(0),type=character(0),
                      width=numeric(0),stringsAsFactors=FALSE)
  kn <- 0 # nodes counter
  kf <- 0 # formula counter
  isPro <- FALSE # process detector
  slim <- 0.25
  fit <- 1
  fat <- 4
  # get variables
  var <- as.character(unique(mod$dataset$var)[,1])
  for(i in 1:length(var)){
    # data node
    size <- ifelse(expandData,minNodeSize,dimNodeSize)
    nodes <- rbind(nodes,data.frame(id=paste0('Target data:',var[i]),indx=kn,
                                    type='Target dataset',dist=mod$parentDist[i],
                                    size=size,stringsAsFactors=FALSE))
    kvar <- kn # remember it for later
    kn <- kn+1
    if(expandData){
      for(L in 1:min(nmax,sum(mod$dataset$var==var[i]))){
        nodes <- rbind(nodes,data.frame(id=paste0('Target data:',var[i],', value:',L),
                                        indx=kn,type='Target data value',dist=mod$parentDist[i],
                                        size=dataNodeSize,stringsAsFactors=FALSE))
        links <- rbind(links,data.frame(source=kn,target=kvar,type='Realisations',
                                        width=slim,stringsAsFactors=FALSE))
        kn <- kn+1
      }
    }
    # D-pars
    dpar <- getParNames(mod$parentDist[i])
    for(j in 1:length(dpar)){
      # get formula
      kf <- kf+1
      f <- mod$formula[kf]
      fsides <- strsplit(f,'=')[[1]] # separate LHS and RHS
      # add D-par node
      nodes <- rbind(nodes,data.frame(id=paste0('Parent parameter:',fsides[1]),
                                      indx=kn,type='Parameter of the parent distribution',dist='',
                                      size=dparNodeSize,stringsAsFactors=FALSE))
      links <- rbind(links,data.frame(source=kn,target=kvar,type='Stochastic',
                                      width=fat,stringsAsFactors=FALSE))
      kpar <- kn # remember it for later
      kn <- kn+1
      # variables in RHS of formula
      fvars <- all.vars(parse(text=fsides[2]))
      for(m in 1:length(fvars)){
          # PAR case
          if(fvars[m]%in%getNames(mod$par)){
            u <- which(fvars[m]==getNames(mod$par))
            type <- 'Parameter to be estimated'
            dist <- mod$par[[u]]$priorDist
            size <- parNodeSize
          }
          # PROCESS case
          if(fvars[m]%in%getNames(mod$process)){
            u <- which(fvars[m]==getNames(mod$process))
            type='Process'
            dist=mod$process[[u]]$dist
            isPro=TRUE
            size <- ifelse(expandProcess,minNodeSize,dimNodeSize)
          }
          # COVARIATE case
          if(fvars[m]%in%getNames(mod$dat)){
            type='Covariate'
            dist=''
            size <- covNodeSize
          }
          v <- which(fvars[m]==nodes$id)
          if(length(v)==0){ # add node and link
            nodes <- rbind(nodes,data.frame(id=fvars[m],
                                            indx=kn,type=type,dist=dist,
                                            size=size,stringsAsFactors=FALSE))
            links <- rbind(links,data.frame(source=kn,target=kpar,type='Deterministic',
                                            width=fit,stringsAsFactors=FALSE))
            kk <- kn # remember it for later
            kn <- kn+1
          } else { # node already there, just add link
            links <- rbind(links,data.frame(source=nodes$indx[v],target=kpar,type='Deterministic',
                                            width=fit,stringsAsFactors=FALSE))
            kk <- nodes$indx[v] # remember it for later
          }
          if(isPro & expandProcess){ # add individual process values
            dim <-  mod$process[[u]]$dim
            for(L in 1:min(nmax,NROW(dim$coord))){
              v <- which(paste0(fvars[m],'_',L)==nodes$id)
              if(length(v)==0){ # add node and link
                nodes <- rbind(nodes,data.frame(id=paste0(fvars[m],'_',L),
                                              indx=kn,type='Individual process value',dist=dist,
                                              size=processNodeSize,stringsAsFactors=FALSE))
                links <- rbind(links,data.frame(source=kn,target=kk,type='Realisations',
                                              width=slim,stringsAsFactors=FALSE))
                kn <- kn+1
              }
            }
          }
          if(isPro){ # add associated hyperparameters, parameters and dimension
            v <- which(paste0('Hyperparameters of ',fvars[m])==nodes$id)
            if(length(v)==0){ # new node and link
              # Hyperpar
              nodes <- rbind(nodes,data.frame(id=paste0('Hyperparameters of ',fvars[m]),
                                              indx=kn,type='Parameter of the parent distribution',dist='',
                                              size=parNodeSize,stringsAsFactors=FALSE))
              links <- rbind(links,data.frame(source=kn,target=kk,type='Stochastic',
                                              width=fat,stringsAsFactors=FALSE))
              khyper <- kn # remember it for later
              kn <- kn+1
            }
            # dimension
            dim <- paste0('Dimension: ',mod$process[[u]]$dim$name)
            v <- which(dim==nodes$id)
            if(length(v)==0){ # add node(s)
              # dim
              nodes <- rbind(nodes,data.frame(id=dim,
                                              indx=kn,type='Dimension',dist='',
                                              size=dimNodeSize,stringsAsFactors=FALSE))
              links <- rbind(links,data.frame(source=kn,target=khyper,type='Deterministic',
                                              width=fit,stringsAsFactors=FALSE))
              kdim <- kn # remember it for later
              kn <- kn+1
              # dim covariates
              cov <- mod$process[[u]]$dim$z
              if(!is.null(cov)){
                for(L in 1:length(cov)){
                  nodes <- rbind(nodes,data.frame(id=names(cov)[L],
                                                  indx=kn,type='Covariate',dist='',
                                                  size=covNodeSize,stringsAsFactors=FALSE))
                  links <- rbind(links,data.frame(source=kn,target=kdim,type='Deterministic',
                                                  width=fit,stringsAsFactors=FALSE))
                  kn <- kn+1
                }
              }
              # distance parameters
              parDist <- mod$process[[u]]$dim$d$par
              if(!is.null(parDist)){
                for(L in 1:length(parDist)){
                  nodes <- rbind(nodes,data.frame(id=parDist[[L]]$name,
                                                  indx=kn,type='Parameter to be estimated',
                                                  dist=parDist[[L]]$priorDist,
                                                  size=parNodeSize,stringsAsFactors=FALSE))
                  links <- rbind(links,data.frame(source=kn,target=kdim,type='Deterministic',
                                                  width=fit,stringsAsFactors=FALSE))
                  kn <- kn+1
                }
              }
            } else { # node exists, just add link
              links <- rbind(links,data.frame(source=nodes$indx[v],target=khyper,type='Deterministic',
                                              width=fit,stringsAsFactors=FALSE))
            }
            # parameters
            if(!is.null(mod$process[[u]]$par)){
              for (L in 1:length(mod$process[[u]]$par)){
                nodes <- rbind(nodes,data.frame(id=mod$process[[u]]$par[[L]]$name,
                                                indx=kn,type='Parameter to be estimated',
                                                dist=mod$process[[u]]$par[[L]]$priorDist,
                                                size=parNodeSize,stringsAsFactors=FALSE))
                links <- rbind(links,data.frame(source=kn,target=khyper,type='Deterministic',
                                                width=fit,stringsAsFactors=FALSE))
                kn <- kn+1
              }
            }
          }
          isPro=FALSE
      }
    }
  }
  return(list(nodes=nodes,links=links))
}

#***************************************************************************----
# plotDAG ----
#
#' Plot a DAG
#'
#' Plot the Directed Acyclic Graph  of a model
#'
#' @param DAG list. A DAG object, i.e. a list with 2 data frames (nodes and links)
#'            typically resulting from a call of the function 'getDAG()'
#' @param fontSize numeric. Text size.
#' @param opacity numeric between 0 ad 1. Nodes opacity.
#' @param legend logical. Add legend?
#' @param zoom logical. Enable zoom?
#' @param arrows logical. Use arrows for links?
#' @param colourScale string. A JavaScript/D3 valid colorscale.
#' @param ... other arguments passed to the function 'forceNetwork()'
#' @return nothing - plotting function.
#' @examples
#' # Generate a fake dataset
#' dat <- dataset(Y=data.frame(Y=rlnorm(50)))
#' # Define parameters to be estimated: mu and sigma
#' param <- list(parameter(name='mu',init=0),parameter(name='sigma',init=1))
#' # Define model
#' mod <- model(dataset=dat,parentDist='LogNormal',
#'              formula=c('meanlog=mu','sdlog=sigma'),par=param)
#' # get its DAG
#' dag<-getDAG(mod)
#' # plot it
#' plotDAG(dag)
#' @export
plotDAG <- function(DAG,fontSize=12,opacity=1,legend=TRUE,zoom=TRUE,
                    arrows=TRUE,colourScale=NULL,...){
  if (!requireNamespace("networkD3", quietly = TRUE)) {
    message('Package networkD3 is required to create this plot.')
    return()
  }
  # define colorscale
  if(is.null(colourScale)){
    colorScale <- paste0('d3.scaleOrdinal().domain([',
                         '"Target dataset", "Target data value",',
                         '"Parameter of the parent distribution",',
                         '"Parameter to be estimated","Process",',
                         '"Individual process value","Covariate","Dimension"',
                         ']).range([',
                         '"',getModelColor('dataset'),'",',
                         '"',getModelColor('data'),'",',
                         '"',getModelColor('parentparameter'),'",',
                         '"',getModelColor('parameter'),'",',
                         '"',getModelColor('process'),'",',
                         '"',getModelColor('processvalue'),'",',
                         '"',getModelColor('covariate'),'",',
                         '"',getModelColor('dimension'),'"',
                         ']);')
  } else {
    colorScale <- colourScale
  }
  cScale <- networkD3::JS(colorScale)
  # define link colors
  lcol <- rep('black',NROW(DAG$links))
  lcol[DAG$links$type=="Stochastic"]='red'
  lcol[DAG$links$type=="Realisations"]='lightgray'
  # define radius
  radius <- networkD3::JS("d.nodesize")
  # define link width
  linkWidth <- networkD3::JS("function(d) { return d.value; }")
  # Define link disctance
  linkDistance <- networkD3::JS("function(d) { return Math.min(100,100*d.value); }")
  # plot
  networkD3::forceNetwork(Links=DAG$links,Nodes=DAG$nodes,
               Source="source",Target="target",
               NodeID="id",Nodesize="size",Group="type",
               Value="width",radiusCalculation=radius,
               linkWidth=linkWidth,linkDistance = linkDistance,
               colourScale=cScale,linkColour = lcol,
               fontSize=fontSize,opacity=opacity,legend=legend,
               zoom=zoom,arrows=arrows,...)
}

#***************************************************************************----
# plot ----
#
#' plot a Model
#'
#' Plot the Directed Acyclic Graph of a model
#'
#' @param x Model
#' @param expandData logical. If true each data point defines a node
#' @param expandProcess logical. If true each process value defines a node
#' @param nmax integer. When expandData and/or expandProcess is true, nmax
#'             defines the maximum number of nodes for individual data
#'             or process values
#' @param dataNodeSize numerix. Size of a data node.
#' @param dparNodeSize logical. Size of parent distribution parameter node.
#' @param parNodeSize logical. Size of a parameter node.
#' @param processNodeSize logical. Size of a process node.
#' @param covNodeSize logical. Size of a covariate node.
#' @param dimNodeSize logical. Size of a dimension node.
#' @param minNodeSize logical. Minimum node size. Used for dataset/process
#'                    node when expandData/expandProcess is TRUE.
#' @param fontSize numeric. Text size.
#' @param opacity numeric between 0 ad 1. Nodes opacity.
#' @param legend logical. Add legend?
#' @param zoom logical. Enable zoom?
#' @param arrows logical. Use arrows for links?
#' @param colourScale string. A JavaScript/D3 valid colorscale.
#' @param ... other arguments passed to the function 'forceNetwork()'
#' @return nothing - plotting function.
#' @examples
#' # Generate a fake dataset
#' dat <- dataset(Y=data.frame(Y=rlnorm(50)))
#' # Define parameters to be estimated: mu and sigma
#' param <- list(parameter(name='mu',init=0),parameter(name='sigma',init=1))
#' # Define model
#' mod <- model(dataset=dat,parentDist='LogNormal',
#'              formula=c('meanlog=mu','sdlog=sigma'),par=param)
#' # plot it
#' plot(mod)
#' @export
plot.model <- function(x,expandData=TRUE,expandProcess=TRUE,nmax=100,
                       dataNodeSize=5,dparNodeSize=12,parNodeSize=dparNodeSize,
                       processNodeSize=dataNodeSize,covNodeSize=dparNodeSize,
                       dimNodeSize=25,minNodeSize=3,
                       fontSize=12,opacity=1,legend=TRUE,zoom=TRUE,
                       arrows=TRUE,colourScale=NULL,...){
  # create DAG
  dag<-getDAG(x,expandData=expandData,expandProcess=expandProcess,
              nmax=nmax,dataNodeSize=dataNodeSize,
              dparNodeSize=dparNodeSize,parNodeSize=parNodeSize,
              processNodeSize=processNodeSize,covNodeSize=covNodeSize,
              minNodeSize=minNodeSize)
  # Plot it
  plotDAG(DAG=dag,fontSize=fontSize,opacity=opacity,legend=legend,
          zoom=zoom,arrows=arrows,colourScale=colourScale,...)
}

#***************************************************************************----
# is function ----
#' Model tester
#'
#' Is an object of class 'model'?
#'
#' @param o Object, an object.
#' @return A logical equal to TRUE if class(o)== 'model', FALSE otherwise.
#' @keywords internal
is.model<-function(o){
  return(class(o)=='model')
}

#***************************************************************************----
# Model utilities ----

#' Get formula indices
#'
#' Indices of formulas corresponding to each variable
#'
#' @param x Model
#' @return A list of lenth nVar; the kth element gives the indices of the formulas
#'    associated with the kth variable
#' @examples
#' # Generate a fake dataset
#' dat <- dataset(Y=c(rlnorm(50),rpois(40,lambda=1)),var=c(rep('V1',50),rep('V2',40)))
#' # Define parameters to be estimated: mu, sigma and lambda
#' param <- list(parameter(name='mu',init=0),parameter(name='sigma',init=1),parameter(name='lambda',init=1))
#' # Define model
#' mod <- model(dataset=dat,parentDist=c('LogNormal','Poisson'),varName=c('V1','V2'),
#'              formula=c('meanlog=mu','sdlog=sigma','lambda=lambda'),par=param)
#' # Get formula indices
#' fIndx <- getFormulaIndices(mod)
#' mod$formula[fIndx[[1]]]
#' mod$formula[fIndx[[2]]]
#' @export
getFormulaIndices <- function(x){
  k=0
  out <- vector(mode='list',length=length(x$varName))
  for(i in 1:length(x$varName)){
    out[[i]]=(k+1):(k+length(getParNames(x$parentDist[i])))
    k=k+length(getParNames(x$parentDist[i]))
  }
  return(out)
}

#' Apply formulas
#'
#' Attempt to compute all formulas in a model, using parameter/process/covariate
#' values contained in an environment env.
#'
#' @param mod Model
#' @param env Environment, typically a named list.
#' @return A vector of size nFormula, containing the result of evaluated formulas
#' @examples
#' # Generate a fake dataset
#' dat <- dataset(Y=rlnorm(50),X=data.frame(x1=rnorm(50),x2=runif(50)))
#' # Define parameters to be estimated: mu0, mu1 , mu2 and sigma
#' param <- list(parameter(name='mu0',init=0),parameter(name='mu1',init=0),
#'               parameter(name='mu2',init=0),parameter(name='sigma',init=1))
#' # Define model
#' mod <- model(dataset=dat,parentDist='LogNormal',
#'              formula=c('meanlog=mu0+mu1*x1+mu2*x2','sdlog=sigma'),par=param)
#' # Evaluate formula
#' applyFormula(mod,env=list(mu0=0,mu1=0.1,mu2=-0.1,x1=1,x2=0.4,sigma=0.5))
#' # If environment is incomplete (here: x1 missing), a NA is returned for unevaluated formula
#' applyFormula(mod,env=list(mu0=0,mu1=0.1,mu2=-0.1,x2=0.4,sigma=0.5))
#' @export
applyFormula <- function(mod,env){
  out <- vector(mode='list',length=length(mod$formula))
  for(i in 1:length(mod$formula)){
    out[[i]]=tryCatch(eval(parse(text=mod$formula[i]),envir=env),
                      error=function(e) NA)
    names(out)[i] <- strsplit(x=mod$formula[i],split='=')[[1]][1]
  }
  return(out)
}

#***************************************************************************----
# internal constructor ----
new_model<-function(dataset,parentDist,varName,formula,par,process,name){
  stopifnot(is.dataset(dataset))
  stopifnot(is.character(parentDist))
  if(!any(is.na(varName))) {stopifnot(is.character(varName))}
  stopifnot(is.character(formula))
  if(!is.null(par)) {
    stopifnot(is.list(par))
    for(i in 1:length(par)){stopifnot(is.parameter(par[[i]]))}
  }
  if(!is.null(process)) {
    stopifnot(is.list(process))
    for(i in 1:length(process)){stopifnot(is.process(process[[i]]))}
  }
  stopifnot(is.character(name))
  o <- list(dataset=dataset,parentDist=parentDist,varName=varName,
            formula=formula,par=par,process=process,name=name)
  class(o) <- 'model'
  return(o)
}

#***************************************************************************----
# validator ----
validate_model<-function(mod){
  # varName is not NA
  if(any(is.na(mod$varName))){
    mess="`varName` is compulsory when several variables are used."
    stop(mess,call.=FALSE)
  }
  # Parent distribution(s) are available
  for(i in 1:length(mod$parentDist)){
    if(all(trimws(mod$parentDist[i])!=getCatalogue()$distribution)){
      mess=paste0("Unknown `parentDist`: ",trimws(mod$parentDist[i]),
                  ". Type getCatalogue(printOnly=TRUE) for a list ",
                  "of available distributions")
      stop(mess,call.=FALSE)
    }
  }
  # No duplicated names in parameters
  if(!is.null(mod$par)){
    names=getNames(mod$par)
    mask=(duplicated(names))
    if(any(mask)){ # duplicated parameter names
      mess=paste0("Duplicated parameter names:",
                  paste0(names[mask],collapse=','))
      stop(mess,call.=FALSE)
    }
  }

  # No duplicated names in processes
  if(!is.null(mod$process)){
    names=getNames(mod$process)
    mask=(duplicated(names))
    if(any(mask)){ # duplicated process names
      mess=paste0("Duplicated process names:",
                  paste0(names[mask],collapse=','))
      stop(mess,call.=FALSE)
    }
  }

  # Dimensions check
  if(!is.null(mod$process)){
    # start by packing all distinct dimensions used in processes in a list
    alldims=packDims(mod$process)
    # No duplicated names in used dimensions
    dnames=getNames(alldims)
    mask=(duplicated(dnames))
    if(any(mask)){ # duplicated process names
    mess=paste0("Duplicated dimension names:",
                paste0(dnames[mask],collapse=','))
    stop(mess,call.=FALSE)
    }
    # Check dataset%iDim is not null
    if(is.null(mod$dataset$iDim)){
      mess=paste0("Dimension(s) indices `iDim` missing in dataset. ",
                  "Dimensions used in this model: ",
                  paste0(dnames,collapse=','))
      stop(mess,call.=FALSE)
    }
    # Check names are consistent with the header of dataset%iDim
    dnames=getNames(alldims) # dimension names
    inames=names(mod$dataset$iDim) # iDim headers
    iDim_column=rep(NaN,length(alldims)) # column of kth dim in dataset%iDim
    for(i in 1:length(alldims)){
      indx=c()
      for(j in 1:length(inames)){
        # Check if dimension name is a substring of iDim name, and save index
        # if this is the case.
        if(grepl(dnames[i],inames[j],fixed=TRUE)){indx=c(indx,j)}
      }
      # Check that at least one iDim corresponds to the dimension
      if(length(indx)==0){
        mess=paste0("Dimension ",dnames[i],": cannot find a corresponding ",
                    "`iDim` header amongst the following: ",
                    paste0(inames,collapse=','),". Please make sure one ",
                    "of the header contains the string: ", dnames[i])
        stop(mess,call.=FALSE)
      }
      # Check that no more than one iDim corresponds to the dimension
      if(length(indx)>1){
        mess=paste0("Dimension ",dnames[i],": several `iDim` headers may ",
                    "correspond: ",paste0(inames[indx],collapse=','),
                    ". Please make sure exactly one header contains the string: ",
                    dnames[i])
        stop(mess,call.=FALSE)
      }
      # length(indx)==1, remember the index
      iDim_column[i]=indx
    }
    # reorder dataset$iDim to be in the same order as suggested by alldims
    mod$dataset$iDim <- mod$dataset$iDim[iDim_column]
  }

  # Get full namespace and check for no duplication
  namespace <- getNames(mod)
  mask=(duplicated(namespace))
  if(any(mask)){ # duplicated names
    mess=paste0("Duplicated names in model components:",
                paste0(namespace[mask],collapse=','))
    stop(mess,call.=FALSE)
  }

  # check formulas make sense
  namespace <- getNames(mod,full=FALSE) # only par,cov & processes
  for (i in 1:length(mod$formula)){
    f <- strsplit(mod$formula[i],'=',fixed=TRUE)[[1]]
    if(length(f)!=2) {
      mess=paste0("Formula ",i,": ",trimws(mod$formula[i]),
                  ": Formula should have the form parname = expression")
      stop(mess,call.=FALSE)
    }
    check=checkFormula(trimws(f[2]),namespace)
    if(!check$ok){
      mess=paste0("Formula ",i,": ",trimws(mod$formula[i]),
                  ". ",check$mess)
      stop(mess,call.=FALSE)
    }
  }

  return(mod)
}

#***************************************************************************----
# Get names ----
# Get namespace of a model, i.e. all parameters, covariates, and processes
getNames.model <- function(mod,full=TRUE){
  ns=c()
  # Parameters
  if(!is.null(mod$par)){ns=c(ns,getNames(mod$par))}
  # Covariates
  if(!is.null(mod$dataset$X)){ns=c(ns,names(mod$dataset$X))}
  # Processes
  if(!is.null(mod$process)){
    # process names
    ns=c(ns,getNames(mod$process))
    if(full==TRUE){
      # process namepaces
      if(!is.null(mod$process$par)){
        ns <- c(ns,getNames(p$par))
      }
      allDims <- packDims(mod$process)
      for (i in 1:length(allDims)){
        # name of the distance
        ns <- c(ns,paste0("D",trimws(allDims[[i]]$name)))
        # add covariates names
        if(!is.null(allDims[[i]]$z)){
          ns <- c(ns,names(allDims[[i]]$z))
        }
      }
    }
  }
  return(ns)
}

#***************************************************************************----
# packDims ----
# Pack all dims used in a list of processes in a list.
# The resulting list contains distinct dims - a 2 processes use the same dim,
# the latter appears only once in the list
packDims<-function(proList){
  alldims=list()
  k=0 # distinct dimensions counter
  for(i in 1:length(proList)){
    if(k==0){ # first dimension
      k=k+1;alldims[[k]]=proList[[i]]$dim
    } else{
      # compare with already-packed dims
      gotIt=FALSE
      for(j in 1:k){
        if(identical(proList[[i]]$dim,alldims[[j]])){gotIt=TRUE;break}
      }
      if(!gotIt){k=k+1;alldims[[k]]=proList[[i]]$dim}
    }
  }
  return(alldims)
}

#***************************************************************************----
# getModelColor ----
# Get color associated with a model element according to its type
# (parameter, data, hyperpar, etc.)
getModelColor<-function(element){
  out=switch(element,
             dataset='#1f77b4', # dark blue
             data='#aec7e8', # light blue
             parentparameter='#808080', # gray
             parameter='#ff7f0e', # orange
             process='#2ca02c', # dark green
             processvalue='#98df8a', # light green
             covariate='#d62728', # dark red
             dimension='#e377c2', # purple
             '#000000' # black
  )
  return(out)
}

#***************************************************************************----
# getMCMCColumnType ----
# Get type associated with each column of the MCMC samples, and associated color
getMCMCColumnType<-function(mod,ncolumn){
  colType <- vector("character",ncolumn)
  k <- 0
  # parameters
  m <- length(mod$par)
  if(m>0){colType[(k+1):(k+m)]='par';k=k+m}
  # processes
  npro <- length(mod$process)
  if(npro>0){
    for(i in 1:npro){
      # process values
      m <- NROW(mod$process[[i]]$dim$coord)
      if(m>0){colType[(k+1):(k+m)]='process';k=k+m}
      # hyperparameters
      m <- length(mod$process[[i]]$par)
      if(m>0){colType[(k+1):(k+m)]='hyperpar';k=k+m}
    }
  }
  # dimensions
  if(npro>0){
    dims <- packDims(mod$process)
    ndim <- length(dims)
    if(ndim>0){
      for(i in 1:ndim){
        m <- length(dims$d$par)
        if(m>0){colType[(k+1):(k+m)]='distancepar';k=k+m}
      }
    }
  }
  # inference functions
  colType[(k+1):(k+4)]='funk'

  # colors
  colorType=colType
  colorType[colType=='par'] <- getModelColor('parameter')
  colorType[colType=='process'] <- getModelColor('processvalue')
  colorType[colType=='hyperpar'] <- getModelColor('process')
  colorType[colType=='distancepar'] <- getModelColor('dimension')
  colorType[colType=='funk'] <- getModelColor('dataset')

  return(list(type=colType,color=colorType))
}


