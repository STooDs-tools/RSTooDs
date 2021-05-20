#***************************************************************************----
# Constructor ----
#' MCMC object constructor.
#'
#' Creates a new instance of a 'mcmc' object
#'
#' @param Nsim Integer, total number of iterations.
#' @param Nadapt Integer, adaptation period: jump sizes are increased/decreased
#'     every Nadapt iterations to comply with the desired moving rates.
#' @param stopAdapt Integer, no more adaptation after iteration number stopAdapt.
#' @param Nburn Integer, number of inital iterations that will not be written to file.
#' @param Nslim Integer, only one iteration very Nslim will be written to file.
#' @param minMR Numeric in (0;1), lower bound for the desired move rate interval.
#' @param maxMR Numeric in (0;1), upper bound for the desired move rate interval.
#' @param downMult Numeric in (0:1), multiplication factor used to decrease jump size
#'     when move rate is too low.
#' @param upMult Numeric (>1, avoid 1/dowMult) multiplication factor used to increase
#'     jump size when move rate is too high.
#' @param fileName Character, name of MCMC file (|!| name of the file only, not full path).
#' @param useSpeedUp Logical, enable all computational tricks to optimize speed?
#'     Should always be .true., except if one wishes to verify the equivalence speedUp/no speedUp.
#' @param jumpFileName Character, name of file reporting MCMC jump sizes at each adaptation
#'     (|!| name of the file only, not full path). No reporting if empty.
#' @param moveRateFileName Character, name of file reporting MCMC move rates at each adaptation
#'     (|!| name of the file only, not full path). No reporting if empty.
#' @return An object of class 'mcmc'.
#' @examples
#' m <- mcmc()
#' @export
mcmc<-function(Nsim=10000,Nadapt=50,stopAdapt=5000,
               Nburn=0,Nslim=1,minMR=0.1,maxMR=0.5,
               downMult=0.9,upMult=1.1,
               fileName='MCMC.txt',useSpeedUp=TRUE,
               jumpFileName='',moveRateFileName=''){
  o<-new_mcmc(Nsim,Nadapt,stopAdapt,Nburn,Nslim,minMR,maxMR,downMult,upMult,
              fileName,useSpeedUp,jumpFileName,moveRateFileName)
  return(validate_mcmc(o))
}

#***************************************************************************----
# toString function ----
#' mcmc to string
#'
#' Convert an object of class 'mcmc' into a ready-to-write vector of string
#'
#' @param x mcmc object, object to be converted.
#' @param ... Optional arguments.
#' @return A string ready to be printed or written.
#' @examples
#' toString(mcmc())
#' @export
toString.mcmc<-function(x,...){
  value=list(x$Nsim,x$Nadapt,x$stopAdapt,x$Nburn,x$Nslim,
             x$minMR,x$maxMR,x$downMult,x$upMult,
             x$fileName,x$useSpeedUp,x$jumpFileName,x$moveRateFileName)
  comment=c(
    'Nsim, total number of iterations',
    'Nadapt, adaptation period: jump sizes are increased/decreased every Nadapt iterations to comply with the desired moving rates',
    'stopAdapt, no more adaptation after iteration number stopAdapt',
    'Nburn, number of inital iterations that will not be written to file',
    'Nslim, only one iteration very Nslim will be written to file',
    'minMR, lower bound for the desired move rate interval',
    'maxMR, upper bound for the desired move rate interval',
    'downMult, (<1) multiplication factor used to decrease jump size when move rate is too low',
    'upMult, (>1, avoid 1/dowMult) multiplication factor used to increase jump size when move rate is too high',
    'fileName, name of MCMC file (|!| name of the file only, not full path)',
    'useSpeedUp, logical, enable all computational tricks to optimize speed. Should always be .true., except if one wishes to verify the equivalence speedUp/no speedUp ',
    'name of file reporting MCMC jump sizes at each adaptation (|!| name of the file only, not full path). No reporting if empty.',
    'name of file reporting MCMC move rates at each adaptation (|!| name of the file only, not full path). No reporting if empty.'
  )
  txt<-toString_engine(value,comment)
  return(txt)
}

#***************************************************************************----
# is function ----
#' mcmc tester
#'
#' Is an object of class 'mcmc'?
#'
#' @param o Object, an object.
#' @return A logical equal to TRUE if class(o)== 'mcmc', FALSE otherwise.
#' @keywords internal
is.mcmc<-function(o){
  return(class(o)=='mcmc')
}

#***************************************************************************----
# internal constructor ----
new_mcmc<-function(Nsim,Nadapt,stopAdapt,Nburn,Nslim,minMR,maxMR,downMult,upMult,
                   fileName,useSpeedUp,jumpFileName,moveRateFileName){
  stopifnot(is.numeric(Nsim))
  stopifnot(is.numeric(Nadapt))
  stopifnot(is.numeric(stopAdapt))
  stopifnot(is.numeric(Nburn))
  stopifnot(is.numeric(Nslim))
  stopifnot(is.numeric(minMR))
  stopifnot(is.numeric(maxMR))
  stopifnot(is.numeric(downMult))
  stopifnot(is.numeric(upMult))
  stopifnot(is.character(fileName))
  stopifnot(is.logical(useSpeedUp))
  stopifnot(is.character(jumpFileName))
  stopifnot(is.character(moveRateFileName))
  o <- list(Nsim=Nsim,Nadapt=Nadapt,stopAdapt=stopAdapt,Nburn=Nburn,
            Nslim=Nslim,minMR=minMR,maxMR=maxMR,
            downMult=downMult,upMult=upMult,
            fileName=fileName,useSpeedUp=useSpeedUp,
            jumpFileName=jumpFileName,moveRateFileName=moveRateFileName)
  class(o) <- 'mcmc'
  return(o)
}

#***************************************************************************----
# validator ----
validate_mcmc<-function(m){
  # Nsim: > 0
  if(m$Nsim <= 0){stop("`Nsim` should be strictly positive",call.=FALSE)}
  # Nadapt: > 0, <= Nsim
  if(m$Nadapt <= 0){stop("`Nadapt` should be strictly positive",call.=FALSE)}
  if(m$Nadapt > m$Nsim){warning("`Nadapt` > `Nsim`; `Nadapt`=`Nsim` is used",call.=TRUE);m$Nadapt <- m$Nsim}
  # stopAdapt: > 0, <= Nsim
  if(m$stopAdapt <= 0){stop("`stopAdapt` should be strictly positive",call.=FALSE)}
  if(m$stopAdapt > m$Nsim){warning("`stopAdapt` > `Nsim`; `stopAdapt`=`Nsim` is used",call.=TRUE);m$stopAdapt <- m$Nsim}
  # Nburn: >= 0, < Nsim
  if(m$Nburn < 0){stop("`stopAdapt` should be positive",call.=FALSE)}
  if(m$Nburn >= m$Nsim){warning("`Nburn` >= `Nsim`; `Nburn` = 0 is used",call.=TRUE);m$Nburn <- 0}
  # Nslim: > 0, < Nsim
  if(m$Nslim <= 0){stop("`Nslim` should be strictly positive",call.=FALSE)}
  if(m$Nslim > m$Nsim){warning("`Nslim` > `Nsim`; `Nslim` = 1 is used",call.=TRUE);m$Nslim <- 1}
  # minMR: in 0-1
  if(m$minMR < 0 | m$minMR > 1){stop("`minMR` should be in [0;1]",call.=FALSE)}
  # maxMR: in 0-1 and >= minMR
  if(m$maxMR < 0 | m$maxMR > 1){stop("`maxMR` should be in [0;1]",call.=FALSE)}
  if(m$maxMR < m$minMR){stop("`maxMR` should be larger than `minMR`",call.=FALSE)}
  # downMult: in (0-1]
  if(m$downMult <= 0 | m$downMult > 1){stop("`downMult` should be in (0;1]",call.=FALSE)}
  # upMult: >=1 , > downMult, avoid 1/downmult
  if(m$upMult < 1){stop("`upMult` should be larger than 1",call.=FALSE)}
  if(m$upMult <= m$downMult){stop("`upMult` should be larger than `downMult`",call.=FALSE)}
  if(m$upMult == 1/m$downMult){warning("avoid `upMult` = 1/`downMult` (oscillating adaptation)",call.=TRUE)}
  return(m)
}

