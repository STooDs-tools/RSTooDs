#*******************************************************************************
#' Write value-comment pairs into string vector
#' @param val List, object/list of values to be printed
#' @param comment Character vector, associated comments (same length as val)
#' @param addQuote Logical, ass double quotes to strings?
#' @return A character vector, ready to be printed of written to file
#' @keywords internal
toString_engine<-function(val,comment,addQuote=T){
  n=length(val)
  txt=vector("character",n)
  for (i in 1:n){
    foo=val[[i]]
    # add double quotes to strings
    if(is.character(foo) & addQuote){foo=addQuotes(foo)}
    # transform R logicals to Fortran logicals
    if(is.logical(foo)){foo=paste(ifelse(foo,'.true.','.false.'))}
    # stitch values and comment
    if(is.null(comment[i])){
      txt[i]=paste(foo,collapse=',')
    } else {
      # format foo to string with no scientific notation
      foo=format(foo,scientific=FALSE,trim=TRUE)
      # collapse foo
      foo=paste(foo,collapse=',')
      # add trailing white spaces
      nw=max(58-nchar(foo),0)
      ws=paste0(rep(' ',nw),collapse='')
      foo=paste0(foo,ws)
      # add comment
      txt[i]=paste(foo,'!',comment[i])
    }
  }
  return(txt)
}

#*******************************************************************************
#' Add double quotes to a string or to each element of a string vector
#' @param txt Character vector
#' @return The double-quoted character vector
#' @keywords internal
addQuotes<-function(txt){
  n=length(txt)
  out=txt
  for(i in 1:n){out[i]=paste0('"',txt[i],'"')}
  return(out)
}

#*******************************************************************************
#' Write a character vector (typically output of toString() to file)
#' @param txt Character vector, text to be written
#' @param dir Character, directory where file is written. The directory is
#'   created if needed. dir should end with a path seperator (not checked here).
#' @param fname Character, file name
#' @return Nothing
#' @keywords internal
quickWrite <- function(txt,dir,fname){
  if(!dir.exists(dir)){dir.create(dir,recursive=T)}
  file=file.path(dir,fname)
  utils::write.table(matrix(txt, ncol = 1, nrow = length(txt)), file = file,
              row.names = FALSE, col.names = FALSE, quote = FALSE)
}

#*******************************************************************************
#' Write a data frame to file)
#' @param DF data frame to be written
#' @param file Character, file (full path)
#' @return Nothing
#' @keywords internal
quickWriteDF <- function(DF,file){
  dir=dirname(file)
  if(!dir.exists(dir)){dir.create(dir,recursive=T)}
  utils::write.table(DF,sep='\t',quote=FALSE,file=file,row.names=FALSE)
}

#*******************************************************************************
#' Check a formula make sense within a given namespace
#' @param f Character string, formula
#' @param namespace Character vector, namespace (i.e. names of known variables)
#' @return A list with the following fields:
#' \describe{
#'   \item{ok}{Does the formula make sense?}
#'   \item{mess}{message}
#' }
#' @keywords internal
checkFormula<-function(f,namespace){
  # initialise
  ok=TRUE;mess='';unknown=c()
  # Get variables used in the formula
  vars <- all.vars(parse(text=f))
  # Check all variables are in the namespace
  n <- length(vars)
  for(i in 1:n){
    if(!vars[i]%in%namespace){unknown=c(unknown,vars[i])}
  }
  # build result list
  ok=(length(unknown)==0)
  if(!ok){
    mess=paste0('unknown variable used in formula: ',
                paste0(unknown,collapse=','))
  }
  return(list(ok=ok,mess=mess))
}

#*******************************************************************************
#' Run an executable file
#' @param exedir directory of the executable
#' @param exename name of the executable
#' @return Nothing
#' @keywords internal
runExe<-function(exedir,exename){
  saveWD <- getwd(); # remember current working dir
  setwd(exedir) # need to set working dir to the exe dir
  os=Sys.info()['sysname'] # determine OS
  cmd=ifelse(os=='Windows',
             paste0(exename,'.exe'), # Windows command
             paste0('./',exename) # Linux command
             )
  system2(cmd, stdout = "") # run exe
  setwd(saveWD) # move back to initial working directory
}
