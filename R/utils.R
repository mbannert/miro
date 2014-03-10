#' Shortcut to create an Environment within the .GlobalEnv
#' 
#' This function creates an environment in the .GlobalEnv.
#' Beware, it does overwrite existing environments.
#' 
#' @param nm character string name of the environment to be 
#' created
#' @export
#' @author Matthias Bannert
create_env <- function(nm){
  e <- new("mi_local")
  assign(nm,e,envir = .GlobalEnv)
  cat("Meta environment",nm,"created.")
}

#' Zoo like Date Conversion
#' 
#' This function is taken from the zoo package. It is basically the
#' S3 method as.Date.numeric of the package zoo. It is used to turn
#' 2005.75 (3rd quarter of 2005) like date formats into dates
#' like 2005-07-01.
#'
#' @param x object of class ts
#' @rdname zooLikeDateConvert
#' @name zooLikeDateconvert
#' @export
#' @author Achim Zeileis, Gabor Grothendieck, Jeffrey A. Ryan,
#' Felix Andrews
.zoolike.Date.convert <- function (x, offset = 0, ...) 
{
  time.x <- unclass(time(x)) + offset
  if (frequency(x) == 1) 
    as.Date(paste(time.x, 1, 1, sep = "-"))
  else if (frequency(x) == 4) 
    as.Date(paste((time.x + 0.001)%/%1, 3 * (cycle(x) - 1) + 
                    1, 1, sep = "-"))
  else if (frequency(x) == 12) 
    as.Date(paste((time.x + 0.001)%/%1, cycle(x), 1, sep = "-"))
  else stop("unable to convert ts time to Date class")
}

#' Remove Orphaned Meta objects
#' 
#' tidy_meta_env deletes meta objects if there is no corresponding data object. 
#' This function is useful when R Objects are deleted and meta information was
#' not deleted. 
#' 
#' @param meta_env character name of the meta environment, defaults to 'meta'.
#' @param key character name of the key attribute, defaults to mi_key.
#' @param data_env environment that holds the data, defaults to .GlobalEnv.
#' @author Matthias Bannert
#' @export
tidy_meta_env <- function(meta_env="meta",key="mi_key",data_env=.GlobalEnv){
  # get mi keys of object that got one
  sapply(ls(),function(x) attr(get(x),key))
  
  # list all objects in the meta envir
  m_objs <- ls(envir=get(meta_env))
  # which objects are available in the meta env?
  av <- m_objs %in% sapply(ls(envir=data_env),function(x) attr(get(x),key))
  # delete those which are not available
  del <- m_objs[!av]
  rm(list = del,envir=get(meta_env))
  cat("Meta environment cleaned. \n")
  }


#' Wrap characters around another character
#' 
#' @export
wrap <- function(x,char="'"){
  paste(char,x,char,sep="")
}

#' Wrap opening and closing characters around another character
#' 
#' @export
add_tag <- function(x,open,close){
  paste(open,x,close,sep="")
}



