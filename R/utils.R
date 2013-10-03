#' Shortcut to create an Environment within the .GlobalEnv
#' 
#' This function creates an environment in the .GlobalEnv.
#' Beware, it does overwrite existing environments.
#' 
#' @param nm character string name of the environment to be 
#' created
#' @author person("Matthias 'Bunny'", "Bannert", role = c("aut","cre"),email="matthias.bannert@gmail.com")
create_env <- function(nm){
  e <- new.env()
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
#' 
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


