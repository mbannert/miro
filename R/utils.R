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
