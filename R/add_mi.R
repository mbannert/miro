#' Helper Function to add meta information 
#' 
#' This functions adds meta information to the meta environment. If the meta
#' environment does not exist, a meta environment is created. This function
#' not be used as standalone because it creates meta objects without 
#' checking whether a corresponding data object exists.
#' The function .add_mi adds fixed meta information as well as a container 
#' for localized meta information by using the miro's reference classes.
#' 
#' @param x data object
#' @param meta_env character name of the meta environment, defaults to "meta"
#' @param srcname character name of the data source
#' @param legacy_key character name of a key that was used in another system
#' @param cmnt character comment
#' @param restrict character description of restrictions to the data
#' @param overwrite boolean defaults to FALSE
#' @author Matthias Bannert
#' @rdname add_mi_base 
#' @seealso \code{\link{add_mit}}
.add_mi <- function(x,meta_env_name = "meta", srcname = NA_character_,
                   legacy_key = NA_character_,cmnt = NA_character_,
                   restrict = NA_character_,overwrite = FALSE){
  # check if environment exists
  if(!exists(meta_env_name)){
  create_env(meta_env_name)  
  }
  
  # name of the meta object
  m_nm <- attributes(x)$mi_key
 
  # run updates if meta object does not exists or overwrite is TRUE
  if(!exists(m_nm,envir = get(meta_env_name),inherits = F) || overwrite == T ){
    meta_obj <- mi_ts()
    meta_obj$start(x,src = srcname,l_key = legacy_key,
                   comment = cmnt, restrictions = restrict)
    assign(m_nm,meta_obj,envir=get(meta_env_name))  
  } else {
    cat("Object already exists. Please choose another 
key or set overwrite to TRUE.")  
  }
}


#' Add meta information to an R object 
#' 
#' This function adds a meta information object to the meta environment. If
#' the meta environment does not exist an environment is created. Also the
#' original data object is linked to the meta information by an 
#' 
#' @param x data object
#' @param key character key, defaults to NA_character_ which leads to using the 
#' name of the original object
#' @param meta_env character name of the meta environment, defaults to "meta"
#' @param srcname character name of the data source
#' @param legacy_key character name of a key that was used in another system
#' @param cmnt character comment
#' @param restrict character description of restrictions to the data
#' @param overwrite boolean defaults to FALSE
#' @author Matthias Bannert 
add_mi <- function(x,key=NA_character_, meta_env_name = "meta", srcname = NA_character_,
                    legacy_key = NA_character_,cmnt = NA_character_,
                    restrict = NA_character_,overwrite = FALSE){
  # add a key to an R Object
  # in order to link the object to
  # a meta object
  if(is.na(key)){
    key <- deparse(substitute(x))
  }
  
  x <- add_key(x,key)
  
  assign(key,x,envir=.GlobalEnv)
  cat("Key assigned to object. \n")
  
  # create meta object in separate environment
  .add_mi(x,meta_env_name = meta_env_name, srcname = srcname,
         legacy_key = legacy_key,cmnt = cmnt,
         restrict = restrict,overwrite = overwrite)
  
  cat("Meta information added. \n")
  
}
