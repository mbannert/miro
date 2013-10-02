add_key <- function(x,nm=NA){
  
  if(is.na(nm)){
    k <- deparse(substitute(x))
    # stop if object looks its nested
    # e.g. list or environment
    if(grepl("\\$|@",k)){
      stop("Object is likely nested. Please use add_key_to_listed_obj().")
    }
  } else {
    k <- nm
  }
  
  attr(x,"mi_key") <- k
  x
  
}

