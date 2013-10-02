add_mi <- function(x,meta_env_name = "meta"){
  if(!exists(meta_env_name)){
  create_env(meta_env_name)  
  }
  
  meta_obj <- mi_ts()
  assign(attributes(x)$mi_key,meta_obj,envir=get(meta_env_name))
  
}

create_env <- function(nm){
  e <- new.env()
  assign(nm,e,envir = .GlobalEnv)
  cat("Meta environment",nm,"created.")
}
