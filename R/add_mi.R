add_mi <- function(x,meta_env_name = "meta", srcname = NA_character_,
                   legacy_key = NA_character_,cmnt = NA_character_,
                   restrict = NA_character_,overwrite = FALSE){
  # check if environment exists
  if(!exists(meta_env_name)){
  create_env(meta_env_name)  
  }
  
  # name of the meta object
  m_nm <- attributes(x)$mi_key
 
  # run updates if meta object does not exists or overwrite is TRUE
  if(!exists(m_nm,envir = get(meta_env_name)) || overwrite == T ){
    meta_obj <- mi_ts()
    meta_obj$start(x,src = srcname,l_key = legacy_key,
                   comment = cmnt, restrictions = restrict)
    assign(m_nm,meta_obj,envir=get(meta_env_name))  
  } else {
    cat("Object already exists. Please choose another 
key or set overwrite to TRUE.")  
  }
  
  
}
