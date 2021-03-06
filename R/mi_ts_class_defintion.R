#' @export
mi_local <- setClass("mi_local",contains = "environment")
setMethod("show","mi_local",
          function(object) cat(ls(envir=object)))

#' @export
mi_ts <- setRefClass("mi_ts",fields = list(ts_mi_key = "character", 
                                           ts_frequency = "numeric",
                                           ts_edited_on = "POSIXct",
                                           ts_edited_by = "character",
                                           ts_legacy_key = "character",
                                           ts_source = "character",
                                           ts_comment = "character",
                                           ts_restrictions = "character",
                                           ts_localized_mi = "mi_local" 
                                           ),
                     methods = list(
                          add_comment = function(cmnt){
                            ts_comment <<- cmnt
                          },
                          add_legacy_key = function(leg_key){
                            ts_legacy_key <<-leg_key
                          },
                          add_source = function(src){
                           ts_source <<- src 
                          },
                          add_restrictions = function(restr){
                            ts_restrictions <<- restr
                          },
                          add_localized_mi = function(language_key,...,
                                                      meta_env_name = "meta",
                                                      overwrite = F
                                                      ){
                            # sanity checks
                            if(!is.character(language_key)) stop("language key must be a character.")
                            if(nchar(language_key) !=2) stop("languge key has to be exactly 2 characters long.")
                            
                            h <- hash::hash(...)
                            
                            # does this language already exist?
                            ex <- exists(language_key,
                                         ts_localized_mi,inherits=F)
                            
                            # create new lang or overwrite old entry
                            if(!ex || overwrite == T){
                              assign(language_key,h,envir=ts_localized_mi)
                              out <- paste("Translations in ",language_key,
                                           " added.",sep="")
                              out
                            } else {
                              h_update <- ts_localized_mi[[language_key]]
                              hash::.set(h_update,as.list(h))
                              assign(language_key,h_update,envir=ts_localized_mi)
                              out <- paste("Translations in ",language_key,
                                           " extended. Identical keys updated.",sep="")
                              out
                            }
                          },
                          show = function(){
                            cat("Time series key: \n")
                            methods::show(ts_mi_key)
                            cat("Suggested key format: Country.provider.source.level.selected_level.variable.item\n \n")
                            cat("Frequency:")
                            format(methods::show(ts_frequency))
                            cat("Last edit by:")
                            methods::show(ts_edited_by)
                            cat("Last edit on:")
                            methods::show(ts_edited_on)
                            if(!is.na(ts_legacy_key)){
                              cat("Legacy key:")
                              methods::show(ts_legacy_key)
                            } 
                            if(!is.na(ts_source)){
                              cat("Orgination:")
                              methods::show(ts_source)
                            } 
                            if(!is.na(ts_comment)){
                              cat("Notes:")
                              methods::show(ts_comment)
                            } 
                            if(!is.na(ts_restrictions)){
                              cat("Restrictions: ")
                              methods::show(ts_restrictions)
                            } 
                            if(length(ls(envir=ts_localized_mi)) != 0){
                              cat("Translated meta information available for: \n")
                              methods::show(ts_localized_mi)
                              cat("\n")
                              cat("Use the $ operator to access the respective language, e.g.: ts_localized_mi$en.")
                            } else {
                              cat("No localized meta information available.")                              
                            }
                          },
                          showAll = function(){
                            cat("Time series key: \n")
                            methods::show(ts_mi_key)
                            cat("Suggested key format: Country.provider.source.level.selected_level.variable.item\n \n")
                            cat("Frequency:")
                            format(methods::show(ts_frequency))
                            cat("Last edit by:")
                            methods::show(ts_edited_by)
                            cat("Last edit on:")
                            methods::show(ts_edited_on)
                            cat("Legacy key:")
                            methods::show(ts_legacy_key)
                            cat("Orgination:")
                            methods::show(ts_source)
                            cat("Notes:")
                            methods::show(ts_comment)
                            cat("Restrictions: ")
                            methods::show(ts_restrictions)
                            if(length(ls(envir=ts_localized_mi)) != 0){
                              cat("Translated meta information available for: \n")
                              methods::show(ts_localized_mi)
                              cat("\n")
                              cat("Use the $ operator to access the respective language, e.g.: ts_localized_mi$en.")
                            } else {
                              cat("No localized meta information available.")
                            } 
                          },
                          start = function(ts_obj,nm = NA_character_,l_key = character(),
                                           src = character(),
                                           comment = character(),
                                           restrictions = character()){
                            k <- attributes(ts_obj)$mi_key
                            ts_mi_key <<- k
                            ts_frequency <<- frequency(ts_obj)
                            ts_edited_on <<- Sys.time()
                            ts_edited_by <<- Sys.getenv('USER')
                            ts_legacy_key <<- l_key
                            ts_source <<- src
                            ts_comment <<- comment
                            ts_restrictions <<- restrictions
                          }
                          )
)

#' @export
result_set <- setRefClass("result_set",
                          fields = list(keys = "data.frame",
                                        selection = "numeric"),
                          methods = list(
                            add_series = function(mi_keys,vintage_keys = list(),
                                                  meta_env_name="meta"){
                              # sanity check of argument
                              if(!is.character(mi_keys)){
                                stop("mi_keys must be a vector
of character representation of the keys you want to add.")
                              }
                              # throw an error if there is no
                              # meta desc
                              if(!all(mi_keys %in% ls(envir=get(meta_env_name)))){
                                stop("Not all time series have meta information.")
                              }
                              
                              df <- data.frame(mi_key = mi_keys,stringsAsFactors=F)
                              # add vintage keys if list is not empty
                              if(length(vintage_keys) != 0){
                                if(all(unlist(vintage_keys) %in% 1:nrow(df))){
                                  df[unlist(vintage_keys),"vintage_key"] <- names(vintage_keys)
                                  keys <<- df  
                                } else {
                                  stop("result set cannot be generated. One of the vintage keys
                                     is out of bounds.")
                                }
                                # go on if list of vintage keys is empty
                              } else {
                                keys <<- df
                              } 
                            },
                            add_selection = function(rows){
                              if(!is.numeric(rows)){
                                stop("selection has to be row number in the result_set data.frame")
                              }
                              # check whether selection fits 
                              if(!all(rows %in% 1:nrow(keys))){
                                stop("selection not in range.")
                              }
                              selection <<- rows
                              
                            },
                            # default show method shows only selected values
                            show = function(){
                              if(length(selection) == 0){
                                sel <- 1:nrow(keys)  
                              } else {
                                sel <- selection
                              }
                              methods::show(keys[sel,])
                            }
                            
                          )
                          
)



