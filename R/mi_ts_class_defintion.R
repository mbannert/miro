#' @export
mi_ts <- setRefClass("mi_ts",fields = list(ts_mi_key = "character", # change this back to "mi_ts_key"
                                           ts_frequency = "numeric",
                                           ts_edited_on = "POSIXct",
                                           ts_edited_by = "character",
                                           ts_legacy_key = "character",
                                           ts_source = "character",
                                           ts_comment = "character",
                                           ts_restrictions = "character",
                                           ts_localized_mi = "environment" # change back to  "metaLocalized"
                                           ),
                     methods = list(
                          addComment = function(cmnt){
                            ts_comment <<- cmnt
                          },
#                           addKey = function(cntry,prov,src,level,
#                                             slevel,var,itm){
#                             'adds a mi4ts time series key.'
#                             ts_key$country <<- cntry
#                             ts_key$provider <<- prov
#                             ts_key$src <<- src
#                             ts_key$level <<- level
#                             ts_key$selected_level <<- slevel
#                             ts_key$variable <<- var
#                             ts_key$item <<- itm
#                             ts_key$fullKey <<- paste(cntry,prov,src,level,
#                                                      slevel,var,itm,sep=".")
#                           },
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
                          getMeta = function(lang="de"){
                            ts_localized_meta$labels[[lang]]
                          },
#                           ggplot = function(...){
#                             g <- ggplot2::ggplot(ts_data,ggplot2::aes(Date,value),...)
#                             g + ggplot2::geom_point() + ggplot2::geom_line() +
#                               ggplot2::ylab(ts_key$fullKey)
#                           },
                          show = function(){
                            methods::show(ts_mi_key)
                            methods::show(ts_frequency)
                            methods::show(ts_edited_by)
                            methods::show(ts_edited_on)
                            if(!is.na(ts_legacy_key)) methods::show(ts_legacy_key)
                            if(!is.na(ts_source)) methods::show(ts_source)
                            if(!is.na(ts_comment)) methods::show(ts_comment)
                            if(!is.na(ts_restrictions)) methods::show(ts_restrictions)
                            methods::show(ts_localized_mi)
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
#                             ts_localized_meta = "metaLocalized"
                            
                          }
                          )
)
