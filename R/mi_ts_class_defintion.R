#' @export
mi_ts <- setRefClass("mi_ts",fields = list(ts_key = "mi_ts_key",
                                           ts_index = "Date",
                                           ts_frequency = "numeric",
                                           ts_generated_on = "character",
                                           ts_generated_by = "character",
                                           ts_legacy_key = "character",
                                           ts_source = "character",
                                           ts_comment = "character",
                                           ts_restrictions = "character",
                                           ts_localized_meta = "metaLocalized"),
                     methods = list(
                          addComment = function(cmnt){
                            ts_comment <<- cmnt
                          },
                          addKey = function(cntry,prov,src,level,
                                            slevel,var,itm){
                            'adds a mi4ts time series key.'
                            ts_key$country <<- cntry
                            ts_key$provider <<- prov
                            ts_key$src <<- src
                            ts_key$level <<- level
                            ts_key$selected_level <<- slevel
                            ts_key$variable <<- var
                            ts_key$item <<- itm
                            ts_key$fullKey <<- paste(cntry,prov,src,level,
                                                     slevel,var,itm,sep=".")
                          },
                          addLegacy = function(key,src){
                            ts_legacy_key <<- key
                            ts_source <<- src
                          },
                          addRestrictions = function(restrict){
                            ts_restrictions <<- restrict
                          },
                          addLocalizedMeta = function(kvp){
                            # sanity checks
                            #  stopifnot(assertive::assert_is_list(kvp))
                            # stopifnot(assertive::assert_is_not_null(names(de)))
                            # make use of list name
                            lang <- deparse(substitute(kvp))
                            # check whether this language already has
                            # meta information
                            if((lang %in% md_localized_meta$languages)){
                              cat("Updated existing language.")
                              # check whether this fields already exist
                              ex <- (names(kvp) %in% names(ts_localized_meta$labels[[lang]]))
                              # new information
                              ni <- kvp[!ex]
                              ts_localized_meta$labels[[lang]] <<- c(ts_localized_meta$labels[[lang]],ni)
                              
                            } else {
                              # append language
                              ts_localized_meta$languages <<- c(ts_localized_meta$languages,lang)
                              ts_localized_meta$labels[[lang]] <<- kvp
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
                          populateFromWorkspace = function(tsObj,
                                                           nm = NA_character_,
                                                           comment = character(),
                                                           restrictions = character()){
                            if(is.zoo(tsObj) || is.ts(tsObj)){
                              
                              if(is.na(nm)){
                                nm <- deparse(substitute(tsObj)) 
                              }
                              # set Key
                              key <- mi4tskey()
                              key$setKey(nm)
                              ts_key <<- key
                              
                              ts_frequency <<- frequency(tsObj)
                              
                              # meta information on users and time
                              ts_generated_on <<- as.character(Sys.Date())
                              ts_generated_by <<- Sys.getenv("USER")
                              
                              # get source information from key
                              ts_source <<- ts_key$src
                              
                              # comments and restrictiions
                              ts_comment <<- comment
                              ts_restrictions <<- restrictions
                              
                              
                            } else {
                              stop("Input is not of valid class. Only ts and zoo are supported.")
                            }
                          }
                          )
)
