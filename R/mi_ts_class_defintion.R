#' @export
mi_ts <- setRefClass("mi_ts",fields = list(ts_mi_key = "character", # change this back to "mi_ts_key"
                                           ts_index = "Date",
                                           ts_frequency = "numeric",
                                           ts_edited_on = "POSIXct",
                                           ts_edited_by = "character",
                                           ts_legacy_key = "character",
                                           ts_source = "character",
                                           ts_comment = "character",
                                           ts_restrictions = "character",
                                           ts_localized_meta = "character" # change back to  "metaLocalized"
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
                          show = function(){
                            methods::show(ts_mi_key)
                            methods::show(ts_index)
                            methods::show(ts_frequency)
                            methods::show(ts_edited_by)
                            methods::show(ts_edited_on)
                            if(!is.na(ts_legacy_key)) methods::show(ts_legacy_key)
                            if(!is.na(ts_source)) methods::show(ts_source)
                            if(!is.na(ts_comment)) methods::show(ts_comment)
                            if(!is.na(ts_restrictions)) methods::show(ts_restrictions)
                            methods::show(ts_localized_meta)
                          },
                          start = function(ts_obj,nm = NA_character_,l_key = character(),
                                           src = character(),
                                           comment = character(),
                                           restrictions = character()){
                            k <- attributes(ts_obj)$mi_key
                            ts_mi_key <<- k
                            ts_index <<- .zoolike.Date.convert(ts_obj)
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
