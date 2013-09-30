
#' @export
mi4ts <- setRefClass("mi4ts",fields = list(ts_key = "mi4tskey",
                                           ts_data = "data.frame",
                                           ts_index = "Date",
                                           ts_frequency = "numeric",
                                           md_generated_on = "character",
                                           md_generated_by = "character",
                                           md_legacy_key = "character",
                                           md_source = "character",
                                           md_comment = "character",
                                           md_restrictions = "character",
                                           md_localized_meta = "metaLocalized"),
                     methods = list(
                       #' add comments to this object
                       #' 
                       #' test
                       #' 
                       #' @method
                          #' @author Matthias Bannert
                          addComment = function(cmnt){
                            md_comment <<- cmnt
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
                            md_legacy_key <<- key
                            md_source <<- src
                          },
                          addRestrictions = function(restrict){
                            md_restrictions <<- restrict
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
                              ex <- (names(kvp) %in% names(md_localized_meta$labels[[lang]]))
                              # new information
                              ni <- kvp[!ex]
                              md_localized_meta$labels[[lang]] <<- c(md_localized_meta$labels[[lang]],ni)
                              
                            } else {
                              # append language
                              md_localized_meta$languages <<- c(md_localized_meta$languages,lang)
                              md_localized_meta$labels[[lang]] <<- kvp
                            }
                          },
                          getMeta = function(lang="de"){
                            md_localized_meta$labels[[lang]]
                          },
                          ggplot = function(...){
                            g <- ggplot2::ggplot(ts_data,ggplot2::aes(Date,value),...)
                            g + ggplot2::geom_point() + ggplot2::geom_line() +
                              ggplot2::ylab(ts_key$fullKey)
                          },
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
                              
                              # ts data
                              data <- as.matrix(tsObj)
                              data <- data.frame(Date=.zoolike.Date.convert(tsObj),
                                                 value=data,stringsAsFactors=F)
                              
                              ts_data <<- as.data.frame(data)
                              ts_index <<- .zoolike.Date.convert(tsObj)
                              
                              ts_frequency <<- frequency(tsObj)
                              
                              # meta information on users and time
                              md_generated_on <<- as.character(Sys.Date())
                              md_generated_by <<- Sys.getenv("USER")
                              
                              # get source information from key
                              md_source <<- ts_key$src
                              
                              # comments and restrictiions
                              md_comment <<- comment
                              md_restrictions <<- restrictions
                              
                              
                            } else {
                              stop("Input is not of valid class. Only ts and zoo are supported.")
                            }
                          },
                          show = function(){
                            methods::show(ts_key)
                            methods::show(head(ts_data))
                            if(nrow(ts_data) != 0){
                              cat("Showing 6 out of",nrow(ts_data),
                                  "rows. Access $ts_data to get the entire data.frame. \n")
                              cat("Use $getMeta() to access localized meta information: ",
                                  md_localized_meta$languages,"\n") 
                            }
                            if(!(length(md_restrictions) == 0)){
                              cat("!! DATASET IS RESTRICTED:",md_restrictions)
                              cat("\n")
                            }
                          },
                          showAll = function(){
                            show()
                            cat("Index:")
                            methods::show(ts_index)
                            cat("\n")
                            cat("Frequency: ")
                            methods::show(ts_frequency)
                            cat("\n")
                            cat("Meta information added on: ")
                            methods::show(md_generated_on)
                            cat("\n")
                            cat("last editor:")
                            methods::show(md_generated_by)
                            cat("\n")
                            cat("Comments: ")
                            methods::show(md_comment)
                            cat("\n")
                            cat("RESTRICTIONS: ")
                            methods::show(md_restrictions)
                          },
                          tsOut = function(){
                            s <- c(as.numeric(strftime(ts_index,"%Y")[1]),
                                   as.numeric(strftime(ts_index,"%m")[1])
                            )
                            out <- ts(ts_data$value,start = s,frequency = ts_frequency)
                            out
                          },
                          zooOut = function(){
                            z <- zoo::zoo(ts_data$value,
                                          zoo::as.yearqtr(ts_data$Date))
                            z
                          })
)
