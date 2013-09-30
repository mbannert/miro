mi_ts_key <- setRefClass("mi_ts_key", fields = list(fullKey = "character",
                                                  country = "character",
                                                  provider = "character",
                                                  src = "character",
                                                  level = "character",
                                                  selected_level = "character",
                                                  variable = "character",
                                                  item = "character"),
                        methods = list(
                          setKey = function(...){
                            chunks <- list(...)
                            if(length(chunks) == 7) {
                              country <<- chunks[[1]]
                              provider <<- chunks[[2]]
                              src <<- chunks[[3]]
                              level <<- chunks[[4]]
                              selected_level <<- chunks[[5]]
                              variable <<- chunks[[6]]
                              item <<- chunks[[7]]
                              fullKey <<- paste(chunks[1:7],collapse=".")
                            } else {
                              if(length(chunks) == 1){
                                pts <- unlist(strsplit(unlist(chunks),"\\."))
                                if(length(pts) != 7){
                                  warning("String is not a valid key.")
                                  fullKey <<- paste(pts,collapse=".")
                                } else {
                                  country <<- pts[1]
                                  provider <<- pts[2]
                                  src <<- pts[3]
                                  level <<- pts[4]
                                  selected_level <<- pts[5]
                                  variable <<- pts[6]
                                  item <<- pts[7]
                                  fullKey <<- paste(pts[1:7],collapse=".")
                                }
                              } else {
                                stop("Argument cannot be turned into a valid key.")
                              }  
                            }
                            
                          },
                          show = function(){
                            cat("m4its time series key of pattern \n")
                            cat("country.provider.src.level.selected_level.variable.item: \n")
                            if(length(fullKey) == 0){
                              cat("No key selected yet.")
                            } else {
                              cat(fullKey)
                            }
                            cat("\n")
                          })
)
