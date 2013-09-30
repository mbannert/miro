metaLocalized <- setRefClass("metaLocalized", fields = list(languages = "character",
                                                            labels = "list"),
                             methods = list(
                               show = function(){
                                 if(length(languages) == 0 ) {cat("No localized meta information added yet.")} else
                                 {
                                   cat("Meta informations is available in:",languages,"\n") 
                                 }
                                 methods::show(labels)
                               })
)
