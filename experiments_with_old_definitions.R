#' Get one or multiple key from database by chunks
#' 
#' This function reads all keys that qualify for the like condition
#' specified in the arguments of this function. 
#' 
#' @param ... chunks of class character that should be use to narrow
#' down selected keys
#' @param conn a postgreSQL connection object
#' @author Matthias Bannert
#' @export
#' @seealso \code{\link{boots2db} and \code{\link{bootsKey}}}
getKeysFromDb <- function(...,conn=con,
                          relation="timeseries_main"){
  like <- unlist(list(...))
  stopifnot(is.character(like))
  like <- addTag(like,"'%","%'")
  like <- paste(like,collapse=" AND ts_key LIKE ")
  statement <- paste("SELECT ts_key FROM ",relation,
                     " WHERE ts_key LIKE ",like,sep="")
  res <- dbGetQuery(conn,statement)
  res <- as.matrix(res)
  res
}


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
                              if(length(vintage_keys) != 0 & all(unlist(vintage_keys) %in% 1:nrow(df))){
                                df[unlist(vintage_keys),"vintage_key"] <- names(vintage_keys)
                                keys <<- df
                              } else {
                                stop("result set cannot be generated. One of the vintage keys
                                     is out of bounds.")
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

ts1 <- ts(rnorm(1:100),start=c(1982,1),frequency=4)
ts2 <- ts(rnorm(1:100),start=c(1985,1),frequency=4)
ts3 <- ts(rnorm(1:100),start=c(1985,1),frequency=4)
ts4 <- ts(rnorm(1:100),start=c(1985,1),frequency=4)

xx <- list(run1=1,run3=4)
unlist(xx)

tset$keys[unlist(xx),"vintage_key"] <- names(xx)
nrow(tset$keys)
ifelse()


matrix(c("ts1","ts2",T,F),ncol=2)

tset <- result_set()
tset$add_series(c("ts1","ts2","ts3","ts4"),list(run3123=3,run3=3))
tset$add_selection(c(1,2,4))
tset$selection


c(1,2,3) %in% 1:nrow(tset$keys)

tset$selection

require(miro)
add_mi(ts1)
add_mi(ts2)
add_mi(ts3)
add_mi(ts4)


length(list(run3123=2,run3=45)) != 0 & all(unlist(list(run3123=2,run3=45)) %in% 1:nrow(tset$keys))


all(c("ts1","ts2","ts3","ts4") %in% ls(envir=meta))






resultKeys <- setRefClass("resultKeys",
                          fields = list(keys = "matrix",
                                        selected = "numeric"),
                          methods = list(
                            populate = function(...){
                              'Gets initial set of Keys from
                              data base.
                              '
                              keys <<- getKeysFromDb(...)
                              selected <<- as.numeric()
                            },
                            select = function(...){
                              mx <- nrow(keys)
                              sel <- sort(unlist(list(...)))
                              stopifnot(range(sel)[2] <= mx)
                              selected <<- sel                              
                            },
                            show = function(){
                              if(length(numeric) > 0){
                                cat("Keys of selected time series, object of class",
                                    classLabel(class(.self)),": \n")
                                methods::show(keys[selected])
                                cat("To see all keys from this query call showAll.")  
                              } else {
                                cat("Keys of selected time series, object of class",
                                    classLabel(class(.self)),": \n")
                                methods::show(keys)
                                cat("No selections made yet.")
                              }
                            },
                            showAll = function(){
                              cat("Keys of selected time series, object of class",
                                  classLabel(class(.self)),": \n")
                              methods::show(keys)
                              cat("The following rows are currently selected: \n")
                              methods::show(selected)
                            }
                            # definitely gotta a save selection method here.  
                          ))

is.resultKeys <- function(x) inherits(x,"resultKeys")

test <- resultKeys()
test$populate("DLU1","NEG")
test$select(1,2)
test$showAll()
pastetest$keys[test$selected]


getIndex <- function(Obj) Obj@ts_index

t1 <- db2boots(tskey=test$keys[1])
t2 <- db2boots(tskey=test$keys[2])
t2a <- db2boots(tskey=test$keys[2])
t3 <- list(t1,t2,t2a)



t5 <- lapply(t3,getIndex)

min(t5[[3]])

identical(t5[[1]],t5[[2]],t5[[3]][1])
all(t5)

resultSet <- setRefClass("resultSet",fields = list(bootsList = "list",
                                                   set = "data.frame"),
                         methods = list(
                           queryByKeys = function(x,...){
                             stopifnot(is.resultKeys(x))
                             li <- lapply(x$keys[x$selected],db2boots)
                             names(li) <- x$keys[x$selected]
                             bootsList <<- li
                             set <<- bootsList2df(li)
                           },
                           show = function(){
                             methods::show(set)
                             cat("Only showing data.frame representation. Access object$bootsList for all information.")
                           },
                           export2STATA = function(f="stata_out.dta"){
                             stopifnot(nrow(set) > 0)
                             foreign::write.dta(set,file=f)
                             cat("File written to ",getwd(),"f")
                           }
                           
                           # should also be able to generate result
                           # sets without database... 
                         )
)

getMeta(out$bootsList$CH.KOF.DLU.GROUP.DLU1.F_948.NEGATIVE,"en")$title

out <- resultSet()
out$export2STATA()
out$queryByKeys(test)
plot(out$bootsList$CH.KOF.DLU.GROUP.DLU1.F_948.NEGATIVE@.Data,type="l")




bootsList2df <- function(li){
  indices <- lapply(li,getIndex)
  nm <- names(li)
  data <- as.data.frame(lapply(li,getDataPart))
  colnames(data) <- nm
  if(compareList(indices)){
    d <- indices[[1]]
    df <- data.frame(Date = as.character(d),
                     data,stringsAsFactors = FALSE)
    df
  } else
    cat ("Time series of different length are not supported at the moment.")
  
}


compareList <- function(li){
  stopifnot(length(li) > 1)
  l <- length(li)
  res <- lapply(li[-1],function(X,x) identical(X,x),x=li[[1]])
  res <- all(unlist(res))
  res
}



compareList(test_false)






a <- out$bootsList$CH.KOF.DLU.GROUP.DLU1.F_948.NEGATIVE@ts_index
b <- out$bootsList$CH.KOF.DLU.GROUP.DLU1.F_948.NEGATIVE_X12_D11@ts_index
identical(a,b)
class(a)
class(as.character(a))

test
bootsList2df(out$bootsList)
getIndex(out$bootsList$CH.KOF.DLU.GROUP.DLU1.F_948.NEGATIVE)

t4 <- lapply(t3,getDataPart)
as.data.frame(t4)
