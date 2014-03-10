ts1 <- ts(rnorm(1:100),start=c(1993,4),frequency=4)
ts2 <- ts(rexp(1:100),start=c(1993,4),frequency=4)

attr(ts1,"key") <- "123"

a <- ts1
x <- cbind(ts1,a)

attributes(x[,1])


meta <- new.env()

mi <- function(tsObj,nm=NA_character_){
  if(!exists("meta")) {meta <- new.env()}
  if(!is.na(nm)){
    k <- nm
  } else {
    k <- deparse(substitute(tsObj))  
  }
  
  attr(tsObj,"key") <- k
  
  
  assign(k,tsObj,envir=.GlobalEnv)
  
  meta[[k]] <- dummy()
  meta[[k]]$key <<- k
  meta[[k]]$comment <<- "some test comment"
  
}


# setting object attributes with cpp
http://gallery.rcpp.org/articles/setting-object-attributes/
  
  
  dummy <- setRefClass("dummy", fields = list(key = "character",
                                              comment = "character",
                                              freq = "numeric"),
                       methods = list(
                         metaplot = function(){
                           plot(get(key,envir=.GlobalEnv),
                                main = comment)
                         }
                       ))


#http://stackoverflow.com/questions/13614399/how-can-i-use-attr-with-lapply

plot(ts1,main = meta$ts1$comment)
ts1
mi(ts1)
attributes(ts1)
mi(ts2)

meta$ts1$metaplot()

require(data.table)
data.table::setattr()

function(li,key="tskey"){
  stopifnot(!is.null(names(li)))
  for (i in seq_along(li)) 
    setattr(li[[i]], name = key, value = names(li[i]))
}



`%.%` <- function(o, a) attr(o, as.character(substitute(a)))
x <- 1
attr(x, "orz") <- 2
x%.%orz


meta$ts2

attributes(ts1,o)


attr()

# how does this work with a list of time series

lits <- list(a = ts1,b = ts2)

lits2 <- lapply(lits,function(x) x*100)
attributes(lits2$b)


le <- list2env(lits)
le$a
le$b

attributes(lits$b)

environment(lits$b)
?environment
env.profile(meta)

?setNames

lapply(seq_along(lits),function(x,i,nms){
  mi(x[[i]],nms[i])
},nms = c("ne","mo"), x = lits)

undebug(mi)
attributes(lits$a)
attr(lits$a,"key") <- "testkey"

attributes(lits$a)