Rcpp::sourceCpp("src/test.cpp")
Rcpp::sourceCpp("src/rcpp_hello_world.cpp")

x <- set_attr(x,"muuuu")

library(microbenchmark)
library(data.table)
myList <- as.list(1:10000)

`lapply.attr<-` <- 
  function() 
    lapply(myList, 'attr<-', which='myname', value='myStaticName')

`for.attr<-` <- 
  function() 
    for (i in seq_along(myList)) 
      attr(myList[[i]], 'myname') <- 'myStaticName'

lapply.setattr <- 
  function() 
    lapply(myList, setattr, name='myname', value='myStaticName')

for.setattr <- function() 
  for (i in seq_along(myList)) 
    setattr(myList[[i]], name = 'myname', value = 'myStaticName')

result <- microbenchmark(`lapply.attr<-`(), `for.attr<-`(), lapply.setattr(), for.setattr())
plot(result)

# tested this until 10M ! the 
# larger it gets the more substantial is the c++ gain. 51 vs 1300 ms !!
tlist <- as.list(1:100)

myList <- as.list(1:10000)

o <- rep("muuu",100)

ta <- function()a <- as.list(o)

require(microbenchmark)
res1 <- microbenchmark(`lapply.attr<-`())
res2 <- microbenchmark(fx(tlist,names(tlist)))
plot(res1)



tlist <- as.list(1:1000)
for (i in 1:length(tlist)){
  
  tlist[[i]] <- ts(rnorm(1:100),start=c(1999,2),frequency=4)
}
names(tlist) <- paste("key",1:1000,sep="_")

tlist[1]


attributes(tlist[[22]])


fx(tlist,names(tlist))
str(tlist)

attributes(tlist[[1]])


myList[[1]]+myList[[2]]

is.character(myList[[1]])

str(myList)




as.list()
tlist


?Rcpp::

myList[1:10]


`lapply.attr<-` <- 
  function() 
    lapply(tlist, 'attr<-', which='testkey', value='myStaticName')

for.setattr <- function() 
  for (i in seq_along(tlist)) 
    setattr(tlist[[i]], name = 'testkey', value = 'myStaticName')