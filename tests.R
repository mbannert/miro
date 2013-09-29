tlist <- as.list(1:1000)
for (i in 1:length(tlist)){
  
  tlist[[i]] <- ts(rnorm(1:100),start=c(1999,2),frequency=4)
}
names(tlist) <- paste("key",1:1000,sep="_")

t1C <- add_key_to_listed_obj(tlist,names(tlist))
attributes(t1C[[1]])




`lapply.attr<-` <- 
  function() 
    lapply(tlist, 'attr<-', which='testkey', value='myStaticName')

for.setattr <- function() 
  for (i in seq_along(tlist)) 
    setattr(tlist[[i]], name = 'testkey', value = 'myStaticName')


require(microbenchmark)
require(data.table)
res1 <- microbenchmark(`lapply.attr<-`())
res3 <- microbenchmark(for.setattr())
res2 <- microbenchmark(add_key_to_listed_obj(tlist,names(tlist)))

