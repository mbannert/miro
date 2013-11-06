
ts1 <- ts(rnorm(1:100),start=c(1993,4),frequency=4)

rm(meta)



add_mi(ts1,srcname="chdlu.db",legacy_key="lalal")
attributes(ts1)

meta$ts1


class(meta)
meta$ts1

meta$ts1$showAll()

debug(meta$moooo$add_localized_mi)
meta$ts1$add_localized_mi("en",paste(LETTERS,letters,sep="_"),1:26)

rm(meta)

meta$ts1$ts_localized_mi$en

lookUp <- function(pattern){
  objs <- ls(envir=.GlobalEnv)
  hits <- lapply(objs,function(x) attr(get(x),"mi_key")) 
  
}

!unlist(lapply(h1,is.null))



meta$moooo$ggplot()

# gotta convert away from 
t0 <- ts1
# convert data frame
df <- data.frame(Date = .zoolike.Date.convert(t0),
                 value = as.numeric(t0))

require(ggplot2)
require(plyr)
g1 <- ggplot(data=df,aes(Date,value)) 
g1 + geom_point()


str(df)


# next things:
# better show method for mi_ts !
# add show all method

# slot based lookup with xsr
# be able to crawl all mi_keys in attributes of the globals environment. 




# maybe rather zoo like date convert based on original series 
# in plotting (and database functions)

