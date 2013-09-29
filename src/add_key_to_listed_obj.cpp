#include <Rcpp.h>
using namespace Rcpp;

//' Add Meta Information Key to Every Element of List
//' 
//' Description follows.
//' 
//'  @param x un-nested list
//'  @param y character vector of the same length as x
//'
// [[Rcpp::export]]
RObject add_key_to_listed_obj(List x, CharacterVector y){
  List out = clone(x);
  int n = x.size();
  CharacterVector val;
  
  for(int i = 0; i < n; ++i) {
    RObject el = out[i];
    val = y[i];
    el.attr("testkey") = val; 
  }
 
  return out;
}
 
/*** R

tlist <- as.list(1:1000)
for (i in 1:length(tlist)){
  
  tlist[[i]] <- ts(rnorm(1:100),start=c(1999,2),frequency=4)
}
names(tlist) <- paste("key",1:1000,sep="_")

tlist.mod <- add_key_to_listed_obj(tlist,names(list))
 
*/