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
RObject add_key_to_listed_obj (List x, CharacterVector y){
  List out = x;
  int n = x.size();
  
  for(int i = 0; i < n; ++i) {
    RObject el = out[i];
    el.attr("mi_key") = y;
  }
 
  return out;
}