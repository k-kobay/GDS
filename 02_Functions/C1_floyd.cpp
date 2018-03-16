#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::plugins("cpp11")]]
// [[Rcpp::export]]
NumericMatrix rcpp_floyd(NumericMatrix M){
  for(int k=0; k<M.nrow(); ++k){
    for(int j=0; j<M.nrow(); ++j){
      for(int i=0; i<M.nrow(); ++i){
        double test = M(i,k) + M(k,j);
        if(M(i,j) > test){
          M(i,j) = test;
        }
      }
    }
  }
  return M;
}