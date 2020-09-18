#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
double hbc(IntegerVector x, 
           NumericVector d, 
           double b, 
           NumericVector a){
  int nj = x.size();
  double suma = 0;
  double bdi  = 0;
  for (int j = 0; j < nj; ++j){
    bdi   = a[j]*(b - d[j]);
    suma += x[j]*bdi - log1p(exp(bdi));
  }
  suma -= 0.5*pow(b,2);
  return suma;
}
