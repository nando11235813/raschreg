#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
double huc(IntegerVector x, 
           NumericVector d,
           NumericVector a,
           double b,
           NumericVector z, 
           NumericVector beta) {
  int nj = x.size();
  int nk = z.size();
  double suma = 0;
  double bdi  = 0;
  double zb   = 0;
  for (int k = 0; k < nk; ++k){
    zb += z[k]*beta[k];
  }
  for (int i  = 0; i < nj; ++i){
    bdi   = a[i]*(b - d[i]);
    suma += x[i]*bdi - log1p(exp(bdi));
  }
  suma -= 0.5*pow(b - zb,2);
  return suma;
}

