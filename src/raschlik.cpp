#include <Rcpp.h>
using namespace Rcpp;


// [[Rcpp::export]]
double raschlik(NumericVector d,
                IntegerMatrix X, 
                NumericVector bmax, 
                NumericVector a, 
                IntegerVector np){
  int n = X.nrow();
  int p = X.ncol();
  double sumai = 0;
  double suma  = 0;
  double spump = 0;
  double bdj   = 0;
  double ebdj  = 0;

  for (int i = 0; i < n; ++i){
    for (int j = 0; j < p; ++j){
      bdj    = a[j]*(bmax[i]-d[j]);
    	ebdj   = exp(bdj);
      sumai += X(i,j)*bdj - log1p(ebdj);
      spump += pow(a[j],2)*ebdj/pow(1+ebdj,2);
    }
    suma += np[i]*(sumai - 0.5*pow(bmax[i],2) - 0.5*log1p(spump));
    spump = 0;
    sumai = 0;
  }
  return suma;
}
