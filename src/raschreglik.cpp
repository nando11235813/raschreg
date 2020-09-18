#include <Rcpp.h>
using namespace Rcpp;


// [[Rcpp::export]]
double raschreglik(NumericVector d,
                   NumericVector a,
                   IntegerMatrix X, 
                   NumericVector bmax, 
                   NumericMatrix Z, 
                   NumericVector beta) {
  int n  = X.nrow();
  int p  = X.ncol();
  int nk = Z.ncol();
  double suma = 0;
  double spump= 0;
  double bdj  = 0;
  double ebdj = 0;
  double zb   = 0;

  for (int i = 0; i < n; ++i){
    for (int k = 0; k < nk; ++k){
      zb += Z(i,k)*beta[k];
    }
    for (int j = 0; j < p; ++j){
      bdj    = a[j]*(bmax[i]- d[j]);
	    ebdj   = exp(bdj);
      suma  += X(i,j)*bdj - log1p(ebdj);
      spump += pow(a[j],2)*ebdj/pow(1+ebdj,2);
    }
    suma -= 0.5*pow(bmax[i] - zb, 2) + 0.5*log1p(spump);
    spump = 0;
    zb    = 0;
  }
  return suma;
}
