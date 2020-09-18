#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
double h1plgc(double b,
       IntegerVector x, 
       NumericVector d, 
       NumericVector g){
  int p = x.size();
  double suma = 0;
  double ebdj  = 0;
  for (int j = 0; j < p; ++j){
    ebdj   = exp(b-d[j]);
    suma += x[j]*log(g[j] + ebdj) + (1-x[j])*log1p(-g[j]) - log1p(ebdj);
  }
  suma -= 0.5*pow(b,2);
  return suma;
}

