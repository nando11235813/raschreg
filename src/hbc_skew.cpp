#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
double hbc_skew(IntegerVector x, 
           NumericVector d, 
           double b, 
           NumericVector a,
           double nu){
  double sk   = 0.99527 * (1 - exp( -nu ))/(1 + exp( -nu ));
  double r    = sqrt(2 / M_PI);
  double s    = pow(2.0/(4.0-M_PI), 1.0/3.0);
  double xi   = -s * (R::sign(sk)) * pow(std::abs(sk), 1.0/3.0);
  double w    = sqrt(1 + pow(xi, 2));
  double lbd  = -xi / sqrt(pow(r, 2) + (pow(r, 2) - 1) * pow(xi, 2));
  
  double dn   = 0;
  double pn   = 0;
  double eta  = 0;

  int nj = x.size();
  double suma = 0;
  double bdi  = 0;
  for (int j = 0; j < nj; ++j){
    bdi   = a[j]*(b - d[j]);
    suma += x[j]*bdi - log1p(exp(bdi));
  }
  eta   = (b - xi)/w;
  dn    = R::dnorm(eta, 0.0, 1.0, 1);
  pn    = R::pnorm(lbd*eta, 0.0, 1.0, 1, 1);
  suma += log(2) - log(w) + dn + pn;
  return suma;
}
