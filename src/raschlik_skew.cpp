#include <Rcpp.h>
using namespace Rcpp;


// [[Rcpp::export]]
double raschlik_skew(NumericVector d,
                IntegerMatrix X, 
                NumericVector bmax, 
                NumericVector a, 
                IntegerVector np,
                double nu){
  int n = X.nrow();
  int p = X.ncol();
  double sumai = 0.0;
  double suma  = 0.0;
  double spump = 0.0;
  double bdj   = 0.0;
  double ebdj  = 0.0;
  double eta   = 0.0;
  double psi   = 0.0;
  double dn    = 0.0;
  double pn    = 0.0;
  
  double sk   = 0.99527 * (1 - exp( -nu ))/(1 + exp( -nu ));
  double r    = sqrt(2.0 / M_PI);
  double s    = pow(2.0/(4.0 - M_PI), 1.0/3.0);
  double xi   = -s * (R::sign(sk)) * pow(std::abs(sk), 1.0/3.0);
  double w    = sqrt(1 + pow(xi, 2));
  double lbd  = -xi / sqrt(pow(r, 2) + (pow(r, 2) - 1) * pow(xi, 2));

  for (int i = 0; i < n; ++i){
    for (int j = 0; j < p; ++j){
      bdj    = a[j]*(bmax[i]-d[j]);
    	ebdj   = exp(bdj);
      sumai += X(i,j)*bdj - log1p(ebdj);
      spump += pow(a[j],2)*ebdj/pow(1+ebdj,2);
    }
  eta   = (bmax[i] - xi)/w;
  dn    = R::dnorm(eta, 0.0, 1.0, 0);
  pn    = R::pnorm(lbd*eta, 0.0, 1.0, 1, 0);
  psi   = R::dnorm(lbd*eta, 0.0, 1.0, 0)/pn;
  suma += np[i]*(sumai + log(2) - log(w) + log(dn) + log(pn) - 0.5*log(spump - 1/pow(w,2) - pow(lbd/w, 2)*psi*(eta - psi)));
  spump = 0;
  sumai = 0;
  }
  return suma;
}
