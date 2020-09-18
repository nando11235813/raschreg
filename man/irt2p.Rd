\name{irt2p}
\alias{irt2p}
\title{
Fitting Two Parameter Logistic Model
}
\description{
Estimate the two parameter logistic model approximating the marginal log-likelihood using Laplace approximation.
}
\usage{
irt2p(items,init=NULL)
}

\arguments{
  \item{items}{
    Numeric matrix (or data.frame) containing only zeroes and ones (J columns).
  }
  \item{init}{
    Optional numeric vector of initial values at which initialize numerical optimization (length 2*J).
  }
}
\details{
Optimizes \code{irt2plikLA} numericaly via \code{nlminb}. Rows containing at least one \code{NA} are removed from \code{items}. Standard errors of model parameters are approximated by inverting the observed information matrix.
}
\value{
  An object of class \code{rasch} is a list containing the following componentes:
\item{call}{The matched call}
\item{coef}{A named vector of coefficients}
\item{iter}{Number of iterations used to optimize de log-likelihood}
\item{loglik}{The log-likelihood value}
\item{vcov}{The variance-covariance matrix of the estimated parameters}
\item{items}{The item matrix}
\item{beta}{(Only when regression terms are included) the estimated regression parameters}
\item{linpred}{(Only when regression terms are included) prediction covariates}
}
\author{
  Fernando Massa, \email{fmassa@iesta.edu.uy}
}
\references{
  \insertRef{baker2004}{raschreg}
}
\seealso{
  \code{\link{rasch}}, \code{\link{raschd}}, \code{\link{raschreg}}, \code{\link{raschdreg}}, \code{\link{irt2preg}}
}

\examples{
n   <- 100
J   <- 7
X   <- sim_rasch(n, J, alpha = runif(J, 0.5, 1.5))
mod <- irt2p(X)
}