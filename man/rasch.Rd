\name{rasch}
\alias{rasch}
\title{
Fitting Rasch model
}
\description{
Estimate Rasch model approximating the marginal log-likelihood using Laplace approximation.
}
\usage{
rasch(items, init = NULL, fixed = NULL)
}

\arguments{
  \item{items}{
    Numeric matrix (or data.frame) containing only zeroes and ones (J columns).
  }
  \item{init}{
    Optional numeric vector of initial values at which initialize numerical optimization (length J).
  }
  \item{fixed}{
    Optional numeric vector (length J). If supplied, only NA entries in fixed will be estimated. 
  }
}
\details{
Optimizes \code{raschlikLA} numericaly via \code{nlminb}. Rows containing at least one \code{NA} are removed from \code{items}. Standard errors of model parameters are approximated by inverting the observed information matrix.
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
  \insertRef{rasch1960}{raschreg}

  \insertRef{baker2004}{raschreg}
}
\seealso{
  \code{\link{raschd}}, \code{\link{raschreg}}, \code{\link{raschdreg}}, \code{\link{irt2p}}, \code{\link{irt2preg}} 
}

\examples{
n   <- 100
J   <- 7
X   <- sim_rasch(n,J)
mod <- rasch(X)
}