\name{raschsk}
\alias{raschsk}
\title{
Fitting Asymmetric Rasch model 
}
\description{
Estimate the asymmetric Rasch model approximating the marginal log-likelihood using Laplace approximation.
}
\usage{
raschsk(items, init = NULL)
}

\arguments{
  \item{items}{
    Numeric matrix (or data.frame) containing only zeroes and ones (J columns).
  }
  \item{init}{
    Optional numeric vector of initial values at which initialize numerical optimization (length J + 1).
  }
}
\details{
The asymmetric model differs from the original in that it assumes that ability is distributed Skew-Normal (following the centred parameters form described by Azzalini). Optimizes \code{raschlikLA} numericaly via \code{nlminb}. Rows containing at least one \code{NA} are removed from \code{items}. Standard errors of model parameters are approximated by inverting the observed information matrix.
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
  
  \insertRef{azzalini2013}{raschreg}
}
\seealso{
  \code{\link{rasch}}, \code{\link{raschd}}, \code{\link{raschreg}}, \code{\link{raschdreg}}, \code{\link{irt2p}}, \code{\link{irt2preg}} 
}

\examples{
n   <- 100
J   <- 7
X   <- sim_rasch(n,J)
mod <- raschsk(X)
}