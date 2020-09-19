\name{raschlikLA}
\alias{raschlikLA}
\title{
  Rasch model marginal log-likelihood
}
\description{
Approximation of the marginal log-likelihood of the Rasch model using the Laplace approximation.
}
\usage{
raschlikLA(par, X, fixed = NULL)
}

\arguments{
  \item{par}{
    Numeric vector of parameters (length J).
  }
  \item{X}{
    Item matrix (J columns) containing only zeroes and ones.
  }
  \item{fixed}{
    Optional numeric vector (length J). If supplied, only NA entries in fixed will be estimated. 
  }
}
\details{
Uses two \code{Rcpp} compiled functions: \code{hbc} and \code{raschlik}
}
\value{
Returns the value of the marginal log-likelihood evaluated at \code{par}
}

\author{
Fernando Massa, \email{fmassa@iesta.edu.uy}
}
\references{
  \insertRef{azevedo1994}{raschreg}
}
\seealso{
  \code{\link{raschdlikLA}}, \code{\link{raschreglikLA}}, \code{\link{raschdreglikLA}}, \code{\link{irt2plikLA}}, \code{\link{irt2preglikLA}}
}
