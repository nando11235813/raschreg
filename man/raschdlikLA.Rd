\name{raschdlikLA}
\alias{raschdlikLA}
\title{
  Rasch model (with discrimination prameter) marginal log-likelihood
}
\description{
Approximation of the marginal log-likelihood of the Rasch model (with a common discrimination parameter for all items) using the Laplace approximation.
}
\usage{
  raschdlikLA(par, X, stz, fixed = NULL)
}

\arguments{
  \item{par}{
    Numeric vector of parameters (length J+1).
  }
  \item{X}{
    Item matrix (J columns) containing only zeroes and ones.
  }
  \item{stz}{
    logical. If \emph{TRUE}, sum to zero constraints are imposed on difficulty parameters.
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
  \code{\link{raschlikLA}}, \code{\link{raschreglikLA}}, \code{\link{raschdreglikLA}}, \code{\link{irt2plikLA}}, \code{\link{irt2preglikLA}}
}
