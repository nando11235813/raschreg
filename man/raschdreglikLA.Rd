\name{raschdreglikLA}
\alias{raschdreglikLA}
\title{
  Rasch regression model (with discrimination prameter) marginal log-likelihood
}
\description{
Approximation of the marginal log-likelihood of the Rasch regression model (with a common discrimination parameter for all items) using the Laplace approximation.
}
\usage{
  raschdreglikLA(par, X, Z)
}

\arguments{
  \item{par}{
    Numeric vector of parameters (length J+p+1).
  }
  \item{X}{
    Item matrix (J columns) containing only zeroes and ones.
  }
  \item{Z}{
    Regression matrix (p columns).
  }
}
\details{
  Uses two Rcpp compiled functions: \code{huc} and \code{raschreglik}
}
\value{
Returns the value of the marginal log-likelihood evaluated at \code{par}.
}
\author{
  Fernando Massa, \email{fmassa@iesta.edu.uy}
}
\references{
  \insertRef{azevedo1994}{raschreg}
}
\seealso{
  \code{\link{raschlikLA}}, \code{\link{raschdlikLA}},   \code{\link{raschreglikLA}}, \code{\link{irt2plikLA}}, \code{\link{irt2preglikLA}}
}