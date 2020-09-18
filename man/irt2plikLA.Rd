\name{irt2plikLA}
\alias{irt2plikLA}
\title{
  Two Parameter Logistic Model Marginal log-likelihood
}
\description{
Approximation of the marginal log-likelihood of the 2 parameter logistic model using the Laplace approximation.
}
\usage{
  irt2plikLA(par,X)
}

\arguments{
  \item{par}{
    Numeric vector of parameters (length 2*J).
  }
  \item{X}{
    Item matrix (J columns) containing only zeroes and ones.
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
  \code{\link{raschlikLA}}, \code{\link{raschdlikLA}}, \code{\link{raschreglikLA}}, \code{\link{raschdreglikLA}}, \code{\link{irt2preglikLA}}
}
