\name{irt2preglikLA}
\alias{irt2preglikLA}
\title{
  Two Parameter Logistic Regression Model Marginal log-likelihood
}
\description{
Approximation of the marginal log-likelihood of the 2 parameter logistic regression model using the Laplace approximation.
}
\usage{
  irt2preglikLA(par, X, Z)
}

\arguments{
  \item{par}{
    Numeric vector of parameters (length 2*J + p).
  }
  \item{X}{
    Item matrix (J columns) containing only zeroes and ones.
  }
  \item{Z}{
    Regression matrix (p columns).
  }
}
\details{
Uses two \code{Rcpp} compiled functions: \code{huc} and \code{raschreglik}
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
  \code{\link{raschlikLA}}, \code{\link{raschdlikLA}},   \code{\link{raschreglikLA}}, \code{\link{raschdreglikLA}}, \code{\link{irt2plikLA}}, \code{\link{irt2preglikLA}}
}
