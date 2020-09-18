\name{raschreglikLA}
\alias{raschreglikLA}
\title{
  Rasch regression model marginal log-likelihood
}
\description{
Approximate the marginal log-likelihood of a Rasch regression model using Laplace approximation.
}
\usage{
  raschreglikLA(par, X, Z)
}

\arguments{
  \item{par}{
    Numeric vector of parameters (length J+p).
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