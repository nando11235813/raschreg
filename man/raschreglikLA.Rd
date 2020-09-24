\name{raschreglikLA}
\alias{raschreglikLA}
\title{
  Rasch regression model marginal log-likelihood
}
\description{
Approximate the marginal log-likelihood of a Rasch regression model using Laplace approximation.
}
\usage{
  raschreglikLA(par, X, Z, fixed = NULL)
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
  \item{fixed}{
    Optional numeric vector (length J). If supplied, only NA entries in fixed will be estimated. 
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