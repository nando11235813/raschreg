\name{vcov.rasch}
\alias{vcov.rasch}

\title{
  Extraction of Variance-Covariance Matrix for a Fitted Model Object
}
\description{
  Returns the variance-covariance matrix of the main parameters of a fitted model object.
}
\usage{
  \method{vcov}{rasch}(object, ...)
}
\arguments{
  \item{object}{
    A fitted rasch model.
  }
  \item{...}{
    Additional arguments.
  }
}
\details{
Extracts cariance/covariance matrix of a fitted Rasch model. This matrix is approximated inverting the Hessian matrix of the (negative) log-lihelihood.
}
\value{
A matrix whose entries are the estimated covariances between the parameter estimates in the model.
}
\author{
  Fernando Massa, \email{fmassa@iesta.edu.uy}
}
