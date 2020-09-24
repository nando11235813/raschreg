\name{vcov.rasch}
\alias{vcov.rasch}

\title{
  Extraction of Variance-Covariance Matrix for a Fitted Model Object
}
\description{
  Returns the variance-covariance matrix of the main parameters of a fitted model object.
}
\usage{
  \method{vcov}{rasch}(object, type = 'hessian', ...)
}
\arguments{
  \item{object}{
    A fitted rasch model.
  }
  \item{type}{
    A character indicating which covariance matrix estimation is to be used. If 'hessian', the Hessian matrix of log-likelihood  is used. if 'opg', the outer product of log-likelihood gradient is used. If 'sandwich', the previous two are used in a Huber type estimator.
  }
  \item{...}{
    Additional arguments.
  }
}
\details{
Extracts variance/covariance matrix of a fitted Rasch model. In the case of discrimination parameters, a delta method correction is implemented because these parameters are estimated on a logarithmic scale.
}
\value{
A matrix whose entries are the estimated covariances between the parameter estimates in the model.
}
\author{
  Fernando Massa, \email{fmassa@iesta.edu.uy}
}
