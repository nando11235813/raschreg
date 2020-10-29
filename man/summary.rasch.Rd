\name{summary.rasch}
\alias{summary.rasch}

\title{
  Summarizing Rasch Model Fits
}
\description{
  \code{summary} method for class \code{"rasch"}.
}
\usage{
  \method{summary}{rasch}(object, cov_type='hessian', correlation=FALSE, signif.stars=getOption("show.signif.stars"),...)
}
\arguments{
  \item{object}{
    A fitted rasch model.
  }
  \item{cov_type}{
    A character indicating which covariance matrix estimation is to be used. If 'hessian', the Hessian matrix of log-likelihood  is used. if 'opg', the outer product of log-likelihood gradient is used. If 'sandwich', the previous two are used in a Huber type estimator. Default to 'hessian'.
  }
  \item{correlation}{
    logical; if \code{TRUE} the correlation matrix of the estimated parameters is returned and printed.
  }
  \item{signif.stars}{
    logical, if \code{TRUE}, 'significance stars' are printed for each coefficient.
  }
  \item{...}{
    Additional arguments affecting the summary produced.
  }
}
\details{
Returns information regarding each parameter type in the model and additionally gives 'significance stars' if signif.stars is TRUE. Correlations are printed to two decimal places. To see the actual correlations print summary(object)$correlation directly.
Difficulties are compared to 0 and discrimination parameters are compared to 1. Since discrimination parameters are estimated on logarithmic scale, the delta method is used to estimated it's variences.
}
\value{
The function summary.rasch computes and returns a list of summary statistics of the fitted  model given in \code{object}.
}
\author{
  Fernando Massa, \email{fmassa@iesta.edu.uy}
}

\examples{
  n   <- 100
  X   <- sim_rasch(n)
  mod <- rasch(X)
  summary(mod)
}