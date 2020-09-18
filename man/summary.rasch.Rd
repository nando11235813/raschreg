\name{summary.rasch}
\alias{summary.rasch}

\title{
  Summarizing Rasch Model Fits
}
\description{
  \code{summary} method for class \code{"rasch"}.
}
\usage{
  \method{summary}{rasch}(object, correlation=FALSE, signif.stars=getOption("show.signif.stars"),...)
}
\arguments{
  \item{object}{
    A fitted rasch model.
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
}
\value{
The function summary.rasch computes and returns a list of summary statistics of the fitted  model given in \code{object}.
}
\author{
  Fernando Massa, \email{fmassa@iesta.edu.uy}
}

\examples{
  n   <- 100
  J   <- 7
  X   <- sim_rasch(n, J)
  mod <- rasch(X)
  summary(mod)
}