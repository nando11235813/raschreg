\name{confint.rasch}
\alias{confint.rasch}

\title{
 Confidence Intervals for Rasch Model Fits
}
\description{
  Computes confidence intervals for one or more parameters in a fitted model.
}
\usage{
  \method{confint}{rasch}(object, parm, level = 0.95, type = 'wald', B = 99, ...)
}
\arguments{
  \item{object}{
    A fitted rasch model.
  }
  \item{parm}{
    A specification of which parameters are to be given confidence intervals. If missing, all parameters are considered.
  }
  \item{level}{
    The confidence level required.
  }
  \item{type}{
    A character string defining the method to be used. Pissible values are: \code{wald}, \code{profile},\code{booth}.
  }
  \item{B}{
    Number of bootstrap replicates. Only applicable when \code{type} is \code{booth}.
  }
  \item{...}{
    Additional arguments.
  }
}
\details{
This method assumes asymptotic normality, and uses suitable \code{coef} and \code{vcov} methods. Type \code{wald} invert the well known Wald statistic. Type \code{profile} is based on the likelihood-ratio statistic. Type \code{boot} uses B replicates simulated from estimated parameters.
}
\value{
A data.frame with columns giving lower and upper confidence limits for each parameter. These will be labelled as (1-level)/2 and 1 - (1-level)/2 in \% (by default 2.5\% and 97.5\%).
}
\author{
  Fernando Massa, \email{fmassa@iesta.edu.uy}
}

\examples{
  n   <- 100
  J   <- 7
  X   <- sim_rasch(n, J)
  mod <- rasch(X)
  confint(mod)
}