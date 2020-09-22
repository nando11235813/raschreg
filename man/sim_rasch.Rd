\name{sim_rasch}
\alias{sim_rasch}
\title{
Simulation of different Rasch models
}
\description{
Simulates Rasch models including (or not) discrimination parameters and a regression component.
}
\usage{
sim_rasch(n, J, delta, alpha, gamma, reg, beta, ability)
}
\arguments{
  \item{n}{
    Number of observations.
  }
  \item{J}{
    Number of items
  }
  \item{delta}{
    Numeric vector of difficulty parameters.
  }
  \item{alpha}{
    Numeric vector of (strictly positive) discrimination parameters.
  }
  \item{gamma}{
    Numeric vector of pseudo-guessing parameters. Each value should belong to the [0,1) interval.
  }
  \item{reg}{
    Numeric matrix of \code{p} explanatory variables of ability.
  }
  \item{beta}{
    Numeric vector of regression coefficients (Length p).
  }
  \item{ability}{
    logical; if TRUE, simulated abilities are given along the items.
  }
}
\details{
If difficulty parameters are not specified, they assume values in the interval from -2 to 2. If discrimination parameters are not specified they assume the default value of 1. If pseudo-guessing parameters are not specified they assume the default value of 0.
}
\value{
\code{sim_rasch} returns an object of class "matrix" containing the simulated items and an extra column with the abilities (if \code{ability = TRUE}). The dimension of the result is determined by \code{n} and \code{J + 1}.
}

\author{
Fernando Massa, \email{fmassa@iesta.edu.uy}
}

\seealso{
  \code{\link{rasch}}
}
\examples{
sim <- sim_rasch(100, 10)
mod <- rasch(sim)
}
