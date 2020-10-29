\name{lincon}
\alias{lincon}
\title{
  Test Linear Constraints.
}
\description{
  \code{lincon} tests a set of linear constraints by means of the Wald statistic.
}
\usage{
  lincon(mod, R, b, cov_type = 'hessian')
}

\arguments{
  \item{mod}{
    A fitted rasch model.
  }
  \item{R}{
    A numeric matrix containging up to q rows (one per constraint) and as many columns as parameters the model has.
  }
  \item{b}{
    A vector of length q.
  }
  \item{cov_type}{
    A character indicating which covariance matrix estimation is to be used. Possible choices are 'hessian', 'opg' and 'sandwich'.
  }
}

\details{
  Linear constraints are specified in the form:
  \deqn{R * \theta = b}
  Where R is a matrix of dimensions q*p. The vector b might be specified either as a numeric object or as a one column matrix.
  The p-value is approximated by an F distribution with q and n - p degres of freedom.
}

\value{
  A data.frame containing the value of the statistic, degrees of freedom and p-value.
}

\author{
  Fernando Massa, \email{fmassa@iesta.edu.uy}
}

\examples{
n   <- 100
X   <- sim_rasch(n)
mod <- rasch(X)
# test equality of delta_1 and delta_2.
R <- matrix(c(1, -1, 0, 0, 0, 0, 0), nrow = 1)
b <- 0
lincon(mod, R = R, b = b)
}