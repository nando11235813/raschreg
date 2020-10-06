\name{cooksD}
\alias{cooksD}
\title{
  Calculate Cook's Distance.
}
\description{
  Function to calculate the Cook's distances.
}
\usage{
  cooksD(mod, cov_type = 'hessian', trace = TRUE)
}

\arguments{
  \item{mod}{
    A fitted rasch model.
  }
  \item{cov_type}{
    A character indicating which covariance matrix estimation is to be used. Possible choices are 'hessian', 'opg' and 'sandwich'.
  }
  \item{trace}{
    Wheter to show progress of the leave-one-out loop.
  }
}

\details{
  The algorithm proceeds by deleting an entire row of the item matrix at a time. At every iteration the vector of parameters of the \emph{full} model is compared with the vector of parameters having deleted the \emph{i-th} row.
}

\value{
  A numeric vector containing the value of the statistic.
}

\author{
  Fernando Massa, \email{fmassa@iesta.edu.uy}
}

\examples{
n   <- 100
J   <- 7
X   <- sim_rasch(n,J)
mod <- rasch(X)
# test equality of delta_1 and delta_2.
cooksD(mod)
}