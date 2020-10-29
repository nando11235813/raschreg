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

\seealso{
  \code{\link{pfs}}
}

\author{
  Fernando Massa, \email{fmassa@iesta.edu.uy}
}

\examples{
n   <- 50
J   <- 15
X   <- sim_rasch(n, delta = seq(-2, 2, length = J))
# alter 10th observation
X[10, ] <- rep(c(1,0),c(7,8))
# estimate model
mod <- rasch(X)
# Cook's distance 
cooksD(mod)
}