\name{test}
\alias{test}
\title{
  Test equality constraints.
}
\description{
  \code{test} implements tests of the form \deqn{Id*par_vec = par_value} using Likelihood Rratio, Rao's Score and Wald statistics.
}
\usage{
  test(mod, restr)
}

\arguments{
  \item{mod}{
    A fitted rasch model.
  }
  \item{restr}{
    Vector containing numeric values too fix model parameters.
  }
}

\details{
  \code{restr} should contain NA values at positions corresponding to free parameters. The first J entries of \code{restr} correspond to difficulties, the next J entries correspond to discrimination paremeters (or 1 if the model has common discrimination paprameter). The last p entries correspond to regression parameters.
}

\value{
  A data.frame containing statistics, degrees of freedom and p-values.
}

\author{
  Fernando Massa, \email{fmassa@iesta.edu.uy}
}

\examples{
n   <- 100
J   <- 7
X   <- sim_rasch(n,J)
mod <- rasch(X)
# delta_4 = 1
r <- rep(NA, J)
r[4] <- 1
test(mod, r)
}