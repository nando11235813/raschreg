\name{forest}
\alias{forest}

\title{
  Forest Plot
}
\description{
  Plots the forest of different sets of parameters of a fitted Rasch (regression) model.
}
\usage{
  forest(mod, level = 0.05, main_dif = NULL, main_disc = NULL, main_reg = NULL)
}
\arguments{
  \item{mod}{
    Fitted Rasch (regression) model.
  }
  \item{level}{
    The confidence level required.
  }
  \item{main_dif}{
    Main title for difficulty paremeters plot.
  }
  \item{main_disc}{
    Main title for discrimination parameters plot.
  }
  \item{main_reg}{
    Main title for regression parameters plot.
  }
}
\details{
  Confidence intervals are calculated assuming asymptotic normality, and uses suitable coef and vcov methods.
}
\author{
  Fernando Massa, \email{fmassa@iesta.edu.uy}
}
\references{
  \insertRef{lewis2001}{raschreg}
}
\seealso{
  \code{\link{plot.rasch}}, \code{\link{info}}, \code{\link{pim}}, \code{\link{itemfit}}
}
\examples{
n <- 100
X <- sim_rasch(n)
mod <- rasch(X)
forest(mod)
}

