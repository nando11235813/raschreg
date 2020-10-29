\name{pim}
\alias{pim}

\title{
  Person Item Map
}
\description{
  Plots the person-item map of a fitted Rasch (regression) model.
}
\usage{
  pim(mod, ab_type = 'wle', main = NULL)
}
\arguments{
  \item{mod}{
    Fitted Rasch (regression) model whose person-item map is to be ploted.
  }
  \item{ab_type}{
    Character. Type of ability estimation procedure. Default to \code{wle}.
  }
  \item{main}{
    A main title for the plot.
  }
}
\details{
  Displays location of item difficulties and the distribution of person parameters along the ability scale.
}
\author{
  Fernando Massa, \email{fmassa@iesta.edu.uy}
}
\references{
  \insertRef{wilson2005}{raschreg}
}
\seealso{
  \code{\link{plot.rasch}}, \code{\link{info}}, \code{\link{forest}}, \code{\link{itemfit}}
}
\examples{
n <- 100
X <- sim_rasch(n)
mod <- rasch(X)
pim(mod)
}

