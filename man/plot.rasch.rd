\name{plot.rasch}
\alias{plot.rasch}

\title{
  Plot Item Characteristic Curves
}
\description{
  The function \code{plot.rasch} computes (and by default plots) the item characteristic curve of every item from a fitted Rasch (regression) model.
}
\usage{
  \method{plot}{rasch}(x, ..., xlim=c(-4,4), item = NULL, main = NULL)
}
\arguments{
  \item{x}{
    Fitted Rasch (regression) model whose ICCs are to be ploted.
  }
  \item{...}{
    Additional arguments.
  }
  \item{xlim}{
    Range of values of ability over which to graph de ICC.
  }
  \item{item}{
    A character string for ICC identification.
  }
  \item{main}{
    A main title for the plot.
  }
}
\details{
Describes the relationship between the latent ability and the probability of a correct response on each item.
}
\value{
  None.
}
\author{
  Fernando Massa, \email{fmassa@iesta.edu.uy}
}

\seealso{
  \code{\link{info}}, \code{\link{pim}}, \code{\link{forest}}, \code{\link{itemfit}}
}
\examples{
n <- 100
X <- sim_rasch(n)
mod <- rasch(X)
plot(mod)
plot(mod, item = 'item4', main = 'ICC for item4')
}

