\name{itemfit}
\alias{itemfit}

\title{
  Item Fit
}
\description{
  Plots fir of a specified item.
}
\usage{
  itemfit(mod, item, ab_type = 'wle', xlim = c(-5, 5), col = 'tomato', main = NULL)
}
\arguments{
  \item{mod}{
    Fitted Rasch (regression) model.
  }
  \item{item}{
    Character. Indicates which item fit is to be assessed.
  }
  \item{ab_type}{
    Character. Type of ability estimation procedure. Default to \code{wle}.
  }
  \item{xlim}{
    Range of \emph{theta} values over which to plot ICC. Default is c(-3,3).
  }
  \item{col}{
    Color of ICC. Default is 'tomato'.
  }
  \item{main}{
    A main title for the plot.
  }
}
\details{
  Displays ICC of a specified item along with observed proportions of items grouped by person-parameter estimates.
}
\author{
  Fernando Massa, \email{fmassa@iesta.edu.uy}
}
\seealso{
  \code{\link{plot.rasch}}, \code{\link{info}}, \code{\link{forest}}, \code{\link{pim}}
}
\examples{
n <- 100
J <- 7
X <- sim_rasch(n, J)
mod <- rasch(X)
itemfit(mod, 'item6')
}

