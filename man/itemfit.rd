\name{itemfit}
\alias{itemfit}

\title{
  Item Fit
}
\description{
  Plots fir of a specified item.
}
\usage{
  itemfit(mod, item, ab_type = 'wle', xlim = c(-5, 5), col = 'tomato',
  main = NULL, level = 0.95)
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
  \item{level}{
    The confidence level required. Default to 0.95.
  }
}
\details{
  Displays ICC of a specified item along with observed proportions (along with likelihood based confidence intervals) of items grouped by person-parameter estimates.
}
\author{
  Fernando Massa, \email{fmassa@iesta.edu.uy}
}
\references{
  \insertRef{somerville2013}{raschreg}
}
\seealso{
  \code{\link{plot.rasch}}, \code{\link{info}}, \code{\link{forest}}, \code{\link{pim}}
}
\examples{
n <- 100
X <- sim_rasch(n)
mod <- rasch(X)
itemfit(mod, 'item6')
}

