\name{info}
\alias{info}

\title{
  Plot Test Information Function
}
\description{
  The function \code{info} computes (and by default plots) the test information function from a fitted Rasch (regression) model.
}
\usage{
  info(mod, plot = TRUE, theta = NULL, item = NULL, main_item = NULL, main_total = NULL, which = 'both')
}
\arguments{
  \item{mod}{
    Fitted Rasch (regression) model whose information curve is to be ploted.
  }
  \item{plot}{
    logical; if \code{plo t= TRUE} (the default) te information curve is plotted.
  }
  \item{theta}{
    numeric; values of ability at which informatiob is to be calculated.
  }
  \item{item}{
    A character string for ICC identification. Only applicable when information is available for all items.
  }
  \item{main_item}{
    A main title for the items information plot.
  }
  \item{main_total}{
    A main title for the total information plot.
  }
  \item{which}{
    Which information plot should be presented. Possible values are \code{'item'}, \code{'total'} and \code{'both'}. 
    If \code{'lord'} the \code{MAPE} estimator is bias-corrected according to Lord's procedure. If \code{'MAPE'} the posterior (approximated) likelihood is maximized. Default to \code{'lord'}.
  }
}
\details{
Displays information regarding the whole set of items considered in the model. Information is computed for each item and for the test as a whole.
}
\value{
A data.frame containing the values of \code{theta}, information of every item, and the test information.
}
\author{
  Fernando Massa, \email{fmassa@iesta.edu.uy}
}

\seealso{
  \code{\link{plot.rasch}}, \code{\link{pim}}, \code{\link{forest}}
}
\examples{
n<-100
J<-7
X<-sim_rasch(n,J)
mod<-rasch(X)
info(mod)
}

