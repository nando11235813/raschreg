\name{rcs}
\alias{rcs}
\title{
  Restricted Cubic Splines
}
\description{
  Generate the Restricted cubic spline basis matrix for a polynomial spline.
}
\usage{
  rcs(x, m=NULL, knots=NULL, scale=TRUE)
}
\arguments{
  \item{x}{
    The predictor variable.
  }
  \item{m}{
    Spline order (number of nodes).
  }
  \item{knots}{
    Internal breakpoints that define the spline.
  }
  \item{scale}{
    logical; if \code{scale = TRUE}, the columns of the basis are scaled to have the same variability.
  }
}
\details{
  The base matrix columns are scaled for optimization purposes. If not provieded, internal knots are determined using quantiles of \code{x}.
}
\value{
Returns the basis matrix for the restricted cubic spline.
}
\author{
  Fernando Massa, \email{fmassa@iesta.edu.uy}
}
\references{
  \insertRef{durrelman1989}{raschreg}
}
\seealso{
  \code{\link{predict.rcs}}
}
