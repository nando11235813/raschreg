\name{predict.rcs}
\alias{predict.rcs}
\title{
  Evaluate a Restricted Cubic Splines Basis
}
\description{
  Evaluate a predefined restricted cubic spline basis at given values.
}
\usage{
  \method{predict}{rcs}(object, new_x, ...)
}
\arguments{
  \item{object}{
    The result of a call to \code{rcs}.
  }
  \item{new_x}{
    The \code{x} values at which evaluations are required.
  }
  \item{...}{
    Additional arguments.
  }
}
\details{
  The base matrix columns are scaled according to the supplied basis in \code{object}.
}
\value{
Returns the basis matrix for the restricted cubic spline at the \code{new_x} values
}
\author{
  Fernando Massa, \email{fmassa@iesta.edu.uy}
}
\seealso{
  \code{\link{rcs}}
}
