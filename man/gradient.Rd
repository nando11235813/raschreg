\name{gradient}
\alias{gradient}
\title{
  Approximation of gradient vector.
}
\description{
  \code{gradient} uses forward difference to approximate the vector of derivatives of \code{fun} at \code{param}.
}
\usage{
  gradient(fun, param, ..., fun0 = NULL)
}
\arguments{
  \item{fun}{
     Function whose gradient vector is to be approximated.
  }
  \item{param}{
     Numeric vector of parameters where \code{fun} is to be evaluated
  }
  \item{...}{
     Any additional arguments passed to \code{fun}.
  }
  \item{fun0}{
    The value of \code{fun} at \code{param}. If it is \code{NULL}, \code{fun} will be evaluated at \code{param}.
  }
}
\details{
  The forward difference uses a step-size of the cubic root of machine epsilon. No scaling is applied to parameters.
}
\value{
A numeric row matrix whose size is determined by the length of \code{param}.
}

\examples{
f  <- function(x) sum(x^2)
x0 <- c(1,1)
f0 <- f(x0)
gradient(f, x0, fun0 = f0)
}