\name{drop1.rasch}
\alias{drop1.rasch}

\title{
  Add or Drop All Possible Single Terms to a Rasch Model
}
\description{
  \code{drop1} method for class \code{"rasch"}.
}
\usage{
  \method{drop1}{rasch}(object, scope = NULL, test = 'LRT', cov_type = 'hessian', ...)
}
\arguments{
  \item{object}{
    A fitted rasch model.
  }
  \item{scope}{
    A formula giving the terms to be considered for dropping.
  }
  \item{test}{
    A character string, matchiing one of \emph{LRT} o \emph{Wald}.
  }
  \item{cov_type}{
    A character indicating which covariance matrix estimation is to be used. Possible choices are 'hessian', 'opg' and 'sandwich'.
  }
  \item{...}{
    Additional arguments affecting the summary produced.
  }
}
\details{
  The hierarchy is respected when considering terms to be dropped from the model: all the main effects present in the interactions are deleted when said effect is deleted from the model.
}
\value{
The function drop1.rasch computes and returns a data.frame containing information regarding the deletion of every term in the \code{object} formula.
}
\author{
  Fernando Massa, \email{fmassa@iesta.edu.uy}
}

\examples{
  dif  <- c(-3, -2, -1, 0, 1, 2, 3) 
	n    <- 200
	x1x2 <- cbind(x1 = rnorm(n), x2 = ifelse(runif(n) < 0.5, 0, 1))
	beta <- c(1.2, 0.7)
	X    <- sim_rasch(n = n, J = 7, delta = dif, reg = x1x2, beta = beta)
  mod  <- raschreg(X, ~ x1 + x2, x1x2)
  drop1(mod)
}