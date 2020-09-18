\name{logLik.rasch}
\alias{AIC.rasch}
\alias{BIC.rasch}
\alias{logLik.rasch}
\title{
  Extract Log-likelihood from a Rasch Model
}
\description{
  \code{logLik}, \code{AIC} and \code{BIC} methods for class \code{"rasch"}.
}
\usage{
  \method{AIC}{rasch}(object,...,k=2)
  \method{BIC}{rasch}(object,...)
  \method{logLik}{rasch}(object,...)
}
\arguments{
  \item{object}{
    A fitted model of class \cite{rasch}.
  }
  \item{...}{
    Optionally more fitted model objects.
  }
  \item{k}{
    Numeric, the \emph{penalty} per parameter to be used; the default k = 2 is the classical AIC.
  }
}

\keyword{ ~internal}