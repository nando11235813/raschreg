\name{ability}
\alias{ability}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
 Person Ability Estimation
}
\description{
  Prediction of person abilities from a Rasch (regression) model.
}
\usage{
ability(mod, type = 'lord')
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{mod}{
    Fitted Rasch (regression) model.
  }
  \item{type}{
    A character string. If \code{'lord'} the \code{MAPE} estimator is bias-corrected according to Lord's procedure. If \code{'MAPE'} the posterior (approximated) likelihood is maximized. Default to \code{'lord'}.
  }
}
\details{
Abilities are estimated as the mode of the posterior distribution (MAP) of abilities given the items and parameter estimates. Alternativelly, this estimates could be corrected using Lord's procedure.
}
\value{
A data.frame containing a single column with the estimated abilities.
}

\author{
  Fernando Massa, \email{fmassa@iesta.edu.uy}
}
\examples{
n<-100
J<-7
X<-sim_rasch(n,J)
mod<-rasch(X)
ability(mod)
}
