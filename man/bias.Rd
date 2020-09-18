\name{bias}
\alias{bias}
\title{
  Ability bias calculation
}
\description{
  Bias calculation for maximum likelihood ability estimates from a Rasch model.
}
\usage{
  bias(mod, theta)
}
\arguments{
  \item{mod}{
     A fitted Rasch (regression) model.
  }
  \item{theta}{
     Numeric, ability estimates whose bias is to be estimated.
  }
}
\details{
  Bias of abilities are calulated given the estimated parameters according to the procedure described in Lord (1983).
}
\value{
A numeric vector containing bias estimates of the provuded ability values.
}
\references{
  \insertRef{lord1980}{raschreg}
}
\seealso{
  \code{\link{ability}}
}