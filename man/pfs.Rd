\name{pfs}
\alias{pfs}
\title{
  Calculate Person Fit Statistics.
}
\description{
  Function to calculate person fit statistics.
}
\usage{
  pfs(mod, level = 0.05, ab_type = 'wle')
}

\arguments{
  \item{mod}{
    A fitted rasch model.
  }
  \item{level}{
    A numeric indicating cutoff value for testing lack of fit. Default is 0.05.
  }
  \item{ab_type}{
    A character indicating which procedure should be used to estimate ability parameters. Possible choices are 'wle' (default), 'mle', 'bme' and 'eap'.
  }
}

\details{
  The procedure described by Snijders (2001) is used to calculate a statistic of the form:
\deqn{\sum_j (y_j - P_j(\theta))w_j(\theta)}
Where \eqn{w_j(\theta)} are corrected to account for ability estimation.
}

\value{
  A data.frame vector containing the value of the estimated abilities, statistics and p-values.
}
\references{
  \insertRef{snijders2001}{raschreg}
}
\seealso{
  \code{\link{cooksD}}
}
\author{
  Fernando Massa, \email{fmassa@iesta.edu.uy}
}

\examples{
n   <- 50
J   <- 15
X   <- sim_rasch(n, J)
pfs(mod)
}