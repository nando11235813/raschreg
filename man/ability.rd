\name{ability}
\alias{ability}
\title{
 Person Ability Estimation
}
\description{
  Prediction of person abilities from a Rasch (regression) model.
}
\usage{
ability(mod, type = 'wle', R = 100)
}
\arguments{
  \item{mod}{
    Fitted Rasch (regression) model.
  }
  \item{type}{
    A character string. Possible values are \code{eap} (Expected a posteriori), \code{bme} (Bayesian modal estimation), \code{mle} (Maximum likellihood estimation) and \code{wle} (Weighted likelihood estimation). Default to \code{'wle'}.
  }
  \item{R}{
  A numeric value indicating the number of simulations used to approximate \code{theta} when type \code{eap} is selected. Default is 100.
  }
}
\details{
Abilities are estimated as follows.
\itemize{
\item \code{eap}. \eqn{\theta} es estimated by approximating:
  \deqn{\frac{\theta P(Y=y|\theta)f(\theta)d\theta}{\int P(Y=y|\theta)f(\theta)d\theta}}
  R values are simulated from \eqn{f(\theta)} and the quotient is approximated by:
  \deqn{\frac{\sum_j \theta_j P(Y=y|\theta_j)}{\sum_j P(Y=y|\theta_j)}}

\item \code{bme}. \eqn{\theta} es estimated as the solution of:
  \deqn{\sum_j (x_j - P_j(\theta))\frac{P^{'}(\theta)}{P(\theta)(1-P(\theta))} - \theta = 0}

\item \code{mle} \eqn{\theta} es estimated as the solution of:
  \deqn{\sum_j (x_j - P_j(\theta))\frac{P^{'}(\theta)}{P(\theta)(1-P(\theta))}  = 0}

\item{wle}\eqn{\theta} es estimated as the solution of:
  \deqn{\sum_j (x_j - P_j(\theta))\frac{P^{'}(\theta)}{P(\theta)(1-P(\theta))} + \frac{J(\theta)}{2I(\theta)} = 0}
}}

\value{
A numeric containing a single column with the estimated abilities.
}

\author{
  Fernando Massa, \email{fmassa@iesta.edu.uy}
}

\references{
  \insertRef{warm1989}{raschreg}
  \insertRef{magis2016}{raschreg}
}

\examples{
n <- 100
x <- sim_rasch(n, ability = TRUE)
J <- ncol(x) - 1
X <- x[, 1:J]
ab <- x[,J+1]
mod <- rasch(X)

# ability estimation
ab_wle <- ability(mod, type = 'wle')
ab_mle <- ability(mod, type = 'mle')
ab_bme <- ability(mod, type = 'bme')
ab_eap <- ability(mod, type = 'eap')

# comparison
cor(cbind(ab, ab_wle, ab_mle, ab_bme, ab_eap))
}
