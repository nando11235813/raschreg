\name{raschreg}
\alias{raschreg}

\title{
  Fitting Rasch regression model
}
\description{
Estimate Rasch model incorporating regression variables for the abiity parameter, approximating the marginal log-likelihood using Laplace approximation.
}
\usage{
  raschreg(items, f_reg, z_reg, init = NULL)
}

\arguments{
  \item{items}{
    Numeric matrix (or data.frame) containing only zeroes and ones (J columns).
  }
  \item{f_reg}{
    A one sided regression formula indicating the explanatory variables for the ability parameter.
  }
  \item{z_reg}{
    A data frame containing the explanatory variables indicated in \code{f_reg}.
  }
  \item{init}{
    Optional numeric vector of initial values at which initialize numerical optimization.
  }
}
\details{
  Optimizes \code{raschreglikLA} numericaly via \code{nlminb}. Rows containing at least one \code{NA}, whether in \code{items} or \code{x_reg}, are removed from both. Standard errors of model parameters are approximated by inverting the observed information matrix.
}
\value{
An object of class \code{rasch} is a list containing the following componentes:
\item{call}{The matched call}
\item{coef}{A named vector of coefficients}
\item{iter}{Number of iterations used to optimize de log-likelihood}
\item{loglik}{The log-likelihood value}
\item{vcov}{The variance-covariance matrix of the estimated parameters}
\item{items}{The item matrix}
\item{beta}{(Only when regression terms are included) the estimated regression parameters}
\item{linpred}{(Only when regression terms are included) prediction covariates}
}
\author{
  Fernando Massa, \email{fmassa@iesta.edu.uy}
}
\references{
  \insertRef{rasch1960}{raschreg}

  \insertRef{baker2004}{raschreg}
  
  \insertRef{deboeck2004}{raschreg}
}
\seealso{
  \code{\link{rasch}}, \code{\link{raschd}}, \code{\link{raschreg}}, \code{\link{raschdreg}}, \code{\link{irt2p}}, \code{\link{irt2preg}}
}

\examples{
  n             <- 100
  J             <- 7
  reg           <- diag(4) \%x\% rep(1, n/4)
  colnames(reg) <- paste('v', 1:4, sep = '')
  reg_coef      <- c(-4, -2, 2, 4)
  Xr            <- sim_rasch(n, J,
                             reg = reg, beta = reg_coef)
  mod           <- raschreg(Xr,
                            ~ -1 +v1 + v2 + v3 + v4, 
                            reg)
}