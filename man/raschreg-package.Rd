\name{raschreg-package}
\alias{raschreg-package}
\docType{package}
\title{
  Item Response Theory regression models.
}
\description{
  Provides a set of functions to estimate, simulate and visualize Item Response Theroy models such as: Rasch model, Rasch model with a common discrimination parameter and the Two Parameter Logistic model). Inclusion of regression effects of the ability paramters are allowed. Marginal log-likelihood is approximated using the Laplace method.
}
\author{
Maintainer: Fernando Massa <fmassa@iesta.edu.uy>
}

\keyword{ package }

\examples{
  \dontrun{
     library(raschreg)
     data(odont_ncd)
     items <- odont_ncd[,c('pocket', 'loa', 'decay', 'f_dentition')]
     mod <- raschreg(items, ~ age + sex_f, odont_ncd)
     summary(mod)
     forest(mod)
  }
}
