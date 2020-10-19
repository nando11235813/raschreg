\name{backward}
\alias{backward}
\title{
Inference based model selecton for IRT models.
}
\description{
Select the optimal number of variables by means of LRT or F tests.
}
\usage{
backward(mod, level = 0.05, test = 'LRT', cov_type = 'hessian')
}
\arguments{
  \item{mod}{
    A fitted rasch model.
  }
  \item{level}{
    Significance level to bu used.
  }
  \item{test}{
    A character string, matchiing one of \emph{LRT} o \emph{Wald}.
  }
  \item{cov_type}{
    A character indicating which covariance matrix estimation is to be used. Possible choices are 'hessian', 'opg' and 'sandwich'.
  }
}
\details{
 \code{backward} uses \code{\link{drop1}} repeatedly until all explanatory variables in the model are significant at a specified level.
}
\value{
\code{backward} returns an object of class \code{rasch} containing the optimal subset of variables.
}

\author{
Fernando Massa, \email{fmassa@iesta.edu.uy}
}

\seealso{
  \code{\link{drop1}}
}
