\name{update.rasch}
\alias{update.rasch}
\title{
  Update and Re-fit a Model Call
}
\description{
  \code{update} will update and (by default) re-fit a model. It does this by extracting the call stored in the object, updating the call and (by default) evaluating that call.
}
\usage{
  \method{update}{rasch}(object, formula., ..., evaluate = TRUE)
}

\arguments{
  \item{object}{
    A fitted rasch model.
  }
  \item{formula.}{
    Changes to the formula - see \code{update.formula} for details.
  }
  \item{...}{
    Additional arguments to the call, or arguments with changed values.
  }
  \item{evaluate}{
    If true, evaluate the new call, else, return the call.
  }
}
\value{
  If \code{evaluate = TRUE} the fitted object, otherwise the updated call.
}