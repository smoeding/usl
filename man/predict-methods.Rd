\docType{methods}
\name{predict}
\alias{predict}
\alias{predict,USL-method}
\title{Predict method for Universal Scalability Law models}
\usage{
\S4method{predict}{USL}(object, newdata, sigma, kappa)
}
\arguments{
  \item{object}{A USL model object for which prediction is
  desired.}

  \item{newdata}{An optional data frame in which to look
  for variables with which to predict. If omitted, the
  fitted values are used.}

  \item{sigma}{Optional parameter to be used for evaluation
  instead of the parameter computed for the model.}

  \item{kappa}{Optional parameter to be used for evaluation
  instead of the parameter computed for the model.}
}
\value{
  \code{predict} produces a vector of predictions.
}
\description{
  \code{predict} is a function for predictions of the
  scalability of a system modeled with the Universal
  Scalability Law.
}
\details{
  The parameters \code{sigma} or \code{kappa} are useful to
  do a what-if analysis. Setting these parameters override
  the model parameters and show how the system would behave
  with a different contention or coherency delay parameter.

  \code{predict} internally uses the function returned by
  \code{\link{scalability}} to calculate the result.
}
\examples{
require(usl)

data(raytracer)

## Print predicted result from USL model for demo dataset
predict(usl(throughput ~ processors, raytracer))
}
\references{
  N. J. Gunther. Guerrilla Capacity Planning.
  Springer-Verlag, Heidelberg, Germany, 2007.
}
\seealso{
  \code{\link{usl}}, \code{\link{scalability}}
}

