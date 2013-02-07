\docType{methods}
\name{scalability}
\alias{scalability}
\alias{scalability,USL-method}
\title{Scalability function of a USL model}
\usage{
\S4method{scalability}{USL}(object, sigma, kappa)
}
\arguments{
  \item{object}{A USL object.}

  \item{sigma}{Optional parameter to be used for evaluation
  instead of the parameter computed for the model.}

  \item{kappa}{Optional parameter to be used for evaluation
  instead of the parameter computed for the model.}
}
\value{
  A function with parameter \code{x} that calculates the
  scalability value of the specific model.
}
\description{
  \code{scalability} is a higher order function and returns
  a function to calculate the scalability for the specific
  USL model.
}
\details{
  The returned function can be used to calculate specific
  values once the model for a system has been created.

  The parameters \code{sigma} or \code{kappa} are useful to
  do a what-if analysis. Setting these parameters override
  the model parameters and show how the system would behave
  with a different contention or coherency delay parameter.
}
\examples{
require(usl)

data(raytracer)

## Compute the scalability function
s <- scalability(usl(throughput ~ processors, raytracer))

## Print scalability for 32 CPUs for the demo dataset
print(s(32))

## Plot scalability for the range from 1 to 64 CPUs
plot(s, from=1, to=64)
}
\references{
  N. J. Gunther. Guerrilla Capacity Planning.
  Springer-Verlag, Heidelberg, Germany, 2007.
}
\seealso{
  \code{\link{usl}}, \code{\link{peak.scalability}}
}

