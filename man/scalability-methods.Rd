\docType{methods}
\name{scalability}
\alias{scalability}
\alias{scalability,USL-method}
\title{Scalability function of a USL model}
\usage{
  \S4method{scalability}{USL}(object)
}
\arguments{
  \item{object}{A USL object.}
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
  The returned function can be used to look at specific
  values once the model for a specific system has been
  created.
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

