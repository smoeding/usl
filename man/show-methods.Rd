\docType{methods}
\name{show}
\alias{show}
\alias{show,USL-method}
\title{Show objects of class "\code{USL}"}
\usage{
\S4method{show}{USL}(object)
}
\arguments{
  \item{object}{The object to be printed.}
}
\value{
  \code{show} returns an invisible \code{NULL}.
}
\description{
  Display the object by printing it.
}
\examples{
require(usl)

data(raytracer)

## Show USL model
show(usl(throughput ~ processors, raytracer))
}
\seealso{
  \code{\link{usl}}, \code{\link{USL-class}}
}

