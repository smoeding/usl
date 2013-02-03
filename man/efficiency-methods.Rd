\docType{methods}
\name{efficiency}
\alias{efficiency}
\alias{efficiency,USL-method}
\title{Efficiency of the system}
\usage{
\S4method{efficiency}{USL}(object)
}
\arguments{
  \item{object}{A USL object.}
}
\value{
  A vector of numeric values.
}
\description{
  The efficiency of a system is a vector that contains for
  every measurement the deviation from linear scalability.
}
\details{
  A value of \code{1} indicates linear scalability while
  values less than \code{1} correspond to the fraction
  compared to linear scalability.
}
\examples{
require(usl)

data(raytracer)

## Show the efficiency
efficiency(usl(throughput ~ processors, raytracer))
}
\seealso{
  \code{\link{usl}}
}

