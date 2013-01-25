\docType{methods}
\name{print}
\alias{print}
\alias{print,SummaryUSL-method}
\alias{print,USL-method}
\title{Print objects of class "\code{SummaryUSL}" or "\code{USL}"}
\usage{
\S4method{print}{SummaryUSL}(x, digits, ...)

\S4method{print}{USL}(x, digits, ...)
}
\arguments{
  \item{x}{An object from class \code{SummaryUSL} or
  \code{USL}.}

  \item{digits}{Minimal number of \emph{significant}
  digits, see \link{print.default}.}

  \item{...}{Other arguments passed to other methods.}
}
\value{
  \code{print} returns the object \code{x} invisibly.
}
\description{
  \code{print} prints its argument and returns is invisibly
  (via \code{\link{invisible}(x)}).
}
\examples{
require(usl)

data(raytracer)

## Print result from USL model for demo dataset
print(usl(throughput ~ processors, raytracer))
}
\seealso{
  \code{\link{usl}}, \code{\link{print}}
}

