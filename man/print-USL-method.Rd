\docType{methods}
\name{print,USL-method}
\alias{print,USL-method}
\title{Print objects of class "\code{USL}"}
\usage{
\S4method{print}{USL}(x, digits = max(3, getOption("digits") - 3), ...)
}
\arguments{
  \item{x}{An object from class \code{USL}.}

  \item{digits}{Minimal number of \emph{significant}
  digits, see \link{print.default}.}

  \item{...}{Other arguments passed to other methods.}
}
\value{
\code{print} returns the object \code{x} invisibly.
}
\description{
\code{print} prints its argument and returns it invisibly
(via \code{\link{invisible}(x)}).
}
\examples{
require(usl)

data(raytracer)

## Print result from USL model for demo dataset
print(usl(throughput ~ processors, raytracer))
}
\seealso{
\code{\link{usl}}, \code{\link{USL-class}}
}

