\name{gradient.usl}
\alias{gradient.usl}
\title{Calculate gradient for the universal scalability function}
\usage{
gradient.usl(sigma, kappa, scale.factor, n)
}
\arguments{
  \item{sigma}{The contention parameter of the model.}

  \item{kappa}{The coherency parameter of the model.}

  \item{scale.factor}{The scale factor used for
  normalization of the model.}

  \item{n}{A vector of values for which the partial
  derivatives are calculated.}
}
\value{
The gradient matrix.
}
\description{
The implementation of this function has been adopted from
the generated output of the \code{\link{deriv}} function.
}
\seealso{
\code{\link{usl}}
}
\keyword{internal}

