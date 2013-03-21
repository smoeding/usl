\docType{methods}
\name{initialize}
\alias{initialize}
\alias{initialize,USL-method}
\title{Generate an object from the "\code{USL}" class}
\usage{
\S4method{initialize}{USL}(.Object, call, frame, regr,
resp, scale.factor, sigma, kappa, boot)
}
\arguments{
  \item{.Object}{The object to initialize.}

  \item{call}{The formula used to create the USL model.}

  \item{frame}{The model frame containing the variables in
  the model.}

  \item{regr}{The name of the regressor variable in the
  model.}

  \item{resp}{The name of the response variable in the
  model.}

  \item{scale.factor}{A numeric value for the scale of the
  model. This is the factor by which the model values have
  been reduced to get a normalized model.}

  \item{sigma}{The contention parameter of the model.}

  \item{kappa}{The coherency delay parameter of the model.}

  \item{boot}{A bootstrap object used to estimate
  confidence intervals for the parameters sigma and kappa.}
}
\value{
  An object of the specific type.
}
\description{
  Initialize the object.
}
\keyword{internal}

