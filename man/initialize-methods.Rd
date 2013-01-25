\docType{methods}
\name{initialize}
\alias{initialize}
\alias{initialize,SummaryUSL-method}
\alias{initialize,USL-method}
\title{Generate an object from the "\code{SummaryUSL}" or "\code{USL}" class}
\usage{
\S4method{initialize}{SummaryUSL}(.Object, call,
coefficients)

\S4method{initialize}{USL}(.Object, frame, regr, resp,
scale.factor, sigma, kappa)
}
\arguments{
  \item{.Object}{The object to initialize.}

  \item{call}{The formula used to create the USL model.}

  \item{coefficients}{A vector containing the coefficients
  of the USL model.}

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
}
\value{
  An object of the specific type.
}
\description{
  Initialize the object.
}
\keyword{internal}

