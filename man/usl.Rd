\name{usl}
\alias{usl}
\title{Create a model for the Universal Scalability Law}
\usage{
  usl(formula, data)
}
\arguments{
  \item{formula}{An object of class "\code{\link{formula}}"
  (or one that can be coerced to that class): a symbolic
  description of the model to be analyzed.  The details of
  model specification are given under 'Details'.}

  \item{data}{An optional data frame, list or environment
  (or object coercible by as.data.frame to a data frame)
  containing the variables in the model. If not found in
  data, the variables are taken from environment(formula),
  typically the environment from which usl is called.}
}
\value{
  An object of class USL.
}
\description{
  \code{usl} is used to create Universal Scalability Law
  models. It can be used to forcast the scalability of
  either a hardware or a software system.
}
\details{
  The Universal Scalability Law has been created by Dr.
  Neil Gunther.
}

