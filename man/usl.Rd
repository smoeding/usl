\name{usl}
\alias{usl}
\title{Create a model for the Universal Scalability Law}
\usage{
usl(formula, data)
}
\arguments{
  \item{formula}{An object of class "\code{\link{formula}}"
  (or one that can be coerced to that class): a symbolic
  description of the model to be analyzed. The details of
  model specification are given under 'Details'.}

  \item{data}{An optional data frame, list or environment
  (or object coercible by as.data.frame to a data frame)
  containing the variables in the model. If not found in
  data, the variables are taken from
  \code{environment(formula)}, typically the environment
  from which \code{usl} is called.}
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
  The USL model works with one independent variable (e.g.
  virtual users, processes, threads, ...) and one dependent
  variable (e.g. throughput). Therefore the model formula
  must be in the simple "\code{response ~ regressor}"
  format.

  The the USL model produces two coefficients as result.
  Parameter \code{sigma} models the contention and
  \code{kappa} the coherency delay of the system.

  The Universal Scalability Law needs to transform the data
  into a normalized form. Currently it is therefore
  necessary to include one data item where the independent
  variable equals 1. The value of the associated dependent
  variable is used as scale factor for the normalization.

  The model uses the following formula to predict the
  relative capacity of the system for a given load
  \code{N}:

  \deqn{C(N) = \frac{N}{1 + \sigma (N - 1) + \kappa N (N -
  1)}}{C(N) = N / (1 + \sigma * (N - 1) + \kappa * N * (N -
  1))}

  The Universal Scalability Law has been created by Dr.
  Neil Gunther.
}
\examples{
require(usl)

data(raytracer)

## Create USL model for "throughput" by "processors"
usl.model <- usl(throughput ~ processors, raytracer)

## Show summary of model parameters
summary(usl.model)

## Extract coefficients for model
coef(usl.model)

## Calculate point of peak scalability
peak.scalability(usl.model)

## Plot scalability function and original data
plot(raytracer)
plot(usl.model, add=TRUE)
}
\references{
  N. J. Gunther. Guerrilla Capacity Planning.
  Springer-Verlag, Heidelberg, Germany, 2007.
}
\seealso{
  \code{\link{scalability}},
  \code{\link{peak.scalability}}, \code{\link{summary}},
  \code{\link{coef}}, \code{\link{fitted}}
  \code{\link{residuals}}, \code{\link{deviance}}
}

