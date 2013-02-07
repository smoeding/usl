\name{usl}
\alias{usl}
\title{Create a model for the Universal Scalability Law}
\usage{
usl(formula, data, method = "default")
}
\arguments{
  \item{formula}{An object of class "\code{\link{formula}}"
  (or one that can be coerced to that class): a symbolic
  description of the model to be analyzed. The details of
  model specification are given under 'Details'.}

  \item{data}{A data frame, list or environment (or object
  coercible by as.data.frame to a data frame) containing
  the variables in the model. If not found in data, the
  variables are taken from \code{environment(formula)},
  typically the environment from which \code{usl} is
  called.}

  \item{method}{Character value specifying the method to
  use. The possible values are described unter 'Details'.}
}
\value{
  An object of class USL.
}
\description{
  \code{usl} is used to create a model for the Universal
  Scalability Law.
}
\details{
  The Universal Scalability Law is used to forcast the
  scalability of either a hardware or a software system.

  The USL model works with one independent variable (e.g.
  virtual users, processes, threads, ...) and one dependent
  variable (e.g. throughput, ...). Therefore the model
  formula must be in the simple "\code{response ~
  predictor}" format.

  The model produces two coefficients as result:
  \code{sigma} models the contention and \code{kappa} the
  coherency delay of the system.

  The argument \code{method} selects the method which is
  used to solve the model:

  \itemize{ \item "\code{default}" for the default method
  using a transformation into a 2nd degree polynom. It can
  only be used if the model frame contains a value for the
  normalization where the predictor equals "\code{1}" for
  one measurement. This is the algorithm introduced by Dr.
  Neil J. Gunther in the book \emph{Guerrilla Capacity
  Planning}. \item "\code{nls}" for a nonlinear regression
  model. This method estimates not only the coefficients
  \code{sigma} and \code{kappa} but also the
  \code{scale.factor} for the normalization.
  \code{\link{nls}} with the "\code{port}" algorithm is
  used internally to solve the model. So all restrictions
  of the "\code{port}" algorithm apply. \item "\code{nlxb}"
  for a nonliner regression model using the function
  \code{\link{nlxb}} from the \code{\link{nlmrt}} package.
  This method also estimates both coefficients and the
  normalization factor. It is expected to be more robust
  than the method used by the \code{nls} function. }

  The Universal Scalability Law uses the following formula
  to predict the relative capacity of the system for a
  given load \code{N}:

  \deqn{C(N) = \frac{N}{1 + \sigma (N - 1) + \kappa N (N -
  1)}}{C(N) = N / (1 + \sigma * (N - 1) + \kappa * N * (N -
  1))}
}
\examples{
require(usl)

data(raytracer)

## Create USL model for "throughput" by "processors"
usl.model <- usl(throughput ~ processors, raytracer)

## Show summary of model parameters
summary(usl.model)

## Show complete list of efficiency parameters
efficiency(usl.model)

## Extract coefficients for model
coef(usl.model)

## Calculate point of peak scalability
peak.scalability(usl.model)

## Plot original data and scalability function
plot(raytracer)
plot(usl.model, add=TRUE)
}
\references{
  N. J. Gunther. Guerrilla Capacity Planning.
  Springer-Verlag, Heidelberg, Germany, 2007.
}
\seealso{
  \code{\link{efficiency}}, \code{\link{scalability}},
  \code{\link{peak.scalability}}, \code{\link{summary}},
  \code{\link{coef}}, \code{\link{fitted}},
  \code{\link{residuals}}, \code{\link{deviance}}
}

