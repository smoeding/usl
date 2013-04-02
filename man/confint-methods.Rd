\docType{methods}
\name{confint}
\alias{confint}
\alias{confint,USL-method}
\title{Confidence Intervals for USL model parameters}
\usage{
\S4method{confint}{USL}(object, parm, level = 0.95, type =
"norm")
}
\arguments{
  \item{object}{A USL object.}

  \item{parm}{A specification of which parameters are to be
  given confidence intervals, either a vector of numbers or
  a vector of names. If missing, all parameters are
  considered.}

  \item{level}{The confidence level required.}

  \item{type}{A character string representing the type of
  interval required. The value must be one of
  "\code{norm}", "\code{basic}", "\code{stud}",
  "\code{perc}" or "\code{bca}".}
}
\value{
  A matrix (or vector) with columns giving lower and upper
  confidence limits for each parameter. These will be
  labelled as (1-level)/2 and 1 - (1-level)/2 in \% (by
  default 2.5\% and 97.5\%).
}
\description{
  Estimate confidence intervals for one or more parameters
  in a USL model.
}
\details{
  Confidence intervals are estimated by using a bootstrap.
  The bootstrap generates a series of random selections
  with replacement for the original data and calculates the
  model parameters sigma and kappa for every selection. The
  set of parameters are used to estimate confidence
  intervals.

  The parameter "\code{type}" determines the type of
  interval that is calculated. See \code{\link{boot.ci}}
  for details.

  The bootstrap is computed once in the \code{\link{usl}}
  function so calling \code{confint} multiple times for a
  specific USL object will return identical results.

  Calculating confidence intervals for a small number of
  observations is unreliable. The function will print
  warning or error messages if the calculated intervals are
  dubious or the estimation is not possible.
}
\examples{
require(usl)

data(specsdm91)

## Create USL model
usl.model <- usl(throughput ~ load, specsdm91)

## Print normal confidence intervals
confint(usl.model, type = "norm")
}
\seealso{
  \code{\link{usl}}, \code{\link{boot.ci}}
}

