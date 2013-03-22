\docType{methods}
\name{confint}
\alias{confint}
\alias{confint,USL-method}
\title{Confidence Intervals for USL model parameters}
\usage{
\S4method{confint}{USL}(object, parm, level)
}
\arguments{
  \item{object}{A USL object.}

  \item{parm}{A specification of which parameters are to be
  given confidence intervals, either a vector of numbers or
  a vector of names. If missing, all parameters are
  considered.}

  \item{level}{The confidence level required.}
}
\value{
  A matrix (or vector) with columns giving lower and upper
  confidence limits for each parameter. These will be
  labelled as (1-level)/2 and 1 - (1-level)/2 in % (by
  default 2.5% and 97.5%).
}
\description{
  Estimate confidence intervals for one or more parameters
  in a USL model.
}
\details{
  Bootstrapping is used to estimate the confidence
  intervals.
}
\seealso{
  \code{\link{usl}}
}

