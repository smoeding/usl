\docType{methods}
\name{$}
\alias{$}
\alias{$,SummaryUSL-method}
\alias{$,USL-method}
\title{Extract parts of an "\code{SummaryUSL}" or "\code{USL}" object}
\usage{
  \S4method{$}{SummaryUSL}(x, name)

  \S4method{$}{USL}(x, name)
}
\arguments{
  \item{x}{Object from which to extract elements.}

  \item{name}{A literal character string or a \link{name}
  (possibly quoted).}
}
\description{
  The operator extracts a part of an
  \code{\link{SummaryUSL-class}} or \code{\link{USL-class}}
  object.
}
\details{
  This is a generic method for the two classes used in the
  usl package.

  The operator is used internally by functions like
  \code{\link{coef}}, so it is necessary to have a working
  implementation of the \code{coef} function.
}
\examples{
\dontrun{
## get coefficients from usl model
usl.model$coefficients
}
}
\seealso{
  \code{\link{SummaryUSL-class}}, \code{\link{USL-class}},
  \code{\link{Extract}}
}
\keyword{internal}

