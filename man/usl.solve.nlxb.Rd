\name{usl.solve.nlxb}
\alias{usl.solve.nlxb}
\title{Solve a USL model using non linear regression}
\usage{
usl.solve.nlxb(model)
}
\arguments{
  \item{model}{A data frame with two columns containing the
  values of the predictor variable in the first column and
  the values of the response variable in the second
  column.}
}
\value{
  A list containing three elements: the scale.factor of the
  model, the model coefficients sigma and kappa.
}
\description{
  This function solves a USL model using non linear
  regression with least squares. It uses the function
  \code{\link{nlxb}} to perform the calculation.
}
\references{
  John C. Nash. nlmrt: Functions for nonlinear least
  squares solutions, 2012. R package version 2012-12.16.
}
\seealso{
  \code{\link{usl}}
}
\keyword{internal}

