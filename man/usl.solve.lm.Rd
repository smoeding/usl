\name{usl.solve.lm}
\alias{usl.solve.lm}
\title{Solve a USL model using a transformation to a 2nd degree polynom}
\usage{
usl.solve.lm(model)
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
  This function solves a USL model using the transformation
  introduced in sections 5.5.1 - 5.5.3 of GCaP.
}
\references{
  Neil J. Gunther. Guerrilla Capacity Planning: A Tactical
  Approach to Planning for Highly Scalable Applications and
  Services. Springer, Heidelberg, Germany, 1st edition,
  2007.
}
\seealso{
  \code{\link{usl}}
}
\keyword{internal}

