\name{usl.solve.nls}
\alias{usl.solve.nls}
\title{Solve a USL model using non linear regression}
\usage{
usl.solve.nls(model)
}
\arguments{
  \item{model}{A data frame with two columns containing the
  values of the predictor variable in column "\code{load}"
  and the values of the response variable in clumn
  "\code{throughput}".}
}
\value{
  A list containing three elements: the scale.factor of the
  model, the model coefficients sigma and kappa.
}
\description{
  This function solves a USL model using non linear
  regression with least squares.
}
\keyword{internal}

