\name{usl.solve.lm}
\alias{usl.solve.lm}
\title{Solve a USL model using a transformation to a 2nd degree polynom}
\usage{
usl.solve.lm(model)
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
  This function solves a USL model using the transformation
  introduced in sections 5.5.1 - 5.5.3 of GCaP.
}
\references{
  N. J. Gunther. Guerrilla Capacity Planning.
  Springer-Verlag, Heidelberg, Germany, 2007.
}
\keyword{internal}

