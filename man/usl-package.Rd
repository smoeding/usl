\docType{package}
\name{usl-package}
\alias{usl-package}
\title{Analyze system scalability with the Universal Scalability Law}
\description{
The Universal Scalability Law is a model to predict
hardware and software scalability. It uses system capacity
as a function of load to forecast the scalability for the
system.
}
\details{
Use the function \code{\link{usl}} to create a model from a
formula and a data frame.

The USL model produces two coefficients as result:
\code{sigma} models the contention and \code{kappa} the
coherency delay of the system.

The Universal Scalability Law has been created by Dr. Neil
Gunther.
}
\references{
Neil J. Gunther. Guerrilla Capacity Planning: A Tactical
Approach to Planning for Highly Scalable Applications and
Services.  Springer, Heidelberg, Germany, 1st edition,
2007.
}
\seealso{
\code{\link{usl}}
}

