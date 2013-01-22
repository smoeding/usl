\docType{methods}
\name{peak.scalability}
\alias{peak.scalability}
\alias{peak.scalability,USL-method}
\title{Peak scalability value of a USL model}
\usage{
  \S4method{peak.scalability}{USL}(object)
}
\arguments{
  \item{object}{A USL object.}
}
\value{
  A numeric value for the point where peak scalability will
  be reached.
}
\description{
  Calculate the point of peak scalability for a specific
  model.
}
\details{
  The peak scalability is the point where the throughput of
  the system goes retrograde (i.e., decreases with
  increasing load). See formula (4.33) in \emph{Guerilla
  Capacity Planning}.
}
\examples{
require(usl)

data(raytracer)

peak.scalability(usl(throughput ~ processors, raytracer))
## Peak scalability will be reached just below 450 processors
}
\references{
  N. J. Gunther. Guerrilla Capacity Planning.
  Springer-Verlag, Heidelberg, Germany, 2007.
}
\seealso{
  \code{\link{usl}}, \code{\link{scalability}}
}

