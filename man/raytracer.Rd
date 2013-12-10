\docType{data}
\name{raytracer}
\alias{raytracer}
\title{Performance of a ray-tracing software on different hardware configurations}
\format{A data frame with 11 rows on 2 variables}
\source{
Neil J. Gunther. Guerrilla Capacity Planning: A Tactical
Approach to Planning for Highly Scalable Applications and
Services.  Springer, Heidelberg, Germany, 1st edition,
2007.  Original dataset from
\url{http://sourceforge.net/projects/brlcad/}
}
\description{
A dataset containing performance data for a ray-tracing
benchmark.
}
\details{
The benchmark measured the number of ray-geometry
intersections per second. The data was gathered on an SGI
Origin 2000 with 64 R12000 processors running at 300 MHz.

The data frame contains the following variables: \itemize{
\item \code{processors} The number of CPUs used for the
benchmark (1--64).  \item \code{throughput} The number of
operations per second. }
}
\keyword{datasets}

