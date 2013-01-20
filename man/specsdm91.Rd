\docType{data}
\name{specsdm91}
\alias{specsdm91}
\title{Performanced of a Sun SPARCcenter 2000 in the SPEC SDM91 benchmark}
\format{A data frame with 7 rows on 2 variables}
\source{
  N. J. Gunther. Guerrilla Capacity Planning.
  Springer-Verlag, Heidelberg, Germany, 2007.  Original
  dataset from
  \url{http://www.spec.org/osg/sdm91/results/results.html}
}
\description{
  A dataset containing performance data for a Sun
  SPARCcenter 2000 (16 CPUs)
}
\details{
  The SPEC SDM91 benchmark has been run on a

  In October 1994 a Sun SPARCcenter 2000 (16 CPUs) was used
  for the SPEC SDM91 benchmark. The benchmark simulates a
  certain number of users working on a UNIX server. It
  measures the number of script executions per hour for a
  given number of simulated users.

  \itemize{ \item \code{load} The number of simulated users
  (1--216).  \item \code{throughput} The achieved
  throughput in scripts per hour. }
}
\keyword{datasets}

