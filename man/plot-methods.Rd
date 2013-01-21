\docType{methods}
\name{plot}
\alias{plot}
\alias{plot,USL-method}
\title{Plot the result from an Universal Scalability Law model}
\arguments{
  \item{x}{The USL object to plot.}

  \item{from}{The start of the range over which the
  scalability function will be plotted.}

  \item{to}{The end of the range over which the scalability
  function will be plotted.}

  \item{xlab}{A title for the x axis: see
  \code{\link{title}}.}

  \item{ylab}{A title for the y axis: see
  \code{\link{title}}.}

  \item{...}{Other graphical parameters passed to plot (see
  \code{\link{par}}, \code{\link{plot.function}}).}
}
\description{
  Create a line plot for an Universal Scalability Law
  model.
}
\details{
  \code{plot} creates a plot of the scalability function
  for the model represented by the argument \code{x}.

  If \code{from} is not specified then the range starts at
  the minimum value given to define the model. An
  unspecified value for \code{to} will lead to plot ending
  at the maximum value from the model. For \code{add =
  TRUE} the defaults are taken from the limits of the
  previous plot.

  \code{xlab} and \code{ylab} can be used to set the axis
  titles. The defaults are the names of the regressor and
  response variables used in the model.
}
\examples{
\dontrun{
## load demo data
data(raytracer)
## build model
usl.model <- usl(throughput ~ processors, raytracer)
## plot scalability function
plot(usl.model)
}
}
\seealso{
  \code{\link{usl}}, \code{\link{plot.function}}
}

