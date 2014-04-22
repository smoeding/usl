\docType{class}
\name{USL-class}
\alias{USL-class}
\title{Class "\code{USL}" for Universal Scalability Law models}
\description{
This class encapsulates the Universal Scalability Law. Use
the function \code{\link{usl}} to create new objects from
this class.
}
\section{Slots}{

\describe{
\item{\code{frame}}{The model frame.}

\item{\code{call}}{The call used to create the model.}

\item{\code{regr}}{The name of the regressor variable.}

\item{\code{resp}}{The name of the response variable.}

\item{\code{scale.factor}}{The scale factor used to create the model.}

\item{\code{coefficients}}{The coefficients sigma and kappa of the model.}

\item{\code{boot}}{A bootstrap object used to estimate confidence intervals for the parameters sigma and kappa.}

\item{\code{fitted}}{The fitted values of the model. This is a vector.}

\item{\code{residuals}}{The residuals of the model. This is a vector.}

\item{\code{df.residual}}{The degrees of freedom of the model.}

\item{\code{r.squared}}{Coefficient of determination of the model.}

\item{\code{adj.r.squared}}{Adjusted coefficient of determination.}

\item{\code{efficiency}}{The efficiency, e.g. speedup per processor.}

\item{\code{na.action}}{The \code{na.action} used by the model.}
}}
\seealso{
\code{\link{usl}}
}

