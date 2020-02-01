# Copyright (c) 2013-2020 Stefan Moeding
# All rights reserved.
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions
# are met:
# 1. Redistributions of source code must retain the above copyright
#    notice, this list of conditions and the following disclaimer.
# 2. Redistributions in binary form must reproduce the above copyright
#    notice, this list of conditions and the following disclaimer in the
#    documentation and/or other materials provided with the distribution.
#
# THIS SOFTWARE IS PROVIDED BY THE AUTHOR AND CONTRIBUTORS ``AS IS'' AND
# ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
# IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
# ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR OR CONTRIBUTORS BE LIABLE
# FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
# DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
# OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
# HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
# LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
# OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
# SUCH DAMAGE.


##############################################################################
#' Solve a USL model using non linear regression
#'
#' This function solves a USL model using non linear regression with least
#' squares. It uses the function \code{\link{nls}} with the "\code{port}"
#' algorithm to perform the calculation. All restrictions of the algorithm
#' apply.
#'
#' @param model A data frame with two columns containing the values of the
#'   predictor variable in the first column and the values of the response
#'   variable in the second column.
#'
#' @return A list containing three elements: the model coefficients alpha,
#'   beta and gamma.
#'
#' @seealso \code{\link{usl}}
#' @keywords internal
#'
usl.solve.nls <- function(model) {
  names(model) <- c("x", "y")

  gamma.start <- max(model$y / model$x)

  model.fit <- nls(y ~ (gamma * x)/(1 + alpha * (x-1) + beta * x * (x-1)),
                   data = model,
                   start = c(gamma = gamma.start, alpha = 0.01, beta = 0.0001),
                   algorithm = "port",
                   lower = c(gamma = 0, alpha = 0, beta = 0),
                   upper = c(gamma = Inf, alpha = 1, beta = 1))

  alpha = coef(model.fit)[['alpha']]
  beta  = coef(model.fit)[['beta']]
  gamma = coef(model.fit)[['gamma']]

  return(list(alpha = alpha, beta = beta, gamma = gamma))
}


##############################################################################
#' Solve a USL model using non linear regression
#'
#' This function solves a USL model using non linear regression with least
#' squares. It uses the function \code{\link{nlxb}} from the \pkg{nlsr}
#' package to perform the calculation.
#'
#' @param model A data frame with two columns containing the values of the
#'   predictor variable in the first column and the values of the response
#'   variable in the second column.
#'
#' @return A list containing three elements: the model coefficients alpha,
#'   beta and gamma.
#'
#' @seealso \code{\link{usl}}
#'
#' @references John C. Nash. nlsr: Functions for nonlinear least squares
#'   solutions, 2017. R package version 2017.6.18.
#'
#' @importFrom nlsr nlxb
#' @importFrom utils capture.output
#' @keywords internal
#'
usl.solve.nlxb <- function(model) {
  names(model) <- c("x", "y")

  gamma.start <- max(model$y / model$x)

  log <- capture.output({
    model.fit <- nlxb(y ~ (gamma * x)/(1 + alpha * (x-1) + beta * x * (x-1)),
                      data = model,
                      start = c(gamma = gamma.start, alpha = 0.01, beta = 0.0001),
                      lower = c(gamma = 0, alpha = 0, beta = 0),
                      upper = c(gamma = Inf, alpha = 1, beta = 1))
  })

  alpha = model.fit$coefficients[['alpha']]
  beta  = model.fit$coefficients[['beta']]
  gamma = model.fit$coefficients[['gamma']]

  return(list(alpha = alpha, beta = beta, gamma = gamma))
}


##############################################################################
#' Create a model for the Universal Scalability Law
#'
#' \code{usl} is used to create a model for the Universal Scalability Law.
#'
#' The Universal Scalability Law is used to forcast the scalability of
#' either a hardware or a software system.
#'
#' The USL model works with one independent variable (e.g. virtual users,
#' processes, threads, ...) and one dependent variable (e.g. throughput, ...).
#' Therefore the model formula must be in the simple
#' "\code{response ~ predictor}" format.
#'
#' The model produces two main coefficients as result: \code{alpha} models the
#' contention and \code{beta} the coherency delay of the system. The third
#' coefficient \code{gamma} estimates the value of the dependent variable
#' (e.g. throughput) for the single user/process/thread case. It therefore
#' corresponds to the scale factor calculated in previous versions of the
#' \code{usl} package.
#'
#' The function \code{\link{coef}} extracts the coefficients from the model
#' object.
#'
#' The argument \code{method} selects which solver is used to solve the
#' model:
#'
#' \itemize{
#'   \item "\code{nls}" for a nonlinear regression model. This method
#'     estimates all coefficients \code{alpha}, \code{beta} and \code{gamma}.
#'     The R base function \code{\link{nls}} with the "\code{port}" algorithm
#'     is used internally to solve the model. So all restrictions of the
#'     "\code{port}" algorithm apply.
#'   \item "\code{nlxb}" for a nonliner regression model using the function
#'     \code{\link{nlxb}} from the \code{\link{nlsr}} package. This method
#'     also estimates all three coefficients. It is expected to be more robust
#'     than the \code{nls} method.
#'   \item "\code{default}" for the default method using a transformation
#'     into a 2nd degree polynom has been removed with the implementation
#'     of the model using three coefficients in the \pkg{usl} package 2.0.0.
#'     Calling the "\code{default}" method will internally dispatch to the
#'     "\code{nlxb}" solver instead.
#' }
#'
#' The Universal Scalability Law can be expressed with following formula.
#' \code{C(N)} predicts the relative capacity of the system for a given
#' load \code{N}:
#'
#' \deqn{C(N) = \frac{\gamma N}{1 + \alpha (N - 1) + \beta N (N - 1)}}{C(N) = N / (1 + \alpha * (N - 1) + \beta * N * (N - 1))}
#'
#' @param formula An object of class "\code{\link{formula}}" (or one that
#'   can be coerced to that class): a symbolic description of the model to be
#'   analyzed. The details of model specification are given under 'Details'.
#' @param data A data frame, list or environment (or object coercible by
#'   as.data.frame to a data frame) containing the variables in the model.
#'   If not found in data, the variables are taken from
#'   \code{environment(formula)}, typically the environment from which
#'   \code{usl} is called.
#' @param method Character value specifying the method to use. The possible
#'   values are described under 'Details'.
#'
#' @return An object of class USL.
#'
#' @seealso \code{\link{efficiency,USL-method}},
#'   \code{\link{scalability,USL-method}},
#'   \code{\link{peak.scalability,USL-method}},
#'   \code{\link{summary,USL-method}},
#'   \code{\link{sigma,USL-method}}
#'   \code{\link{predict,USL-method}},
#'   \code{\link{overhead,USL-method}},
#'   \code{\link{confint,USL-method}},
#'   \code{\link{coef}},
#'   \code{\link{fitted}},
#'   \code{\link{residuals}},
#'   \code{\link{df.residual}}
#'
#' @references Neil J. Gunther. Guerrilla Capacity Planning: A Tactical
#'   Approach to Planning for Highly Scalable Applications and Services.
#'   Springer, Heidelberg, Germany, 1st edition, 2007.
#'
#' @references John C. Nash. nlsr: Functions for nonlinear least squares
#'   solutions, 2017. R package version 2017.6.18.
#'
#' @examples
#' require(usl)
#'
#' data(raytracer)
#'
#' ## Create USL model for "throughput" by "processors"
#' usl.model <- usl(throughput ~ processors, raytracer)
#'
#' ## Show summary of model parameters
#' summary(usl.model)
#'
#' ## Show complete list of efficiency parameters
#' efficiency(usl.model)
#'
#' ## Extract coefficients for model
#' coef(usl.model)
#'
#' ## Calculate point of peak scalability
#' peak.scalability(usl.model)
#'
#' ## Plot original data and scalability function
#' plot(raytracer)
#' plot(usl.model, add=TRUE)
#'
#' @export
#'
usl <- function(formula, data, method = "default") {
  ## canonicalize the arguments
  formula <- as.formula(formula)

  if (length(formula) < 3L) {
    stop("'formula' must be a 3-part formula")
  }

  if(!is.data.frame(data) && !is.environment(data)) {
    stop("'data' must be a data frame or an environment")
  }

  # Check parameter and variable names from formula
  var.names <- all.vars(formula)

  if (length(var.names) != 2L) {
    stop("'formula' must contain exactly 2 variables")
  }

  # Create model frame
  call <- match.call()
  frame <- match.call(expand.dots = FALSE)
  m <- match(c("formula", "data"), names(frame), 0)
  frame <- frame[c(1, m)]
  frame$na.action <- "na.omit"
  frame$drop.unused.levels <- TRUE
  frame[[1]] <- as.name("model.frame")
  frame <- eval(frame, parent.frame())

  # Verify there are enough values to do the calculation
  if (nrow(frame) < 6) {
    warning("'data' has only a few values; the result might not be accurate")
  }

  # Extract terms from the formula and get the names of the
  # predictor and response variables given by the user
  mt <- attr(frame, "terms")

  regr <- var.names[-attr(mt, "response")] # predictor
  resp <- var.names[attr(mt, "response")]  # response

  model.input <- data.frame(frame[regr], frame[resp])

  # Choose solver function
  sel <- switch(method, nls=2, 1)
  usl.solve <- switch(sel, usl.solve.nlxb, usl.solve.nls)

  # Solve the model for the model frame
  model.result <- usl.solve(model.input)

  # Create object for class USL
  .Object <- new(Class = "USL", call, frame, regr, resp,
                 model.result[['alpha']],
                 model.result[['beta']],
                 model.result[['gamma']])

  # Finish building the USL object
  nam <- row.names(frame)

  y.obs <- frame[, resp, drop = TRUE]
  y.fit <- predict(.Object)
  y.res <- y.obs - y.fit

  .Object@fitted    <- structure(y.fit, names = nam)
  .Object@residuals <- structure(y.res, names = nam)

  # The following estimation of the standard errors is based on the
  # source code of the nls() function in R base.
  # See also: Nonlinear Regression and Nonlinear Least Squares,
  # Appendix to An R and S-PLUS Companion to Applied Regression, John
  # Fox, January 2002

  # residual variance
  df <- df.residual(.Object)
  rv <- ifelse(df <= 0, NaN, sum(y.res ^ 2) / df)
  
  # residual standard deviation
  .Object@sigma <- sqrt(rv)

  # gradient matrix
  grad <- gradient.usl(.Object)

  XtXinv <- solve(t(grad) %*% grad)

  # standard error of coefficients
  .Object@coef.std.err <- sqrt(diag(XtXinv) * rv)

  return(.Object)
}
