# Copyright (c) 2013 Stefan Moeding
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
#' Solve a USL model by transformation to a 2nd degree polynom
#'
#' This function solves a USL model using the transformation introduced in
#' sections 5.5.1 - 5.5.3 of GCaP.
#'
#' @param predictor A vector containing the values of the predictor variable.
#' @param response A vector containing the values of the response variable.
#'
#' @return A list containing three elements: the scale.factor of the model,
#'   the model coefficients sigma and kappa.
#'
#' @references N. J. Gunther. Guerrilla Capacity Planning. Springer-Verlag,
#'   Heidelberg, Germany, 2007.
#'
usl.solve.lm <- function(predictor, response) {
  # Create the model frame
  model <- data.frame(predictor, response)
  names(model) <- c("predictor", "response")

  # Verify that the scale factor for normalization is in the dataframe
  if (all(model$predictor != 1)) {
    stop(paste0("'data' must contain one row where '",
                names(predictor), "' = 1"))
  }

  # Calculate scale factor: get throughput for entry where load=1
  scale.factor <- model[match(1, model$predictor), ]$response

  model <- within(model, {
    # normalize data (cf. GCaP chapter 5.4)
    capacity   <- response / scale.factor

    # compute efficiency (cf. GCaP chapter 5.5.1)
    efficiency <- capacity / predictor

    # compute deviations from linearity (cf. GCaP chapter 5.5.2)
    x <- predictor - 1
    y <- (predictor / capacity) - 1
  })

  # Capacity grows more than load: can this really be?
  if (any(model$efficiency > 1)) {
    warning("'data' shows efficiency > 1; this looks almost too good to be true")
  }

  # Solve quadratic model without intercept
  model.fit <- lm(y ~ I(x^2) + x - 1, data = model)

  # Calculate coefficients sigma & kappa used by the USL model
  sigma <- coef(model.fit)[[2]] - coef(model.fit)[[1]]
  kappa <- coef(model.fit)[[1]]

  return(list(scale.factor = scale.factor, sigma = sigma, kappa = kappa))
}


usl.solve.nls <- function(predictor, response) {
  # Create the model frame
  model <- data.frame(predictor, response)
  names(model) <- c("x", "y")

  model.fit <- nls(y ~ scale.factor * x/(1 + sigma * (x-1) + kappa * x * (x-1)) - 1,
                   data = model,
                   start = c(scale.factor = 1, sigma = 0.1, kappa = 0.01),
                   algorithm = "port",
                   lower = c(0, 0, 0),
                   upper = c(Inf, 1, 1))

  scale.factor = coef(model.fit)[['scale.factor']]
  sigma = coef(model.fit)[['sigma']]
  kappa = coef(model.fit)[['kappa']]

  return(list(scale.factor = scale.factor, sigma = sigma, kappa = kappa))
}

##############################################################################
#' Create a model for the Universal Scalability Law
#'
#' \code{usl} is used to create Universal Scalability Law models. It can be
#' used to forcast the scalability of either a hardware or a software system.
#'
#' The USL model works with one independent variable (e.g. virtual users,
#' processes, threads, ...) and one dependent variable (e.g. throughput).
#' Therefore the model formula must be in the simple
#' "\code{response ~ predictor}" format.
#'
#' The the USL model produces two coefficients as result. Parameter
#' \code{sigma} models the contention and \code{kappa} the coherency delay
#' of the system.
#'
#' The Universal Scalability Law needs to transform the data into a
#' normalized form. Currently it is therefore necessary to include one data
#' item where the independent variable equals 1. The value of the associated
#' dependent variable is used as scale factor for the normalization.
#'
#' The model uses the following formula to predict the relative capacity of
#' the system for a given load \code{N}:
#'
#' \deqn{C(N) = \frac{N}{1 + \sigma (N - 1) + \kappa N (N - 1)}}{C(N) = N / (1 + \sigma * (N - 1) + \kappa * N * (N - 1))}
#'
#' The Universal Scalability Law has been created by Dr. Neil Gunther.
#'
#' @param formula An object of class "\code{\link{formula}}" (or one that
#'   can be coerced to that class): a symbolic description of the model to be
#'   analyzed. The details of model specification are given under 'Details'.
#' @param data An optional data frame, list or environment (or object
#'   coercible by as.data.frame to a data frame) containing the variables in
#'   the model. If not found in data, the variables are taken from
#'   \code{environment(formula)}, typically the environment from which
#'   \code{usl} is called.
#'
#' @return An object of class USL.
#'
#' @seealso \code{\link{scalability}}, \code{\link{peak.scalability}},
#'   \code{\link{summary}}, \code{\link{coef}}, \code{\link{fitted}}
#'   \code{\link{residuals}}, \code{\link{deviance}}
#'
#' @references N. J. Gunther. Guerrilla Capacity Planning. Springer-Verlag,
#'   Heidelberg, Germany, 2007.
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
#' ## Extract coefficients for model
#' coef(usl.model)
#'
#' ## Calculate point of peak scalability
#' peak.scalability(usl.model)
#'
#' ## Plot scalability function and original data
#' plot(raytracer)
#' plot(usl.model, add=TRUE)
#'
#' @export
#'
usl <- function(formula, data) {
  ## canonicalize the arguments
  formula <- as.formula(formula)

  if (length(formula) < 3L) {
    stop("formula must be a 3-part formula")
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

  result <- usl.solve.lm(frame[regr], frame[resp])
  #result <- usl.solve.nls(frame[regr], frame[resp])

  # Create object for class USL
  .Object <- new(Class = "USL", call, frame, regr, resp,
                 result[['scale.factor']],
                 result[['sigma']], result[['kappa']])

  # Finish the object and return it
  return(finish(.Object))
}
