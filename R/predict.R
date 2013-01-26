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
#' Predict method for Universal Scalability Law models
#'
#' \code{predict} is a function for predictions of the scalability of a
#' system modeled with the Universal Scalability Law.
#'
#' The parameters \code{sigma} or \code{kappa} are useful to do a what-if
#' analysis. Setting these parameters override the model parameters and show
#' how the system would behave with a different contention or coherency delay
#' parameter.
#'
#' \code{predict} internally uses the function returned by
#' \code{\link{scalability}} to calculate the result.
#'
#' @usage \S4method{predict}{USL}(object, newdata, sigma, kappa)
#' @param object A USL model object for which prediction is desired.
#' @param newdata An optional data frame in which to look for variables
#'   with which to predict. If omitted, the fitted values are used.
#' @param sigma Optional parameter to be used for evaluation instead of the
#'   parameter computed for the model.
#' @param kappa Optional parameter to be used for evaluation instead of the
#'   parameter computed for the model.
#'
#' @return \code{predict} produces a vector of predictions.
#'
#' @seealso \code{\link{usl}}, \code{\link{scalability}},
#'   \code{\link{USL-class}}
#'
#' @references N. J. Gunther. Guerrilla Capacity Planning. Springer-Verlag,
#'   Heidelberg, Germany, 2007.
#'
#' @examples
#' require(usl)
#'
#' data(raytracer)
#'
#' ## Print predicted result from USL model for demo dataset
#' predict(usl(throughput ~ processors, raytracer))
#'
#' @aliases predict,USL-method
#' @docType methods
#' @rdname predict-methods
#' @export
#'
setMethod(
  f = "predict",
  signature = "USL",
  definition = function(object, newdata, sigma, kappa) {
    # Predict for the initial data used to create the model
    # if no data frame 'newdata' is given as parameter
    if (missing(newdata)) newdata <- object@frame

    if (missing(sigma)) sigma <- coef(object)[['sigma']]
    if (missing(kappa)) kappa <- coef(object)[['kappa']]

    # Extract regressor variable from data frame
    x <- newdata[, object@regr, drop=TRUE]

    # Calculate values (ignore NA)
    y <- scalability(object, sigma, kappa)(x)

    return(structure(y, names=row.names(newdata)))
  }
)
