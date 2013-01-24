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
#' \code{predict} internally uses the function returned by
#' \code{\link{scalability}} to calculate the result.
#'
#' @usage \S4method{predict}{USL}(object, newdata)
#' @param object A USL model object for which prediction is desired.
#' @param newdata An optional data frame in which to look for variables
#'   with which to predict. If omitted, the fitted values are used.
#'
#' @return \code{predict} produces a vector of predictions.
#'
#' @seealso \code{\link{usl}}, \code{\link{scalability}}
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
  definition = function(object, newdata) {
    if (missing(newdata)) {
      # Predict for the initial data used to create the model
      newdata <- object@frame
    }

    # Extract regressor variable from data frame
    x <- na.omit(newdata[, object@regr, drop=TRUE])

    # Calculate values (ignore NA)
    y <- scalability(object)(x)

    return(structure(y, names=row.names(newdata)))
  }
)
