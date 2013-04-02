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
#' Confidence Intervals for USL model parameters
#'
#' Estimate confidence intervals for one or more parameters in a USL model.
#'
#' Confidence intervals are estimated by using a bootstrap. The bootstrap
#' generates a series of random selections with replacement for the original
#' data and calculates the model parameters sigma and kappa for every
#' selection. The set of parameters are used to estimate confidence intervals.
#'
#' The parameter "\code{type}" determines the type of interval that is
#' calculated. See \code{\link{boot.ci}} for details.
#'
#' The bootstrap is computed once in the \code{\link{usl}} function so
#' calling \code{confint} multiple times for a specific USL object will
#' return identical results.
#'
#' Calculating confidence intervals for a small number of observations is
#' unreliable. The function will print warning or error messages if the
#' calculated intervals are dubious or the estimation is not possible.
#'
#' @usage \S4method{confint}{USL}(object, parm, level = 0.95, type = "norm")
#' @param object A USL object.
#' @param parm A specification of which parameters are to be given confidence
#'   intervals, either a vector of numbers or a vector of names. If missing,
#'   all parameters are considered.
#' @param level The confidence level required.
#' @param type A character string representing the type of interval required.
#'   The value must be one of "\code{norm}", "\code{basic}", "\code{stud}",
#'   "\code{perc}" or "\code{bca}".
#'
#' @return A matrix (or vector) with columns giving lower and upper confidence
#'   limits for each parameter. These will be labelled as (1-level)/2 and
#'   1 - (1-level)/2 in \% (by default 2.5\% and 97.5\%).
#'
#' @seealso \code{\link{usl}}, \code{\link{boot.ci}}
#'
#' @examples
#' require(usl)
#'
#' data(specsdm91)
#'
#' ## Create USL model
#' usl.model <- usl(throughput ~ load, specsdm91)
#'
#' ## Print normal confidence intervals
#' confint(usl.model, type = "norm")
#'
#' @aliases confint,USL-method
#' @docType methods
#' @rdname confint-methods
#' @importFrom boot boot.ci
#' @export
#'
setMethod(
  f = "confint",
  signature = "USL",
  definition = function(object, parm, level = 0.95, type = "norm") {
    col.name <- paste(formatC(100 * c((1-level)/2, 1-(1-level)/2)), "%")
    row.name <- NULL
    ci.value <- NULL # vector with confidence interval values
    warn.msg <- NULL # warning messages thrown by boot.ci

    type.all = c("norm", "basic", "stud", "perc", "bca")

    # Verify argument 'type'
    if (!(type %in% type.all)) {
      stop(paste0("type must be one of: ", paste(type.all, collapse=", ")))
    }

    # Map boot.ci input parameter to output object element name
    type.elem <- switch(type,
                        norm="normal", basic="basic", stud="student",
                        perc="percent", bca="bca")

    # Return confidence intervals for both parameters if 'parm' is unset
    if (missing(parm)) parm <- c(1, 2)

    # Replace named parameters
    if (mode(parm) != "numeric") {
      parm <- gsub("sigma", "1", parm, ignore.case = TRUE)
      parm <- gsub("kappa", "2", parm, ignore.case = TRUE)
    }

    # Get both (1=sigma, 2=kappa) intervals from bootstrap object
    for (index in 1:2) {
      if (index %in% parm) {
        ci.obj <- tryCatch(boot.ci(object@boot, conf = level, type, index),
                           error = identity, warning = identity)

        # Stop on error
        if (inherits(ci.obj, "error")) {
          stop(ci.obj$message)
        }

        # Remember all warnings (discard duplicates)
        if (inherits(ci.obj, "warning")) {
          warn.msg <- unique(c(warn.msg, ci.obj$message))
          next # continue with calculation for next parameter
        }

        ci <- ci.obj[[type.elem]]

        # Extract the last two values from the first row in ci.value
        # and add to result vector
        ci.value <- c(ci.value, ci[1, (ncol(ci)-1):ncol(ci)])

        # Add name of parameter as rowname
        row.name <- c(row.name, switch(index, "sigma", "kappa"))
      }
    }

    # Print all warnings
    for(m in warn.msg) {
      warning(m)
    }

    # Use dummy matrix if no sensible parameters were requested
    if (length(row.name) < 1) {
      row.name <- NA
      ci.value <- c(NA, NA)
    }

    # Return confidence intervals as matrix
    matrix(ci.value, nrow = length(row.name), ncol = 2,
           byrow = TRUE, dimnames = list(row.name, col.name))
  })
