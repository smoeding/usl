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
#' Bootstrapping is used to estimate the confidence intervals.
#'
#' @usage \S4method{confint}{USL}(object, parm, level)
#' @param object A USL object.
#' @param parm A specification of which parameters are to be given confidence
#'   intervals, either a vector of numbers or a vector of names. If missing,
#'   all parameters are considered.
#' @param level The confidence level required.
#'
#' @return A matrix (or vector) with columns giving lower and upper confidence
#'   limits for each parameter. These will be labelled as (1-level)/2 and
#'   1 - (1-level)/2 in % (by default 2.5% and 97.5%).
#'
#' @seealso \code{\link{usl}}
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
  definition = function(object, parm, level = 0.95) {
    cnam <- paste(formatC(100 * c((1-level)/2, 1-(1-level)/2)), "%")
    vect <- NULL
    rnam <- NULL
    nrow <- 0

    type <- "bca"
    ci.element <- switch(type, "norm"="normal", "basic", "stud"="student", "perc"="percent", "bca"="bca")

    # Default is to return all confidence intervals
    if (missing(parm)) parm <- c(1, 2)

    if (mode(parm) != "numeric") {
      parm <- gsub("sigma", "1", parm, ignore.case = TRUE)
      parm <- gsub("kappa", "2", parm, ignore.case = TRUE)
    }

    # Get both intervals from bootstrap object (1=sigma, 2=kappa)
    for (index in c(1, 2)) {
      if (index %in% parm) {
        ci <- boot.ci(object@boot, conf = level, type, index)[[foo]]

        # Extract last two values from the first row in ci and append to vect
        vect <- c(vect, ci[1, (ncol(ci)-1):ncol(ci)])

        # Add name of parameter as rowname
        rnam <- c(rnam, switch(index, "sigma", "kappa"))
        nrow <- nrow + 1
      }
    }

    # Use dummy matrix if no sensible parameters were requested
    if (nrow < 1) {
      rnam <- NA
      nrow <- 1
      vect <- c(NA, NA)
    }

    matrix(vect, nrow, 2, byrow = TRUE, dimnames = list(rnam, cnam))
  })
