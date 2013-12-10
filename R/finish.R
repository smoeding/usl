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
#' Finish USL object after initialize
#'
#' \code{finish} updates a USL object and sets additional parameters after
#' the object has been initialized.
#'
#' This is a package internal function.
#'
#' @param .Object A USL model object returned from \code{new}.
#'
#' @return An object of class USL with updated model parameters.
#'
#' @seealso \code{\link{usl}}, \code{\link{initialize,USL-method}}
#'
#' @keywords internal
#'
finish <- function(.Object) {
  nam <- row.names(.Object@frame)
  y.observed <- .Object@frame[, .Object@resp, drop=TRUE]

  .Object@fitted    <- structure(predict(.Object), names = nam)
  .Object@residuals <- structure(y.observed - .Object@fitted, names = nam)
  .Object@deviance  <- sum(.Object@residuals ^ 2)

  .Object@r.squared <- 1 - (.Object@deviance / sum((y.observed - mean(y.observed)) ^ 2))

  n <- length(y.observed) # sample size
  p <- 1                  # number of regressors
  .Object@adj.r.squared <- 1 - (1 - .Object@r.squared) * ((n-1) / (n-p-1))

  return(.Object)
}
