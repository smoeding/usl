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
#' Generate an object from the SummaryUSL class
#'
#' Initialize the object.
#' 
#' @param .Object The SummaryUSL object to initialize.
#' 
#' @param call The formula used to create the USL model.
#' 
#' @param coefficients A vector containing the coefficients of the USL model.
#'
#' @name initialize
#' @aliases initialize,SummaryUSL-method 
#' @docType methods
#' @rdname initialize-methods
#' 
setMethod(
  f = "initialize",
  signature = "SummaryUSL",
  definition = function(.Object, call, coefficients) {
    if (!missing(call))         .Object@call         <- call
    if (!missing(coefficients)) .Object@coefficients <- coefficients
    
    return(.Object)
  }
)

#' Generate an object from the USL class
#' 
#' Initialize the object.
#' 
#' @param .Object The SummaryUSL object to initialize.
#' 
#' @param call The formula used to create the USL model.
#' 
#' @name initialize
#' @aliases initialize,USL-method 
#' @docType methods
#' @rdname initialize-methods
#' 
setMethod(
  f = "initialize",
  signature = "USL",
  definition = function(.Object, call, frame, regr, resp, scale.factor, sigma, kappa) {
    coefficients <- c(sigma, kappa)
    names(coefficients) <- c("sigma", "kappa")
    
    .Object <- callNextMethod(.Object, call, coefficients)
    
    .Object@frame        <- frame
    .Object@regr         <- regr
    .Object@resp         <- resp
    .Object@scale.factor <- scale.factor
    
    # Call inspector
    validObject(.Object)
    return(.Object)
  }
)
