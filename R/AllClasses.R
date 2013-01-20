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
#' Class "\code{SummaryUSL}" for summaries of class "\code{USL-class}"
#'
#' \code{SummaryUSL} is a class to store the summary of an
#' \code{\link{USL-class}}.
#'
#' @section Slots: \describe{ \item{\code{call}:}{The call to calculate the
#'  model} \item{\code{coefficients}:}{The coefficients sigma and kappa of
#'  the model} }
#'  
#' @name SummaryUSL-class
#' @rdname SummaryUSL-class
#' @exportClass SummaryUSL
setClass("SummaryUSL",
         representation(call         = "call",
                        coefficients = "vector"))


##############################################################################
#' Class "\code{USL}" for Universal Scalability Law models
#' 
#' This class encapsulates the Universal Scalability Law.
#' 
#' @name USL-class
#' @rdname USL-class
#' @exportClass USL
setClass("USL",
         representation(frame = "data.frame",
                        regr  = "character",
                        resp  = "character",
                        scale.factor = "numeric"),
         validity = function(object) {
           if (object@scale.factor < 0)
             stop("scale.factor must be positive")

           return(TRUE)
         },
         contains="SummaryUSL")
