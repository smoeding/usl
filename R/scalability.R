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
#' Scalability function of the USL model
#'
#' Return a function to calculate the scalability for a specific model.
#'  
#' @param object An USL object
#' 
#' @return A function with parameter \code{x} that calculates the value
#'   of the specific model.
#'
#' @name scalability
#' @aliases scalability,USL-method
#' @docType methods
#' @rdname scalability-methods
#' @export
setMethod(
  f = "scalability",
  signature = "USL",
  definition = function(object) {
    .func <- function(x) {
      # Formula (4.31) on page 57 of GCaP:
      cap <- x / (1 + (coef(object)[['sigma']] * (x-1)) + (coef(object)[['kappa']] * x * (x-1)))
      
      # Scale it to the measurements
      return(object@scale.factor * cap)
    }
    
    # Return the usl function
    return(.func)
  }
)


##############################################################################
#' Peak scalability value
#' 
#' Calculate the point of peak scalability for a specific model. 
#' See formula (4.33) in "Guerilla Capacity Planing".
#'
#' @param object An USL object for the model
#' 
#' @return A numeric value for the point where peak scalability is reached.
#'
#' @name peak.scalability
#' @aliases peak.scalability,USL-method
#' @docType methods
#' @rdname peak.scalability-methods
#' @export
setMethod(
  f = "peak.scalability",
  signature = "USL",
  definition = function(object) 
    sqrt((1 - coef(object)[['sigma']]) / coef(object)[['kappa']])
)
