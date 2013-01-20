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
#' Plot the result from an Universal Scalability Law model
#' 
#' Draw a line plot for an Universal Scalability Law model.
#' 
#' @param x The USL object
#' @param ... Other graphical parameters (see \code{\link{par}})
#' 
#' @name plot
#' @aliases plot,USL-method
#' @docType methods
#' @rdname plot-methods
#' @export
setMethod(
  f = "plot",
  signature = "USL",
  definition = function(x, y = NULL, from = NULL, to = NULL, xlab = NULL, ylab = NULL, ...) {
    # Get the function to calculate scalability for the model
    .func <- scalability(x)
    
    # Take range from the model if not specified
    if (missing(from)) from <- min(x@frame[, x@regr])
    if (missing(to)) to <- max(x@frame[, x@regr])
    
    # Titles for axis
    if (missing(xlab)) xlab <- x@regr
    if (missing(ylab)) ylab <- x@resp
    
    # Plot the scalability function
    plot(x=.func, from=from, to=to, xlab=xlab, ylab=ylab, ...)
  }
)
