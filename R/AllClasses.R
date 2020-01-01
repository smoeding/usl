# Copyright (c) 2013-2020 Stefan Moeding
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
#' Class "\code{USL}" for Universal Scalability Law models
#'
#' This class encapsulates the Universal Scalability Law. Use the function
#' \code{\link{usl}} to create new objects from this class.
#'
#' @slot frame The model frame.
#' @slot call The call used to create the model.
#' @slot regr The name of the regressor variable.
#' @slot resp The name of the response variable.
#' @slot coefficients The coefficients alpha, beta and gamma of the model.
#' @slot coef.std.err The standard errors for the coefficients alpha and beta.
#' @slot coef.names A vector with the names of the coefficients.
#' @slot fitted The fitted values of the model. This is a vector.
#' @slot residuals The residuals of the model. This is a vector.
#' @slot df.residual The degrees of freedom of the model.
#' @slot r.squared Coefficient of determination of the model.
#' @slot adj.r.squared Adjusted coefficient of determination.
#' @slot efficiency The efficiency, e.g. speedup per processor.
#' @slot na.action The \code{na.action} used by the model.
#'
#' @seealso \code{\link{usl}}
#'
#' @name USL-class
#' @exportClass USL
setClass("USL",
         representation(frame         = "data.frame",
                        call          = "call",
                        regr          = "character",
                        resp          = "character",
                        coefficients  = "vector",
                        coef.std.err  = "vector",
                        coef.names    = "vector",
                        fitted        = "vector",
                        residuals     = "vector",
                        df.residual   = "integer",
                        r.squared     = "numeric",
                        adj.r.squared = "numeric",
                        efficiency    = "vector",
                        na.action     = "character"),
         prototype(coef.names    = c("alpha", "beta", "gamma"),
                   df.residual   = 0L,
                   r.squared     = 0,
                   adj.r.squared = 0,
                   na.action     = "na.omit"),
         validity = function(object) {
           err <- character()

           if (length(object@regr) == 0) {
             msg <- "name of regressor variable cannot be empty"
             err <- c(err, msg)
           }

           if (length(object@resp) == 0) {
             msg <- "name of regsponse variable cannot be empty"
             err <- c(err, msg)
           }

           if ((object@r.squared < 0) || (object@r.squared > 1)) {
             msg <- "r.squared must be 0 <= r.squared <= 1"
             err <- c(err, msg)
           }

           if ((object@adj.r.squared < 0) || (object@adj.r.squared > 1)) {
             msg <- "adj.r.squared must be 0 <= adj.r.squared <= 1"
             err <- c(err, msg)
           }

           if (length(err) == 0) return(TRUE) else return(err)
         })
