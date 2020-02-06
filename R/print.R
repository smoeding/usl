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
#' Print objects of class "\code{USL}"
#'
#' \code{print} prints its argument and returns it invisibly (via
#' \code{\link{invisible}(x)}).
#'
#' @param x An object from class \code{USL}.
#' @param digits Minimal number of \emph{significant} digits, see
#'   \link{print.default}.
#' @param ... Other arguments passed to other methods.
#'
#' @return \code{print} returns the object \code{x} invisibly.
#'
#' @seealso \code{\link{usl}}, \code{\link{USL-class}}
#'
#' @examples
#' require(usl)
#'
#' data(raytracer)
#'
#' ## Print result from USL model for demo dataset
#' print(usl(throughput ~ processors, raytracer))
#'
#' @export
#'
setMethod(
  f = "print",
  signature = "USL",
  definition = function(x, digits = max(3L, getOption("digits") - 3L), ...) {
    qnames <- c("Min", "1Q", "Median", "3Q", "Max")

    cat("\nCall:\n",
        paste(deparse(x@call), sep = "\n", collapse = "\n"), "\n", sep = "")

    cat("\nEfficiency:\n")
    zz <- zapsmall(quantile(x@efficiency), digits + 1)
    print(structure(zz, names = qnames), digits = digits, ...)

    cat("\nResiduals:\n")
    zz <- zapsmall(quantile(x@residuals), digits + 1)
    print(structure(zz, names = qnames), digits = digits, ...)

    cat("\nCoefficients:\n")
    tval <- x@coefficients / x@coef.std.err
    pval <- 2 * pt(abs(tval), x@df.residual, lower.tail = FALSE)

    para <- c(x@coefficients, x@coef.std.err, tval, pval)
    rows <- attributes(x@coefficients)$names
    cols <- c("Estimate", "Std. Error", "t value", "Pr(>|t|)")

    para.mat <- matrix(para, nrow = length(x@coef.names), dimnames = list(rows, cols))

    printCoefmat(para.mat, digits = digits, print.gap = 2)

    cat("\nResidual standard error:", format(signif(x@sigma, digits)),
        "on", x@df.residual, "degrees of freedom\n")

    cat("\nScalability bounds:\n")

    cat("limit: ")
    cat(x@resp, signif(x@limit, digits), "(Amdahl asymptote)\n")

    cat("max:   ")
    if (x@coefficients[['beta']] > 0) {
      cat(x@resp, signif(x@peak[2], digits), "at ")
      cat(x@regr, signif(x@peak[1], digits), "\n")
    }
    else {
      cat("none (beta=0)\n")
    }

    cat("opt:   ")
    cat(x@resp, signif(x@optimal[2], digits), "at ")
    cat(x@regr, signif(x@optimal[1], digits), "\n")

    cat("\n")
    invisible(x)
  }
)
