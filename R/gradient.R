# Copyright (c) 2014-2019 Stefan Moeding
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
#' Calculate gradient for the universal scalability function
#'
#' The implementation of this function has been adopted from the generated
#' output of the \code{\link{deriv}} function.
#'
#' @param x The USL object.
#'
#' @return The gradient matrix.
#'
#' @seealso \code{\link{usl}}
#'
#' @keywords internal
#'
gradient.usl <- function(x) {
  alpha = x@coefficients['alpha']
  beta  = x@coefficients['beta']
  gamma = x@coefficients['gamma']
  n = x@frame[, x@regr, drop = TRUE]

  # Based on the output of:
  # deriv(~ (gamma * n) / (1 + (alpha * (n-1)) + (beta * n * (n-1))), # rhs
  #       c('alpha', 'beta', 'gamma'),                                # params
  #       function(alpha, beta, gamma, n){})                          # args

  expr1 <- gamma * n
  expr2 <- n - 1
  expr3 <- 1 + (alpha * expr2) + (beta * n * expr2)
  expr4 <- expr3 ^ 2

  grad.alpha <- -(expr1 * expr2 / expr4)
  grad.beta  <- -(expr1 * (n * expr2) / expr4)
  grad.gamma <- n / expr3

  matrix(c(grad.alpha, grad.beta, grad.gamma),
         nrow = length(n),
         dimnames = list(1:length(n), x@coef.names))
}
