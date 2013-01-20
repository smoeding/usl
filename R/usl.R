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
#' Create a model for the Universal Scalability Law
#' 
#' \code{usl} is used to create Universal Scalability Law models. It can be
#' used to forcast the scalability of either a hardware or a software system.
#' 
#' The Universal Scalability Law has been created by Dr. Neil Gunther.
#' 
#' @param formula An object of class "\code{\link{formula}}" (or one that can be
#'   coerced to that class): a symbolic description of the model to be analyzed.
#'   The details of model specification are given under 'Details'.
#' @param data An optional data frame, list or environment (or object coercible 
#'   by as.data.frame to a data frame) containing the variables in the model. If
#'   not found in data, the variables are taken from environment(formula), 
#'   typically the environment from which usl is called.
#'   
#' @return An object of class USL.
#'   
#' @export
#' 
usl <- function(formula, data) {
  ## canonicalize the arguments
  formula <- as.formula(formula)
  
  if (length(formula) < 3L)
    stop("formula must be a 3-part formula")
  
  if(!is.data.frame(data) && !is.environment(data))
    stop("'data' must be a data frame or an environment")
  
  # Check parameter and variable names from formula
  var.names <- all.vars(formula)
  
  if (length(var.names) != 2)
    stop("formula can contain only 2 variables")
  
  # Create model frame
  call <- match.call()
  frame <- match.call(expand.dots = FALSE)
  m <- match(c("formula", "data"), names(frame), 0)
  frame <- frame[c(1, m)]
  frame$drop.unused.levels <- TRUE
  frame[[1]] <- as.name("model.frame")
  frame <- eval(frame, parent.frame())
  
  # Extract terms from the formula and get the names of the 
  # regressor and response variables given by the user
  mt <- attr(frame, "terms")
  
  regr <- var.names[-attr(mt, "response")] # regressor
  resp <- var.names[attr(mt, "response")]  # response
  
  # Sort frame by load
  frame <- frame[order(frame[regr]),]

  # Create a copy of the model frame to perform the calculations
  model <- data.frame(frame[regr], frame[resp])
  names(model) <- c("load", "throughput")
  
  # Verify that the scale factor for normalization is available
  # from the dataframe
  if (all(model$load != 1))
    stop(paste("'data' must contain a row where", regr, "= 1"))
  
  scale.factor <- model[match(1, model$load), 2]
  
  model <- within(model, {
    # normalize data
    capacity   <- throughput / scale.factor
    
    # compute efficiency
    efficiency <- capacity / load
    
    # compute deviations from linearity
    x <- load - 1
    y <- (load / capacity) - 1
  })
  
  #if (any(model$efficiency > 1))
  #  warning("efficiency for 'data' > 1")
  
  # Solve quadratic model without intercept 
  model.fit <- lm(y ~ I(x^2) + x - 1, data = model, na.action = "na.exclude")
  
  #print(summary(model.fit)$r.squared)
  #print(summary(model.fit)$adj.r.squared)
  
  # Calculate coefficients sigma & kappa used by the USL model
  sigma <- coef(model.fit)[[2]] - coef(model.fit)[[1]]
  kappa <- coef(model.fit)[[1]]
  
  # Create object for class USL and return it
  new(Class="USL", call, frame, regr, resp, scale.factor, sigma, kappa)
}
