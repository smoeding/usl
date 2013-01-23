#
# Test function coef
#

library(usl)

d <- data.frame(load = c(1, 3, 4, 12, 24, 48),
                tput = c(1, 1.090909, 1, 0.5217391, 0.2944785, 0.1566068))

u <- usl(tput ~ load, d)

signif(coef(u)[['sigma']], 4)
signif(coef(u)[['kappa']], 4)
