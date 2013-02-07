#
# Test functions coef, fitted, residuals, deviance, efficiency
#

library(usl)

data(specsdm91)

options(digits=3)

u <- usl(throughput ~ load, specsdm91)

coef(u)

fitted(u)

residuals(u)

deviance(u)

efficiency(u)
