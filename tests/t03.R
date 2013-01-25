#
# Test functions coef, fitted, residuals, deviance
#

library(usl)

data(specsdm91)

options(digits=4)

u <- usl(throughput ~ load, specsdm91)

coef(u)

fitted(u)

residuals(u)

deviance(u)
