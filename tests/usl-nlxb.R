# usl-nlxb.R --- Test method nlxb

library(usl)

options(scipen=9)

data(specsdm91)

u <- usl(throughput ~ load, specsdm91, method = "nlxb")

signif(coef(u)[['alpha']], 3)
signif(coef(u)[['beta']], 3)
signif(coef(u)[['gamma']], 3)
