# fitted.R --- Test function fitted

library(usl)

data(specsdm91)

options(digits=3)

u <- usl(throughput ~ load, specsdm91)

fitted(u)
