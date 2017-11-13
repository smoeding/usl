# residuals.R --- Test function residuals

library(usl)

data(specsdm91)

options(digits=3)

u <- usl(throughput ~ load, specsdm91)

residuals(u)
