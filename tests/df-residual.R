# df-residual.R --- Test function df.residual

library(usl)

data(specsdm91)

options(digits=3)

u <- usl(throughput ~ load, specsdm91)

df.residual(u)
