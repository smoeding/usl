# efficiency.R --- Test function efficiency

library(usl)

data(specsdm91)

options(digits=3)

u <- usl(throughput ~ load, specsdm91)

efficiency(u)
