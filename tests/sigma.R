# sigma.R --- Test function sigma

library(usl)

data(specsdm91)

u <- usl(throughput ~ load, specsdm91)

signif(sigma(u), digits=3)
