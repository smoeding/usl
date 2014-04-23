#
# Test function predict
#

library(usl)

data(specsdm91)

u <- usl(throughput ~ load, specsdm91)

new <- data.frame(load=c(24, 48, 90, 126, 180))

predict(u, new)
