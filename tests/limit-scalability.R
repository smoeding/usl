# limit-scalability.R --- Test function limit.scalability

library(usl)

data(specsdm91)

u <- usl(throughput ~ load, specsdm91)

# Calculate where limit scalability is reached
stopifnot(all.equal(limit.scalability(u), 3245.589, 0.0001))

# Calculate scalability for different coefficients
stopifnot(all.equal(limit.scalability(u, 0.001, 0.00001), 89995.23, 0.0001))
