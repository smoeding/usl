# peak-scalability.R --- Test function peak.scalability

library(usl)

data(specsdm91)

u <- usl(throughput ~ load, specsdm91)

# Calculate where peak scalability is reached
stopifnot(all.equal(peak.scalability(u), 96.51956, 0.0001))

# Calculate scalability for different coefficients
stopifnot(all.equal(peak.scalability(u, 0.001, 0.00001), 316.0696, 0.0001))
