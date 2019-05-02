# peak-scalability.R --- Test function peak.scalability

library(usl)

data(specsdm91)

u <- usl(throughput ~ load, specsdm91)

# Calculate where peak scalability is reached
signif(peak.scalability(u), 2)

# Calculate scalability for different coefficients
signif(peak.scalability(u, 0.001, 0.00001), 2)
