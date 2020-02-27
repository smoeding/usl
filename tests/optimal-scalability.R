# optimal-scalability.R --- Test function optimal.scalability

library(usl)

data(specsdm91)

u <- usl(throughput ~ load, specsdm91)

# Calculate where optimal scalability is reached
stopifnot(all.equal(optimal.scalability(u), 36.06401, 0.0001))

# Calculate scalability for different coefficients
stopifnot(all.equal(optimal.scalability(u, 0.001, 0.00001), 1000, 0.0001))
