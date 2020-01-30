# scalability.R --- Test function scalability

library(usl)

data(specsdm91)

u <- usl(throughput ~ load, specsdm91)
s1 <- scalability(u)
s2 <- scalability(u, 0.001, 0.00001)
s3 <- scalability(u, 0.001, 0.00001, 100)

# Calculate scalability
signif(s1(20), 2)

# Calculate scalability for different coefficients
signif(s2(20), 2)

# Calculate scalability for different gamma
signif(s3(20), 2)
