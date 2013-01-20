##############################################################################

library(usl)

data(specsdm91)

# Show the data from the SPEC SDM91 benchmark
specsdm91

# Create usl model from data frame
usl.model <- usl(throughput ~ load, specsdm91)

# Show model summary
summary(usl.model)

# Show point of maximum scalability
peak.scalability(usl.model)

# Plot data
plot(specsdm91, pch=16)

# Add scalability function to plotted data
plot(usl.model, add=TRUE, col="red")
