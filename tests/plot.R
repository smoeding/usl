# plot.R --- Test function plot

library(usl)

data(specsdm91)

u <- usl(throughput ~ load, specsdm91)

plot(u, ylim=c(0, 3000), bounds=TRUE)
