# usl-nlxb.R --- Test method nlxb

library(usl)

data(specsdm91)

usl(throughput ~ load, specsdm91, method = "nlxb")
