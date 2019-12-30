# usl-nlxb.R --- Test method nlxb

library(usl)

options(digits=3, scipen=9)

data(specsdm91)

usl(throughput ~ load, specsdm91, method = "nlxb")
