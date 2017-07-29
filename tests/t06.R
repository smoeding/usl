#
# Test import of package nlsr
#

library(usl)

data(specsdm91)

usl(throughput ~ load, specsdm91, method = "nlxb")
