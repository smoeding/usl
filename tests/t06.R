#
# Test import of package nlmrt
#

library(usl)

data(specsdm91)

u <- usl(throughput ~ load, specsdm91, method = "nlxb", R=3)
