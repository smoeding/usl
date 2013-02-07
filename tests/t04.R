#
# Test failure when normalization is not possible
#

library(usl)

try(usl(throughput ~ load,
        data.frame(load=seq(2,8), throughput=2*seq(2,8))))
