# warn-accuracy.R --- Warn if not enough data

library(usl)

options(digits=1, scipen=7)

data(raytracer)

r <- raytracer[1:5, ]

u <- usl(throughput ~ processors, data=r)

coef(u)
