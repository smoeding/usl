# usl-default.R --- Test method default

library(usl)

options(digits=1, scipen=7)

data(raytracer)

u <- try(usl(throughput ~ processors, data=raytracer))

coef(u)
