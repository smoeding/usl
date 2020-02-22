# confint.R --- Test function confint

library(usl)

data(raytracer)

options(digits=3, scipen=6)

r <- raytracer[1:6, ]

u <- usl(throughput ~ processors, data = r)

coef(u)

confint(u)

confint(u, parm=1)
confint(u, parm="alpha")

confint(u, parm=2)
confint(u, parm="beta")

confint(u, parm=3)
confint(u, parm="gamma")

confint(u, parm=4)
confint(u, parm="none")
