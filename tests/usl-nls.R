# usl-nls.R --- Test method nls

library(usl)

options(digits=3, scipen=9)

dfr <- data.frame(load=c(1, 2,      4,      6,      8,      10),
                  tput=c(1, 1.8868, 3.0769, 3.5294, 3.5398, 3.3557))

try(u <- usl(tput ~ load, data=dfr, method="nls"))

coef(u)
