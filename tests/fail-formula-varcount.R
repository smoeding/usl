# fail-formula-varcount.R --- Fail if formula has wrong number of variables

library(usl)

data(airquality)

try(usl(Ozone ~ Wind + Temp, data=airquality))
