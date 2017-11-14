# fail-formula-length.R --- Fail if length of formula is wrong

library(usl)

data(airquality)

try(usl(~ Wind + Temp, data=airquality))
