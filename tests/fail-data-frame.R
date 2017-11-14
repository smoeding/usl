# fail-data-frame.R --- Fail if argument is not a data frame

library(usl)

data(AirPassengers)

try(usl(Jan ~ Feb + Mar, data=AirPassengers))
