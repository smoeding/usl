# Analyze system scalability with the Universal Scalability Law in R

This is an R package to analyze system performance data with the Universal Scalability Law.

The Universal Scalability Law (USL) was developed by Dr. Neil J. Gunther. It can be used to analyze system performance data in order to learn more about the scalability limitations of the system.

Details are presented on the [authors website ](http://www.perfdynamics.com/ "www.perfdynamics.com") and in his book *Guerrilla Capacity Planning*.

## Example session

Here is an example how the package can be used:

```R
library(usl)

# Load data from the SPEC SDM91 benchmark
data(specsdm91)

specsdm91

# Analyze "throughput" by "load" for the "specsdm91" data
usl.model <- usl(throughput ~ load, specsdm91)

# Show a model summary including scalability coefficients
summary(usl.model)

# Predict the location of the maximum in the scalability function
peak.scalability(usl.model)

# Plot original data and computed scalability function
plot(specsdm91, pch=16)
plot(usl.model, col="red", add=TRUE)
```

The ```summary``` command returns the following output:

```
Call:
usl(formula = throughput ~ load, data = specsdm91)

Coefficients:
       sigma         kappa  
1.704689e-02  7.892498e-05
```

The following image shows the plotted output:

![SPEC SDM91 scalability function](http://download.moeding.net/diagrams/usl/specsdm91.png "SPEC SDM91 scalability function")
