# MATH-267A
Maham Test



```R
library(MASS)
data(Cars93)
mycars <- Cars93[,c("MPG.city", "MPG.highway", "Horsepower", "Price", "Origin")]
cathistogram(continous2categorical(mycars))
```

![Sample plot using cathistogram](https://raw.githubusercontent.com/saqib-ali/MATH-267A/master/sampleplots/Rplot2.png)

