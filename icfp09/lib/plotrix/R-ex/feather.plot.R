### Name: feather.plot
### Title: Display vectors along a horizontal reference line.
### Aliases: feather.plot
### Keywords: misc

### ** Examples

 feather.plot(0.6+rnorm(8)/5,seq(0,7*pi/4,by=pi/4),1:8,
  main="Standard Coordinates",xlab="Time",ylab="Value")
 if(dev.interactive()) par(ask=TRUE)
 feather.plot(0.6+rnorm(8)/5,seq(0,7*pi/4,by=pi/4),1:8,
  main="Meteorological Coordinates",xlab="Time",ylab="Value",
  fp.type="m",xlabels=TRUE)
 par(ask=FALSE)



