# Load the 'plotrix' library
library(plotrix, lib = './lib/')

coords = read.csv("scratch/meetandgreet.coords", header=FALSE)[,c(2,3)]
other.coords = read.csv("visualization-data/meetandgreet.2003.coords",header=FALSE)[,c(2,3)]

xy = rbind(coords, other.coords)
plot(xy, asp=1, main="Meet and Greet: scenario 2003", xlab="x-coordinate", ylab="y-coordinate")

# Draw a big green earth
draw.circle(0,0,6357000,nv=1000,border="green",col="green",lty="solid",lwd=1)

# Draw initial orbit radius
draw.circle(0,0,8357000.000000492,nv=1000,border="black",col=NA, lty="solid",lwd=1)

# Draw target orbit radius
draw.circle(0,0,7357000.00000093,nv=1000,border="red",col=NA, lty="solid",lwd=1)
