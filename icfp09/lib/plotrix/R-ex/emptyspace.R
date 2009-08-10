### Name: emptyspace
### Title: Find an empty space on a plot.
### Aliases: emptyspace
### Keywords: misc

### ** Examples

 x<-rnorm(10)
 y<-rnorm(10)
 plot(x,y,main="Find the empty space",xlab="X",ylab="Y")
 es<-emptyspace(x,y)
 boxed.labels(es,labels="Here is the\nempty space")



