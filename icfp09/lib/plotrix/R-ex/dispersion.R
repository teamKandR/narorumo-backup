### Name: dispersion
### Title: Display a measure of dispersion
### Aliases: dispersion dispbars
### Keywords: misc

### ** Examples

 disptest<-matrix(rnorm(200),nrow=20)
 disptest.means<-rowMeans(disptest)
 row.order<-order(disptest.means)
 se.disptest<-unlist(apply(disptest,1,std.error))
 plot(disptest.means[row.order],main="Dispersion as error bars",
  ylim=c(min(disptest.means-se.disptest),max(disptest.means+se.disptest)),
  xlab="Occasion",ylab="Value")
 dispersion(1:20,disptest.means[row.order],se.disptest[row.order])
 plot(disptest.means[row.order],main="Dispersion as confidence band",
  ylim=c(min(disptest.means-se.disptest),max(disptest.means+se.disptest)),
  xlab="Occasion",ylab="Value")
 dispersion(1:20,disptest.means[row.order],se.disptest[row.order],type="l",
  fill="#eeccee",lty=2)
 # remember to redraw the points
 points(disptest.means[row.order])



