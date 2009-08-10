### Name: pie.labels
### Title: Place labels on a pie chart
### Aliases: pie.labels
### Keywords: misc

### ** Examples

 pieval<-c(2,4,6,8)
 plot(1:5,type="n",axes=FALSE)
 box()
 bisect.angles<-floating.pie(3,3,pieval)
 pie.labels(3,3,bisect.angles,c("two","four","six","eight"))



