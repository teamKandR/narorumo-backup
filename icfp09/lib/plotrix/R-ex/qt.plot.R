### Name: qt.plot
### Title: Quantity by interval plot
### Aliases: qt.plot
### Keywords: misc

### ** Examples

 # first a moderate drinker with frequent bigger sessions
 qnt<-sample(0:5,365,TRUE,prob=c(0.02,0.1,0.4,0.3,0.1,0.08))
 qtdates<-seq(as.Date("2007-01-01"),as.Date("2007-12-31"),by=1)
 qt.plot(qnt,as.numeric(qtdates),xlab="Number of days interval",
  ylab="Standard drinks per session")
 # now add monthly bigger sessions and notice how this
 qnt[c(30,60,90,120,150,180,210,240,270,300,330,360)]<-rep(4:5,length.out=12)
 qt.plot(qnt,as.numeric(qtdates),xlab="Number of days interval",
  ylab="Standard drinks per session")



