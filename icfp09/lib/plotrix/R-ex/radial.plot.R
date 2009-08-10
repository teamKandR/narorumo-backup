### Name: radial.plot
### Title: Plot values on a circular grid of 0 to 2*pi radians.
### Aliases: radial.plot
### Keywords: misc

### ** Examples

 testlen<-rnorm(10)*2+5
 testpos<-seq(0,18*pi/10,length=10)
 testlab<-letters[1:10]
 oldpar<-radial.plot(testlen,testpos,main="Test Radial Lines",line.col="red",lwd=3)
 testlen<-c(sin(seq(0,1.98*pi,length=100))+2+rnorm(100)/10)
 testpos<-seq(0,1.98*pi,length=100)
 radial.plot(testlen,testpos,rp.type="p",main="Test Polygon",line.col="blue")
 # now do a 12 o'clock start with clockwise positive
 radial.plot(testlen,testpos,start=pi/2,clockwise=TRUE,
  rp.type="s",main="Test Symbols (clockwise)",
  point.symbols=16,point.col="green",show.centroid=TRUE)
 # one without the circular grid and multiple polygons
 # see the "diamondplot" function for variation on this
 posmat<-matrix(sample(2:9,30,TRUE),nrow=3)
 radial.plot(posmat,labels=paste("X",1:10,sep=""),rp.type="p",
  main="Spiderweb plot",line.col=2:4,show.grid=FALSE,lwd=1:3,
  radial.lim=c(0,10))
 # dissolved ions in water
 ions<-c(3.2,5,1,3.1,2.1,4.5)
 ion.names<-c("Na","Ca","Mg","Cl","HCO3","SO4")
 radial.plot(ions,labels=ion.names,rp.type="p",main="Dissolved ions in water",
  grid.unit="meq/l",radial.lim=c(0,5),poly.col="yellow")
 par(xpd=oldpar$xpd,mar=oldpar$mar,pty=oldpar$pty)
 # reset the margins
 par(mar=c(5,4,4,2))



