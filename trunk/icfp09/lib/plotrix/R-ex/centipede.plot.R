### Name: centipede.plot
### Title: Display a centipede plot
### Aliases: centipede.plot
### Keywords: misc

### ** Examples

 testcp<-list("",40)
 for(i in 1:40) testcp[[i]]<-rnorm(sample(1:8,1)*50)
 segs<-get.segs(testcp)
 centipede.plot(segs,main="Test centipede plot",vgrid=0)



