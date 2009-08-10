### Name: plot.dendrite
### Title: Plot a dendrogram of a dendrite object.
### Aliases: plot.dendrite
### Keywords: misc

### ** Examples

 sex<-sample(c("M","F"),100,TRUE)
 hair<-sample(c("Blond","Black","Brown","Red"),100,TRUE)
 eye<-sample(c("Blue","Black","Brown","Green"),100,TRUE)
 charac<-data.frame(sex=sex,hair=hair,eye=eye)
 characlist<-makeDendrite(charac)
 plot.dendrite(characlist,names(charac),main="Test dendrogram",cex=0.8)



