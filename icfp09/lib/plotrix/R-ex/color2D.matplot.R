### Name: color2D.matplot
### Title: Display a numeric matrix as color matrix
### Aliases: color2D.matplot
### Keywords: misc

### ** Examples

 x<-matrix(rnorm(1024)+sin(seq(0,2*pi,length=1024)),nrow=32)
 color2D.matplot(x,c(1,0),c(0,0),c(0,1),show.legend=TRUE,
  xlab="Columns",ylab="Rows",main="2D matrix plot")
 # generate colors that show negative values in red and positive in green
 cellcol<-matrix(rep("#000000",1024),nrow=32)
 cellcol[x<0]<-color.scale(x[x<0],c(1,0.8),c(0,0.8),0)
 cellcol[x>0]<-color.scale(x[x>0],0,c(0.8,1),c(0.8,0))
 # now do hexagons without borders
 color2D.matplot(x,cellcolors=cellcol,xlab="Columns",ylab="Rows",
  do.hex=TRUE,main="2D matrix plot (hexagons)",border=NA)
 # for this one, we have to do the color legend separately
 # because of the two part color scaling
 legval<-seq(min(x),max(x),length.out=6)
 legcol<-rep("#000000",6)
 legcol[legval<0]<-color.scale(legval[legval<0],c(1,0.8),c(0,0.8),0)
 legcol[legval>0]<-color.scale(legval[legval>0],0,c(0.8,1),c(0.8,0))
 color.legend(0,-5,6,-4,round(c(min(x),0,max(x)),1),rect.col=legcol)
 # do a color only association plot
 xt<-table(sample(1:10,100,TRUE),sample(1:10,100,TRUE))
 observed<-xt[,rev(1:dim(xt)[2])]
 expected<-outer(rowSums(observed),colSums(observed),"*")/sum(xt)
 deviates<-(observed-expected)/sqrt(expected)
 cellcol<-matrix(rep("#000000",100),nrow=10)
 cellcol[deviates<0]<-
  color.scale(deviates[deviates<0],c(1,0.8),c(0,0.5),0)
 cellcol[deviates>0]<-
  color.scale(deviates[deviates>0],0,c(0.7,0.8),c(0.5,0))
 color2D.matplot(x=round(deviates,2),cellcolors=cellcol,
  show.values=TRUE,main="Association plot")



