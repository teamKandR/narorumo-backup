### Name: boxed.labels
### Title: Place labels in boxes
### Aliases: boxed.labels
### Keywords: misc

### ** Examples

 x<-rnorm(10)
 y<-rnorm(10)
 plot(x,y,type="p")
 nums<-c("one","two","three","four","five","six","seven","eight","nine","ten")
 boxed.labels(x,y-0.1,nums)
 # now label a barplot
 xpos<-barplot(c(1,3,2,4))
 boxed.labels(xpos,0.5,nums[1:4])
 # perform a PCA on the "swiss" dataset and plot the first two components
 data(swiss)
 swiss.pca<-prcomp(swiss)
 plot(swiss.pca$rotation[,1:2],xlim=c(-1,0.2),main="PCA of swiss dataset",
  type="n")
 boxed.labels(swiss.pca$rotation[1:6],swiss.pca$rotation[7:12],ypad=1.5,
  colnames(swiss),bg=c("red","purple","blue","blue","darkgreen","red"),
  col="white")



