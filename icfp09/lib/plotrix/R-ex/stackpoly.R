### Name: stackpoly
### Title: Display the columns of a matrix or data frame as stacked
###   polygons.
### Aliases: stackpoly
### Keywords: misc

### ** Examples

 testx<-matrix(abs(rnorm(100)),nrow=10)
 stackpoly(matrix(cumsum(testx),nrow=10),main="Test Stackpoly I",
  xaxlab=c("One","Two","Three","Four","Five",
  "Six","Seven","Eight","Nine","Ten"),border="black",staxx=TRUE)
 stackpoly(testx,main="Test Stackpoly II",
  xaxlab=c("One","Two","Three","Four","Five",
  "Six","Seven","Eight","Nine","Ten"),border="black",
  staxx=TRUE,stack=TRUE)
 stackpoly(rev(sort(testx-mean(testx))),main="Test Waterfall Plot",
  col="green",border="black")



