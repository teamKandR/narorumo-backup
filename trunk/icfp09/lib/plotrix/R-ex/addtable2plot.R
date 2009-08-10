### Name: addtable2plot
### Title: Add a table of values to a plot
### Aliases: addtable2plot
### Keywords: misc

### ** Examples

 testdf<-data.frame(Before=c(10,7,5),During=c(8,6,2),After=c(5,3,4))
 rownames(testdf)<-c("Red","Green","Blue")
 barp(testdf,main="Test addtable2plot",ylab="Value",
  names.arg=colnames(testdf),col=2:4)
 # show most of the options
 addtable2plot(2,8,testdf,bty="o",display.rownames=TRUE,hlines=TRUE,
  title="The table")



