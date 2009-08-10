### Name: hierobarp
### Title: Display a nested breakdown of numeric values.
### Aliases: hierobarp
### Keywords: misc

### ** Examples

 test.df<-data.frame(Age=rnorm(100,25,10),
  Sex=sample(c("M","F"),100,TRUE),
  Marital=sample(c("D","M","S","W"),100,TRUE),
  Employ=sample(c("Full Time","Part Time","Unemployed"),100,TRUE))
 test.col<-list(Overall="green",Employ=c("purple","orange","brown"),
  Marital=c("#1affd8","#caeecc","#f7b3cc","#94ebff"),Sex=c(2,4))
 hierobarp(formula=Age~Sex+Marital+Employ,data=test.df,ylab="Mean age (years)",
  main="Show only the final breakdown",errbars=TRUE,col=test.col$Sex)
 # set up functions for 20 and 80 percentiles
 q20<-function(x,na.rm=TRUE) return(quantile(x,probs=0.2,na.rm=TRUE))
 q80<-function(x,na.rm=TRUE) return(quantile(x,probs=0.8,na.rm=TRUE))
 # show the asymmetric dispersion measures
 hierobarp(formula=Age~Sex+Marital+Employ,data=test.df,ylab="Mean age (years)",
  main="Use median and quantiles for dispersion",
  num.desc=c("median","q80","q20","valid.n"),errbars=TRUE,col=test.col$Sex)
 hierobarp(formula=Age~Sex+Marital+Employ,data=test.df,ylab="Mean age (years)",
  main="Show the entire hierarchical breakdown",col=test.col,showall=TRUE,
  showbrklab=TRUE)
 ## Not run: 
##D   # example of a legend that might be included, needs a lot of space
##D   legend(2.5,34,c("Overall","Full time","No work","Part time","Divorced",
##D   "Married","Single","Widowed","Female","Male"),
##D    fill=unlist(test.col))
##D  
## End(Not run)



