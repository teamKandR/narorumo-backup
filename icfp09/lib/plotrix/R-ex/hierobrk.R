### Name: hierobrk
### Title: Perform a nested breakdown of numeric values.
### Aliases: hierobrk
### Keywords: misc

### ** Examples

 test.df<-data.frame(Age=rnorm(100,25,10),
  Sex=sample(c("M","F"),100,TRUE),
  Marital=sample(c("M","X","S","W"),100,TRUE),
  Employ=sample(c("FT","PT","NO"),100,TRUE))
 hierobrk(formula=Age~Sex+Marital+Employ,data=test.df)



