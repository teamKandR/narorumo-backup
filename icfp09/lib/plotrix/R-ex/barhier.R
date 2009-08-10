### Name: barhier
### Title: Display a set of hierarchically associated variables
### Aliases: barhier
### Keywords: misc

### ** Examples

 cat1<-sample(LETTERS[1:4],40,TRUE)
 cat2<-paste(cat1,sample(1:4,40,TRUE),sep="")
 cat3<-paste(cat2,sample(letters[1:4],40,TRUE),sep="")
 hcats<-data.frame(cat1,cat2,cat3)
 barhier(hcats,col=c("#ff8080","#dddd80","#80ff80","#8080ff"),
  main="Hierarchical count chart",fade=TRUE)



