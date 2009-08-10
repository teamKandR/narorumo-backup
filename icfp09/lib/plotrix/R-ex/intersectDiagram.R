### Name: intersectDiagram
### Title: Display set intersections
### Aliases: intersectDiagram
### Keywords: misc

### ** Examples

 # create a matrix where each row represents an element and
 # a 1 (or TRUE) in each column indicates that the element is a member
 # of that set.
 druguse<-matrix(c(sample(c(0,1),200,TRUE),
  sample(c(0,1),200,TRUE),
  sample(c(0,1),200,TRUE),
  sample(c(0,1),200,TRUE)),ncol=4)
 colnames(druguse)<-c("Alc","Tob","THC","Amp")
 druglist<-makeIntersectList(druguse)
 # first display it as counts
 intersectDiagram(druglist)
 # then as percent with non.members, passing the initial matrix
 intersectDiagram(druguse,pct=TRUE,show.nulls=TRUE)



