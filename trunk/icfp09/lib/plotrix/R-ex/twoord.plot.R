### Name: twoord.plot
### Title: Plot with two ordinates
### Aliases: twoord.plot
### Keywords: misc

### ** Examples

 twoord.plot(2:10,seq(3,7,by=0.5)+rnorm(9),
  1:15,rev(60:74)+rnorm(15),xlab="Sequence",
  ylab="Ascending values",rylab="Descending values",
  main="Plot with two ordinates - points and lines")
 twoord.plot(2:10,seq(3,7,by=0.5)+rnorm(9),
  1:15,rev(60:74)+rnorm(15),xlab="Sequence",
  ylab="Ascending values",rylab="Descending values",
  main="Plot with two ordinates - bars on the left",
  type=c("bar","l"),lcol=3,rcol=4)
 twoord.plot(2:10,seq(3,7,by=0.5)+rnorm(9),
  1:15,rev(60:74)+rnorm(15),xlab="Sequence",
  ylab="Ascending values",rylab="Descending values",
  main="Plot with two ordinates - bars on the right",
  type=c("b","bar"),lcol=2,rcol=NA,halfwidth=0.2)



