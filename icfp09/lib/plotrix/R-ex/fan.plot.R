### Name: fan.plot
### Title: Display a fan plot.
### Aliases: fan.plot
### Keywords: misc

### ** Examples

 # IUCN counts of threatened species by geographical area
 iucn.df<-data.frame(area=c("Africa","Asia","Europe","N&C America",
  "S America","Oceania"),threatened=c(5994,7737,1987,4716,5097,2093))
 fan.plot(iucn.df$threatened,
  labels=paste(iucn.df$area,iucn.df$threatened,sep="-"),
  main="Threatened species by geographical area",ticks=276)
 # expand the plot to a semicircle
 fan.plot(iucn.df$threatened,max.span=pi,
  labels=paste(iucn.df$area,iucn.df$threatened,sep="-"),
  main="Threatened species by geographical area",ticks=276)
 # expand further to 3/4 of a circle
 fan.plot(iucn.df$threatened,max.span=1.5*pi,
  labels=paste(iucn.df$area,iucn.df$threatened,sep="-"),
  main="Threatened species by geographical area",ticks=276)



