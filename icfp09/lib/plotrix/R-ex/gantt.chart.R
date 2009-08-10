### Encoding: latin1

### Name: gantt.chart
### Title: Display a Gantt chart
### Aliases: gantt.chart
### Keywords: misc

### ** Examples

 Ymd.format<-"%Y/%m/%d"
 gantt.info<-list(labels=
  c("First task","Second task","Third task","Fourth task","Fifth task"),
  starts=
  as.POSIXct(strptime(
  c("2004/01/01","2004/02/02","2004/03/03","2004/05/05","2004/09/09"),
  format=Ymd.format)),
  ends=
  as.POSIXct(strptime(
  c("2004/03/03","2004/05/05","2004/05/05","2004/08/08","2004/12/12"),
  format=Ymd.format)),
  priorities=c(1,2,3,4,5))
 vgridpos<-as.POSIXct(strptime(c("2004/01/01","2004/02/01","2004/03/01",
  "2004/04/01","2004/05/01","2004/06/01","2004/07/01","2004/08/01",
  "2004/09/01","2004/10/01","2004/11/01","2004/12/01"),format=Ymd.format))
 vgridlab<-
  c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
 gantt.chart(gantt.info,main="Calendar date Gantt chart (2004)",
  priority.legend=TRUE,vgridpos=vgridpos,vgridlab=vgridlab,hgrid=TRUE)



