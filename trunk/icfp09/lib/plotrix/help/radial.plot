radial.plot             package:plotrix             R Documentation

_P_l_o_t _v_a_l_u_e_s _o_n _a _c_i_r_c_u_l_a_r _g_r_i_d _o_f _0 _t_o _2*_p_i _r_a_d_i_a_n_s.

_D_e_s_c_r_i_p_t_i_o_n:

     Plot numeric values as distances from the center of a circular
     field in the directions defined by angles in radians.

_U_s_a_g_e:

      radial.plot(lengths,radial.pos=NULL,labels,label.pos=NULL,radlab=FALSE,
      start=0,clockwise=FALSE,rp.type="r",label.prop=1.1,main="",xlab="",ylab="",
      line.col=par("fg"),lty=par("lty"),lwd=par("lwd"),mar=c(2,2,3,2),
      show.grid=TRUE,show.grid.labels=TRUE,show.radial.grid=TRUE,
      grid.col="gray",grid.bg="transparent",
      grid.left=FALSE,grid.unit=NULL,point.symbols=NULL,point.col=NULL,
      show.centroid=FALSE,radial.lim=NULL,poly.col=NULL,...)

_A_r_g_u_m_e_n_t_s:

 lengths: A numeric data vector or matrix. If 'lengths' is a matrix,
          the rows will be considered separate data vectors.

radial.pos: A numeric vector or matrix of positions in radians.  These
          are interpreted as beginning at the right (0 radians) and
          moving counterclockwise. If 'radial.pos' is a matrix, the
          rows must  correspond to rows of 'lengths'.

  labels: Character strings to be placed at the outer ends of the
          lines. If set to NA, will suppress printing of labels, but if
          missing, the radial positions will be used.

label.pos: The positions of the labels around the plot in radians.

  radlab: Whether to rotate the outer labels to a radial orientation.

   start: Where to place the starting (zero) point. Defaults to the 3
          o'clock position.

clockwise: Whether to interpret positive positions as clockwise from
          the starting point. The default is counterclockwise.

 rp.type: Whether to draw (r)adial lines, a (p)olygon, (s)ymbols or
          some combination of these. If 'lengths' is a matrix and
          rp.type is a vector, each row of 'lengths' can be displayed
          differently.

label.prop: The label position radius as a proportion of the  maximum
          line length.

    main: The title for the plot.

xlab,ylab: Normally x and y axis labels are suppressed.

line.col: The color of the radial lines or polygons drawn.

     lty: The line type(s) to be used for polygons or radial lines.

     lwd: The line width(s) to be used for polygons or radial lines.

     mar: Margins for the plot. Allows the user to leave space for
          legends, long labels, etc.

show.grid: Logical - whether to draw a circular grid.

show.grid.labels: Logical - whether to display labels for the grid.

show.radial.grid: Whether to draw radial lines to the plot labels.

grid.col: Color of the circular grid.

 grid.bg: Fill color of above.

grid.left: Whether to place the radial grid labels on the left side.

grid.unit: Optional unit description for the grid.

point.symbols: The symbols for plotting (as in pch).

point.col: Colors for the symbols.

show.centroid: Whether to display a centroid.

radial.lim: The range of the grid circle. Defaults to
          'range(pretty(lengths))'.

poly.col: Fill color if polygons are drawn. Use NA for no fill.

     ...: Additional arguments are passed to 'plot'.

_D_e_t_a_i_l_s:

     'radial.plot' displays a plot of radial lines, polygon(s), symbols
     or a combination of these centered at the midpoint of the plot
     frame, the lengths, vertices or positions corresponding to the
     numeric magnitudes of the data values. If 'show.centroid' is TRUE,
     an enlarged point at the centroid of values is displayed. The
     centroid is calculated as the average of x and y values unless 
     'rp.type="p"'. In this case, the barycenter of the polygon is
     calculated. Make sure that these suit your purpose, otherwise
     calculate the centroid that you really want and add it with the
     'points' function.

     If the user wants to plot several sets of lines, points or symbols
     by passing matrices or data frames of 'lengths' and 'radial.pos',
     remember that these will be grouped by row, so transpose if the
     data are grouped by columns.

     The size of the labels on the outside of the plot can be adjusted
     by  setting 'par(cex.axis=)' and that of the labels inside by
     setting 'par(cex.lab=)'. If 'radlab' is TRUE, the labels will be
     rotated to a radial alignment. This may help when there are many
     values and labels. If some labels are still crowded, try running
     'label.pos' through the 'spreadout' function.

     The radial.plot family of plots is useful for illustrating cyclic
     data such as wind direction or speed (but see 'oz.windrose' for
     both), activity at different times of the day, and so on. While 
     'radial.plot' actually does the plotting, another function is
     usually  called for specific types of cyclic data. Note that if
     the observations are not taken at equal intervals around the
     circle, the centroid may not mean much.

_V_a_l_u_e:

     nil

_A_u_t_h_o_r(_s):

     Jim Lemon - thanks to Jeremy Claisse and Antonio Hernandez Matias
     for the 'lty' and 'rp.type' suggestions respectively and Patrick
     Baker for the request that led to 'radlab'.

_S_e_e _A_l_s_o:

     'polar.plot','clock24.plot'

_E_x_a_m_p_l_e_s:

      testlen<-rnorm(10)*2+5
      testpos<-seq(0,18*pi/10,length=10)
      testlab<-letters[1:10]
      oldpar<-radial.plot(testlen,testpos,main="Test Radial Lines",line.col="red",lwd=3)
      testlen<-c(sin(seq(0,1.98*pi,length=100))+2+rnorm(100)/10)
      testpos<-seq(0,1.98*pi,length=100)
      radial.plot(testlen,testpos,rp.type="p",main="Test Polygon",line.col="blue")
      # now do a 12 o'clock start with clockwise positive
      radial.plot(testlen,testpos,start=pi/2,clockwise=TRUE,
       rp.type="s",main="Test Symbols (clockwise)",
       point.symbols=16,point.col="green",show.centroid=TRUE)
      # one without the circular grid and multiple polygons
      # see the "diamondplot" function for variation on this
      posmat<-matrix(sample(2:9,30,TRUE),nrow=3)
      radial.plot(posmat,labels=paste("X",1:10,sep=""),rp.type="p",
       main="Spiderweb plot",line.col=2:4,show.grid=FALSE,lwd=1:3,
       radial.lim=c(0,10))
      # dissolved ions in water
      ions<-c(3.2,5,1,3.1,2.1,4.5)
      ion.names<-c("Na","Ca","Mg","Cl","HCO3","SO4")
      radial.plot(ions,labels=ion.names,rp.type="p",main="Dissolved ions in water",
       grid.unit="meq/l",radial.lim=c(0,5),poly.col="yellow")
      par(xpd=oldpar$xpd,mar=oldpar$mar,pty=oldpar$pty)
      # reset the margins
      par(mar=c(5,4,4,2))

