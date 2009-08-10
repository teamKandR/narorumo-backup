fan.plot               package:plotrix               R Documentation

_D_i_s_p_l_a_y _a _f_a_n _p_l_o_t.

_D_e_s_c_r_i_p_t_i_o_n:

     Displays numerical values as the arcs of overlapping sectors.

_U_s_a_g_e:

      fan.plot(x,edges=200,radius=1,col=NULL,align.at=NULL,max.span=NULL,
       labels=NULL,labelpos=NULL,label.radius=1.2,align="left",shrink=0.02,
       main="",ticks=NULL,include.sumx=FALSE,...)

_A_r_g_u_m_e_n_t_s:

       x: Vector of numbers.

   edges: The number of edges with which to draw a circle.

  radius: The radius of the sectors.

     col: The colors with which to fill the sectors.

align.at: Where to align the sectors (see Details).

max.span: The angle of the maximal sector in radians. The default is to
          scale 'x' so that it sums to 2*pi.

  labels: Labels placed around the sector arcs.

labelpos: Optional circumferential positions for the labels.

label.radius: How far away from the sectors the labels will be placed.
          May be a vector with a radius for each label.

   align: Position of the alignment of sectors (see Details).

  shrink: How much to shrink each successive sector in user units.

    main: Optional title for the plot.

   ticks: The number of ticks that would appear if the sectors were on
          a pie chart. Default is no ticks, TRUE gives the number of
          ticks equal to the integer sum of 'x', which is fairly
          sensible if  'x' is a vector of integers.

include.sumx: Whether to include the sum of all 'x' values as the
          largest sector.

     ...: Additional arguments passed to 'polygon'.

_D_e_t_a_i_l_s:

     The fan plot is a variant of the pie chart that places the sectors
     "on top" of each other from the largest to the smallest. By
     default, the largest sector is centered with its circumferential
     arc upwards, giving the plot the appearance of a folding fan.
     Passing a value for 'align.at' will place the point of alignment
     at that angle in  radians. The sectors may be aligned at either
     the left or right edges or in the center. Note that 'align' must
     be one of 'left right' or 'center'. Each successive sector is
     radially "shrunk" by a constant amount so that two equal sectors
     will both be visible.

     In cases where there are several segments with very small
     differences, the labels may be crowded. There is a simple routine
     in the function to spread out crowded labels, but it may not be
     sufficient for severe crowding. By capturing the return value and
     manually altering  the label positions, the crowded labels can be
     separated. This new vector of positions may then be passed as
     'labelpos'.

     The calculation of sectors tries to ensure that they are circular.
     Thus there will be white space if the plotting device dimensions
     are not proportional to the size of the plot. The user must adjust
     the dimensions of the plotting device to get the correct
     appearance.

_V_a_l_u_e:

     The circumferential positions of the labels in radians. These are
     returned in order of decreasing size of the values plotted.

_A_u_t_h_o_r(_s):

     Jim Lemon, Anupam Tyagi

_S_e_e _A_l_s_o:

     'floating.pie'

_E_x_a_m_p_l_e_s:

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

