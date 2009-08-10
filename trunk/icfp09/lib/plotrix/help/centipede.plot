centipede.plot            package:plotrix            R Documentation

_D_i_s_p_l_a_y _a _c_e_n_t_i_p_e_d_e _p_l_o_t

_D_e_s_c_r_i_p_t_i_o_n:

     Displays a centipede plot on the current graphics device.

_U_s_a_g_e:

      centipede.plot(segs,mct="mean",lower.limit="std.error",
       upper.limit=lower.limit,left.labels=NULL,right.labels=NULL,sort.segs=TRUE,
       main="",xlab=NA,vgrid=NA,mar=NA,col=par("fg"),bg="green",...)

_A_r_g_u_m_e_n_t_s:

    segs: a matrix of midpoints and limits calculated by 'get.segs' OR
          a 'dstat' object returned by 'brkdn'.

     mct: The function to use in calculating the midpoint of each
          segment.

lower.limit: The functions to use in calculating the lower limits for
          each subset of the data.

upper.limit: The functions to use in calculating the upper limits.

left.labels: The variable or subset labels to place  at the left margin
          of the plot. Default values are provided.

right.labels: The variable or subset labels to place  at the right
          margin of the plot.

sort.segs: Whether to sort the segments in ascending order.

    main: Optional title for the plot.

    xlab: Optional x axis label for the plot. The default NA displays a
          text label showing the midpoint and limit functions.

   vgrid: Optional vertical line(s) to display on the plot.

     mar: Margin widths for the plot. Defaults to c(4,5,1,4) or 
          c(4,5,3,4) if there is a title.

     col: The color(s) of the limit lines and borders of the midpoint
          markers.

      bg: The color(s) to fill the midpoint markers.

     ...: additional arguments passed to 'plot'.

_D_e_t_a_i_l_s:

     'centipede.plot' displays one or more midpoints and limits as 
     filled circles with horizontal error bars. It places labels on the
     left and right sides of the plot. If these labels are long, it may
     be necessary to pass explicit values to the 'mar' argument to
     leave enough room.

     Similarly, centipede plots typically have a large number of
     subsets, and  it may be necessary to start the graphics device
     with an aspect ratio that will prevent crowding of the labels when
     over 30 segments are displayed.

     The matrix 'segs' may be entered manually or read from a file. The
     first row specifies midpoints, the second and third rows the lower
     and upper limits respectively and the fourth row the number of
     valid observations. If a 'dstat' object is passed as 'segs', the
     function will calculate the lower and upper values according to
     the relevant arguments. This type of plot is also known as a
     caterpillar plot or a league table.

_V_a_l_u_e:

     nil.

_A_u_t_h_o_r(_s):

     Jim Lemon

_S_e_e _A_l_s_o:

     'get.segs'

_E_x_a_m_p_l_e_s:

      testcp<-list("",40)
      for(i in 1:40) testcp[[i]]<-rnorm(sample(1:8,1)*50)
      segs<-get.segs(testcp)
      centipede.plot(segs,main="Test centipede plot",vgrid=0)

