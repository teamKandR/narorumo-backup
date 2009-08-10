polar.plot              package:plotrix              R Documentation

_P_l_o_t _v_a_l_u_e_s _o_n _a _c_i_r_c_u_l_a_r _g_r_i_d _o_f _0 _t_o _3_6_0 _d_e_g_r_e_e_s.

_D_e_s_c_r_i_p_t_i_o_n:

     'polar.plot' displays a plot of radial lines, symbols or a polygon
      centered at the midpoint of the plot frame on a 0:360 circle.
     Positions are interpreted as beginning at the right and moving
     counterclockwise unless 'start' specifies another starting point
     or 'clockwise' is TRUE.

_U_s_a_g_e:

      polar.plot(lengths,polar.pos=NULL,labels,label.pos=NULL,
       start=0,clockwise=FALSE,rp.type="r",...)

_A_r_g_u_m_e_n_t_s:

 lengths: numeric data vector. Magnitudes will be represented as the
          radial positions of symbols, line ends or polygon vertices.

polar.pos: numeric vector of positions on a 0:360 degree circle. These
          will be converted to radians when passed to 'radial.plot'.

  labels: text labels to place on the periphery of the circle. This 
          defaults to labels every 20 degrees. For no labels, pass an
          empty string.

label.pos: positions of the peripheral labels in degrees

   start: The position for zero degrees on the plot in degrees.

clockwise: Whether to increase angles clockwise rather than the default
          counterclockwise.

 rp.type: Whether to plot radial lines, symbols or a polygon.

     ...: additional arguments passed to 'radial.plot' and then to
          'plot'.

_V_a_l_u_e:

     nil

_A_u_t_h_o_r(_s):

     Jim Lemon

_S_e_e _A_l_s_o:

     'radial.plot'

_E_x_a_m_p_l_e_s:

      testlen<-c(rnorm(36)*2+5)
      testpos<-seq(0,350,by=10)
      polar.plot(testlen,testpos,main="Test Polar Plot",lwd=3,line.col=4)
      polar.plot(testlen,testpos,main="Test Clockwise Polar Plot",
       start=90,clockwise=TRUE,lwd=3,line.col=4)
      # reset the margins
      par(mar=c(5,4,4,2))

