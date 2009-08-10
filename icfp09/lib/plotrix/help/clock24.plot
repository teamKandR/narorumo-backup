clock24.plot             package:plotrix             R Documentation

_P_l_o_t _v_a_l_u_e_s _o_n _a _2_4 _h_o_u_r "_c_l_o_c_k_f_a_c_e".

_D_e_s_c_r_i_p_t_i_o_n:

     'clock24.plot' displays a plot of radial lines, symbols or a
     polygon centered at the midpoint of the plot frame on a 24 hour
     'clockface'.  In contrast to the default behavior of
     'radial.plot', the positions  are interpreted as beginning at
     vertical (000) and moving clockwise.

_U_s_a_g_e:

      clock24.plot(lengths,clock.pos,labels=NULL,label.pos=NULL,rp.type="r",...)

_A_r_g_u_m_e_n_t_s:

 lengths: numeric data vector. Magnitudes will be represented as line
          lengths, or symbol or polygon vertex positions.

clock.pos: numeric vector of positions on the 'clockface'. These must
          be in decimal hours and will be rescaled to radians.

  labels: Labels to place at the circumference.

label.pos: Radial positions of the labels.

 rp.type: Whether to plot radial lines, symbols or a polygon.

     ...: additional arguments are passed to 'radial.plot' and then to
          'plot'.

_V_a_l_u_e:

     nil

_A_u_t_h_o_r(_s):

     Jim Lemon

_S_e_e _A_l_s_o:

     'polar.plot','radial.plot'

_E_x_a_m_p_l_e_s:

      testlen<-rnorm(24)*2+5
      testpos<-0:23+rnorm(24)/4
      clock24.plot(testlen,testpos,main="Test Clock24 (lines)",show.grid=FALSE,
       line.col="green",lwd=3)
      if(dev.interactive()) par(ask=TRUE)
      # now do a 'daylight' plot
      clock24.plot(testlen[7:19],testpos[7:19],
       main="Test Clock24 daytime (symbols)",
       point.col="blue",rp.type="s",lwd=3)
      # reset the margins
      par(mar=c(5,4,4,2))

