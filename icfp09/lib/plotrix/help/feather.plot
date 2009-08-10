feather.plot             package:plotrix             R Documentation

_D_i_s_p_l_a_y _v_e_c_t_o_r_s _a_l_o_n_g _a _h_o_r_i_z_o_n_t_a_l _r_e_f_e_r_e_n_c_e _l_i_n_e.

_D_e_s_c_r_i_p_t_i_o_n:

     Displays vectors along a line usually representing time or
     position.

_U_s_a_g_e:

      feather.plot(r,theta,xpos,yref=0,use.arrows=TRUE,
      col.refline="lightgray",fp.type="s",main="",xlab="",ylab="",
      xlabels=NULL,...)

_A_r_g_u_m_e_n_t_s:

       r: radii of vectors

   theta: direction of vectors in radians

    xpos: where to start each vector along the reference line

    yref: vertical position to place the reference line

use.arrows: whether to put arrow heads on the ends of the vectors

col.refline: the color of the reference line

 fp.type: whether to use "standard" coordinates (begin at the right and
          move counterclockwise) or "meteorological" coordinates (begin
          at the top and move clockwise) when interpreting the values
          of 'theta'

    main: the title of the plot

    xlab: the label for the reference line

    ylab: the label for the vertical axis

 xlabels: optional labels for the reference line

     ...: additional arguments passed to 'arrows' or 'segments'

_D_e_t_a_i_l_s:

     This function places vectors of length 'r' and angle 'theta' 
     along a reference line that may represent time or position or some
     other value. The user is responsible for spacing the vectors so
     that they do not overlap if this is desired.

_V_a_l_u_e:

     nil

_A_u_t_h_o_r(_s):

     Jim Lemon, Eduardo Klein

_S_e_e _A_l_s_o:

     'spread.labels'

_E_x_a_m_p_l_e_s:

      feather.plot(0.6+rnorm(8)/5,seq(0,7*pi/4,by=pi/4),1:8,
       main="Standard Coordinates",xlab="Time",ylab="Value")
      if(dev.interactive()) par(ask=TRUE)
      feather.plot(0.6+rnorm(8)/5,seq(0,7*pi/4,by=pi/4),1:8,
       main="Meteorological Coordinates",xlab="Time",ylab="Value",
       fp.type="m",xlabels=TRUE)
      par(ask=FALSE)

